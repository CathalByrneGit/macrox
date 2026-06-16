# --------------------------------------------------------------------------- #
#  select_item() — extract a single metadata field from a PDF                 #
#                                                                              #
#  Complements select_table() for documents like invoices, reports, and forms  #
#  where individual fields (invoice number, date, total) sit outside tables.  #
#  Results accumulate in sess$items (named list) alongside sess$tables.       #
# --------------------------------------------------------------------------- #

#' Extract a single metadata field from a PDF
#'
#' Extracts a single structured value from a PDF using either an LLM or a
#' local GLiNER2 model.  Results are stored in `sess$items[[label]]` and
#' recorded as a step.
#'
#' @param sess A `pdfmacro_session` object.
#' @param label Character label for the item (e.g. `"invoice_number"`).
#' @param prompt Description of the field to extract.  For the LLM backend
#'   this is the full instruction; for GLiNER it is the field description
#'   passed alongside `label` as the entity type.
#' @param cast Optional type to cast the returned string to: `"character"`
#'   (default), `"numeric"`, `"integer"`, or `"date:<fmt>"` e.g.
#'   `"date:%d/%m/%Y"`.
#' @param page Page number.  For `backend = "llm"`, `NULL` sends all page text
#'   and a non-NULL value renders an image.  For `backend = "gliner"`, `NULL`
#'   uses all pages' text and a non-NULL value uses only that page's text
#'   (no image rendering).
#' @param area Named `c(top, left, bottom, right)` in PDF points.  Only used
#'   when `backend = "llm"` and `page` is non-NULL.
#' @param backend Extraction backend: `"llm"` (default, requires ellmer) or
#'   `"gliner"` (local GLiNER2 model, requires reticulate + gliner2 Python
#'   package installed via [setup_gliner()]).
#' @param chat An existing `ellmer` Chat object.  Only used when
#'   `backend = "llm"`.  See [select_table_llm()].
#' @param provider LLM provider (default `"anthropic"`).  Only used when
#'   `backend = "llm"` and `chat` is `NULL`.
#' @param model Model name.  Only used when `backend = "llm"`.
#' @param base_url Base URL for `"openai_compatible"` providers.
#' @param dpi Render resolution when `backend = "llm"` and `page` is set
#'   (default 120).
#' @param gliner_model GLiNER2 model name (HuggingFace repo).  Only used when
#'   `backend = "gliner"`.  Default `"fastino/gliner2-base-v1"` (205M
#'   params); use `"fastino/gliner2-large-v1"` for higher accuracy.
#' @return `sess` invisibly (step is recorded).
#'
#' @examples
#' \dontrun{
#' # LLM backend (default)
#' sess |> select_item("invoice_number",
#'   prompt = "Extract the invoice ID or reference number.")
#'
#' # GLiNER backend — local, no API key needed
#' setup_gliner()
#' sess |> select_item("invoice_number",
#'   prompt  = "Invoice ID or reference number",
#'   backend = "gliner")
#'
#' # GLiNER on a specific page
#' sess |> select_item("total_amount",
#'   prompt  = "Grand total amount payable",
#'   page    = 1L,
#'   cast    = "numeric",
#'   backend = "gliner")
#' }
#' @export
select_item <- function(sess, label, prompt,
                         cast         = NULL,
                         page         = NULL,
                         area         = NULL,
                         backend      = c("llm", "gliner"),
                         chat         = NULL,
                         provider     = "anthropic",
                         model        = NULL,
                         base_url     = NULL,
                         dpi          = 120L,
                         gliner_model = "fastino/gliner2-base-v1") {
  backend <- match.arg(backend)

  # ── GLiNER path ─────────────────────────────────────────────────────────────
  if (backend == "gliner") {
    py        <- .ensure_gliner(gliner_model)
    all_pages <- pdftools::pdf_text(sess$path)
    text      <- if (!is.null(page)) {
      all_pages[[as.integer(page)]]
    } else {
      paste(all_pages, collapse = "\n\n---\n\n")
    }

    cli::cli_inform(c("i" = "Extracting {.val {label}} via GLiNER2..."))
    raw_value <- tryCatch(
      {
        val <- py$gliner_extract_item(text, label, prompt)
        if (is.null(val)) "" else as.character(val)
      },
      error = function(e) cli::cli_abort(
        "GLiNER2 extraction failed for {.val {label}}: {conditionMessage(e)}"
      )
    )

    cast_value <- .cast_item(raw_value, cast, label)

    if (is.null(sess$items)) sess$items <- list()
    sess$items[[label]] <- list(
      value        = cast_value,
      raw          = raw_value,
      cast         = cast %||% "character",
      prompt       = prompt,
      backend      = "gliner",
      gliner_model = gliner_model
    )

    record_step(sess, list(
      step         = "select_item",
      label        = label,
      prompt       = prompt,
      cast         = cast,
      page         = page,
      backend      = "gliner",
      gliner_model = gliner_model
    ))

    cli::cli_inform(c(
      "v" = "Item {.val {label}}: {.strong {as.character(cast_value)}} [GLiNER2]"
    ))
    return(invisible(sess))
  }

  # ── LLM path ────────────────────────────────────────────────────────────────
  .check_ellmer()
  resolved <- .resolve_llm_chat(chat, provider, model, base_url)
  chat_obj <- resolved$chat
  provider <- resolved$provider
  model    <- resolved$model
  base_url <- resolved$base_url

  item_type <- ellmer::type_object(
    value = ellmer::type_string(
      paste0("The extracted value for: ", prompt)
    )
  )

  if (!is.null(page)) {
    cli::cli_inform(c("i" = "Rendering page {page} for item {.val {label}}..."))
    img_path <- .render_page_for_llm(sess$path, page, area, dpi)
    on.exit(unlink(img_path), add = TRUE)
    result <- tryCatch(
      chat_obj$chat_structured(
        ellmer::content_image_file(img_path),
        prompt,
        type = item_type
      ),
      error = function(e) cli::cli_abort(
        "LLM extraction failed for item {.val {label}}: {conditionMessage(e)}"
      )
    )
  } else {
    cli::cli_inform(c("i" = "Extracting item {.val {label}} from document text..."))
    all_text    <- paste(pdftools::pdf_text(sess$path), collapse = "\n\n---\n\n")
    full_prompt <- paste0(
      "The following is the extracted text of a PDF document:\n\n",
      substr(all_text, 1L, 12000L),
      "\n\n---\n\n",
      prompt
    )
    result <- tryCatch(
      chat_obj$chat_structured(full_prompt, type = item_type),
      error = function(e) cli::cli_abort(
        "LLM extraction failed for item {.val {label}}: {conditionMessage(e)}"
      )
    )
  }

  raw_value  <- result$value %||% ""
  cast_value <- .cast_item(raw_value, cast, label)

  if (is.null(sess$items)) sess$items <- list()
  sess$items[[label]] <- list(
    value    = cast_value,
    raw      = raw_value,
    cast     = cast %||% "character",
    prompt   = prompt,
    backend  = "llm",
    provider = provider,
    model    = model
  )

  record_step(sess, list(
    step     = "select_item",
    label    = label,
    prompt   = prompt,
    cast     = cast,
    page     = page,
    area     = area,
    backend  = "llm",
    provider = provider,
    model    = model,
    base_url = base_url,
    dpi      = as.integer(dpi)
  ))

  cli::cli_inform(c(
    "v" = "Item {.val {label}}: {.strong {as.character(cast_value)}} [{provider}]"
  ))
  invisible(sess)
}


.cast_item <- function(raw_value, cast, label) {
  if (!is.null(cast) && nchar(trimws(cast)) > 0L) {
    tryCatch(
      .cast_col(raw_value, cast),
      error = function(e) {
        cli::cli_warn(
          "Cast to {.val {cast}} failed for {.val {label}} — keeping as character."
        )
        raw_value
      }
    )
  } else {
    raw_value
  }
}


#' Update the prompt for a recorded select_item step and re-extract
#'
#' @param sess A `pdfmacro_session` object.
#' @param label Label of the item to update.
#' @param prompt New prompt string.
#' @param cast New cast type, or `NULL` to keep existing.
#' @param chat An existing `ellmer` Chat object to use for the re-extraction.
#'   See [select_table_llm()]. `NULL` (default) reuses the provider/model
#'   recorded for the step.
#' @param re_extract Re-run the LLM call immediately (default `TRUE`).
#' @return `sess` invisibly.
#' @export
update_item <- function(sess, label, prompt = NULL, cast = NULL,
                         chat = NULL, re_extract = TRUE) {
  idx <- rev(which(vapply(sess$steps, function(s)
    isTRUE(s$step == "select_item") && identical(s$label, label),
    logical(1))))[1L]

  if (is.na(idx)) {
    cli::cli_abort("No {.val select_item} step found for {.val {label}}.")
  }

  step <- sess$steps[[idx]]
  if (!is.null(prompt)) step$prompt <- prompt
  if (!is.null(cast))   step$cast   <- cast
  sess$steps[[idx]] <- step

  if (re_extract) {
    old_rep       <- sess$.replaying
    sess$.replaying <- TRUE
    select_item(sess,
      label        = label,
      prompt       = step$prompt,
      cast         = step$cast,
      page         = step$page,
      area         = step$area,
      backend      = step$backend      %||% "llm",
      chat         = chat,
      provider     = step$provider     %||% "anthropic",
      model        = step$model,
      base_url     = step$base_url,
      dpi          = step$dpi          %||% 120L,
      gliner_model = step$gliner_model %||% "fastino/gliner2-base-v1"
    )
    sess$.replaying <- old_rep
    sess$steps[[idx]] <- step   # restore the updated step
    cli::cli_inform(c("v" = "Re-extracted item {.val {label}}."))
  }
  invisible(sess)
}


#' Show all extracted items
#'
#' @param sess A `pdfmacro_session` object.
#' @return `sess` invisibly. # not recorded
#' @export
show_items <- function(sess) {
  if (is.null(sess$items) || length(sess$items) == 0L) {
    cli::cli_inform("No items extracted yet. Use {.fn select_item} first.")
    return(invisible(sess))
  }
  cli::cli_rule(left = paste0("Items (", length(sess$items), ")"))
  for (lbl in names(sess$items)) {
    item <- sess$items[[lbl]]
    cli::cli_bullets(setNames(
      paste0("{.val {lbl}}: {.strong {as.character(item$value)}}  [{item$cast}]"),
      "*"
    ))
  }
  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  export_json()                                                                #
# --------------------------------------------------------------------------- #

#' Export session data as a structured JSON payload
#'
#' Combines extracted items (document metadata) and tables into a single
#' self-describing JSON document. Suitable for API ingestion, database loading,
#' and RAG pipelines.
#'
#' @param sess A `pdfmacro_session` object.
#' @param path File path to write the JSON. `NULL` (default) returns the JSON
#'   string invisibly without writing.
#' @param pretty Logical; pretty-print the JSON (default `TRUE`).
#' @param include_steps Include the recorded step list in the output (default
#'   `FALSE`).
#' @return The JSON string, invisibly.
#' @export
export_json <- function(sess, path = NULL, pretty = TRUE,
                         include_steps = FALSE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg jsonlite} is required for JSON export.",
      "i" = "Install with: {.code install.packages('jsonlite')}"
    ))
  }

  # Convert each table to a list of row-objects
  tables_out <- lapply(sess$tables, function(df) {
    lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
  })

  # Items: simplify to label → value for clean JSON
  items_out <- lapply(sess$items %||% list(), function(item) item$value)

  out <- list(
    source    = basename(sess$path),
    extracted = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    items     = items_out,
    tables    = tables_out
  )

  if (include_steps) {
    out$steps <- lapply(sess$steps, function(s) s[!grepl("^\\.", names(s))])
  }

  json <- jsonlite::toJSON(out, pretty = pretty, auto_unbox = TRUE,
                            na = "null")

  if (!is.null(path)) {
    writeLines(json, path)
    cli::cli_inform(c("v" = "JSON exported to {.path {path}}"))
  }

  invisible(json)
}
