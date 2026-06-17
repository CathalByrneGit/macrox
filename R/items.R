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
#' @param sess A `macrox_session` object.
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
#' @param all_matches Logical; only used when `backend = "gliner"`.  If `TRUE`,
#'   return every occurrence of the entity found in the text as a character
#'   vector instead of just the first match.  Default `FALSE`.
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
                         gliner_model = "fastino/gliner2-base-v1",
                         all_matches  = FALSE) {
  backend <- match.arg(backend)

  # ── GLiNER path ─────────────────────────────────────────────────────────────
  if (backend == "gliner") {
    py        <- .ensure_gliner(gliner_model)
    all_pages <- pdftools::pdf_text(sess$path)
    text      <- if (!is.null(page)) {
      pg <- as.integer(unlist(page))
      pg <- pg[pg >= 1L & pg <= length(all_pages)]
      if (length(pg) == 1L) all_pages[[pg]] else as.list(all_pages[pg])
    } else {
      paste(all_pages, collapse = "\n\n---\n\n")
    }

    cli::cli_inform(c("i" = "Extracting {.val {label}} via GLiNER2..."))
    raw_value <- tryCatch(
      {
        val <- py$gliner_extract_item(text, label, prompt,
                                      all_matches = isTRUE(all_matches))
        if (is.null(val)) {
          if (isTRUE(all_matches)) character(0L) else ""
        } else if (is.list(val)) {
          as.character(unlist(val))
        } else {
          as.character(val)
        }
      },
      error = function(e) cli::cli_abort(
        "GLiNER2 extraction failed for {.val {label}}: {conditionMessage(e)}"
      )
    )

    cast_value <- .cast_item(raw_value, cast, label)

    val_summary <- if (length(cast_value) > 1L) {
      paste0("[", length(cast_value), " matches: ",
             paste(head(as.character(cast_value), 3L), collapse = ", "),
             if (length(cast_value) > 3L) ", …" else "", "]")
    } else {
      as.character(cast_value)
    }

    if (is.null(sess$items)) sess$items <- list()
    sess$items[[label]] <- list(
      value        = cast_value,
      raw          = raw_value,
      cast         = cast %||% "character",
      prompt       = prompt,
      backend      = "gliner",
      gliner_model = gliner_model,
      all_matches  = isTRUE(all_matches)
    )

    record_step(sess, list(
      step         = "select_item",
      label        = label,
      prompt       = prompt,
      cast         = cast,
      page         = page,
      backend      = "gliner",
      gliner_model = gliner_model,
      all_matches  = isTRUE(all_matches)
    ))

    cli::cli_inform(c(
      "v" = "Item {.val {label}}: {.strong {val_summary}} [GLiNER2]"
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


#' Extract multiple metadata fields in one GLiNER model pass
#'
#' Runs a single GLiNER2 inference call to extract several named fields from a
#' PDF, which is significantly faster than calling [select_item()] once per
#' field. Each field is recorded as a separate `select_item` step so macros
#' replay with the standard `select_item` mechanism.
#'
#' GLiNER2 can return multiple occurrences of the same entity type when they
#' are present in the text; `select_items_batch` surfaces the first match per
#' field (the same behaviour as [select_item()]).
#'
#' @param sess A `macrox_session` object.
#' @param items Named character vector: names are field labels, values are
#'   natural-language prompts describing the field.
#'   E.g. `c(invoice_no = "Invoice ID or reference number",
#'            total = "Grand total amount payable")`.
#' @param page Page number to extract from.  `NULL` (default) concatenates all
#'   pages' text.
#' @param cast Named character vector of cast types keyed by label.  Unlisted
#'   fields default to `"character"`.  Supported types: `"character"`,
#'   `"numeric"`, `"integer"`, `"date:<fmt>"` (e.g. `"date:%d/%m/%Y"`).
#' @param gliner_model GLiNER2 model identifier.  Default
#'   `"fastino/gliner2-base-v1"` (205M); use `"fastino/gliner2-large-v1"` for
#'   higher accuracy.
#' @param all_matches If `TRUE`, return every occurrence found per label as a
#'   character vector.  Labels listed in `cast` are still applied element-wise.
#'   Default `FALSE`.
#' @return `sess` invisibly (one `select_item` step recorded per field).
#' @export
select_items_batch <- function(sess, items, page = NULL, cast = NULL,
                                gliner_model = "fastino/gliner2-base-v1",
                                all_matches  = FALSE) {
  if (!is.character(items) || is.null(names(items)) ||
      any(!nzchar(names(items)))) {
    cli::cli_abort(c(
      "{.arg items} must be a named character vector.",
      "i" = 'E.g. {.code c(invoice_no = "Invoice ID", total = "Grand total")}'
    ))
  }
  if (is.null(cast)) cast <- character(0)

  py       <- .ensure_gliner(gliner_model)
  all_text <- pdftools::pdf_text(sess$path)
  text     <- if (!is.null(page)) {
    pg <- as.integer(unlist(page))
    pg <- pg[pg >= 1L & pg <= length(all_text)]
    if (length(pg) == 1L) all_text[[pg]] else as.list(all_text[pg])
  } else {
    paste(all_text, collapse = "\n\n---\n\n")
  }

  cli::cli_inform(c(
    "i" = "Extracting {length(items)} field{?s} via GLiNER2 in one pass..."
  ))

  raw <- tryCatch(
    reticulate::py_to_r(
      py$gliner_batch_extract(text, as.list(items),
                              all_matches = isTRUE(all_matches))
    ),
    error = function(e) cli::cli_abort(
      "GLiNER2 batch extraction failed: {conditionMessage(e)}"
    )
  )

  if (is.null(sess$items)) sess$items <- list()

  for (lbl in names(items)) {
    prompt    <- items[[lbl]]
    cast_type <- if (lbl %in% names(cast)) cast[[lbl]] else "character"
    raw_val   <- if (isTRUE(all_matches)) {
      as.character(unlist(raw[[lbl]] %||% list()))
    } else {
      raw[[lbl]] %||% ""
    }
    cast_val  <- .cast_item(raw_val, cast_type, lbl)

    val_summary <- if (length(cast_val) > 1L) {
      paste0("[", length(cast_val), " matches: ",
             paste(head(as.character(cast_val), 3L), collapse = ", "),
             if (length(cast_val) > 3L) ", …" else "", "]")
    } else {
      as.character(cast_val)
    }

    sess$items[[lbl]] <- list(
      value        = cast_val,
      raw          = raw_val,
      cast         = cast_type,
      prompt       = prompt,
      backend      = "gliner",
      gliner_model = gliner_model,
      all_matches  = isTRUE(all_matches)
    )

    record_step(sess, list(
      step         = "select_item",
      label        = lbl,
      prompt       = prompt,
      cast         = cast_type,
      page         = page,
      backend      = "gliner",
      gliner_model = gliner_model,
      all_matches  = isTRUE(all_matches)
    ))

    cli::cli_inform(c(
      "v" = "  {.val {lbl}}: {.strong {val_summary}}"
    ))
  }

  invisible(sess)
}


#' Extract structured records from a PDF page using GLiNER2
#'
#' Uses GLiNER2's `extract_json` to extract a structured entity schema
#' (multiple fields, optional list fields, multiple records) from a PDF page.
#' Results are stored as a data frame in `sess$tables[[label]]`, identical to
#' tabulapdf / LLM table extraction — downstream steps and exports work
#' unchanged.
#'
#' @param sess A `macrox_session` object.
#' @param label Table label to store results under.
#' @param entity Entity type name used in the GLiNER2 schema. Defaults to
#'   `label`.
#' @param fields Named character vector: `field_name = "description"`. Every
#'   name becomes a column in the resulting data frame.
#' @param list_fields Character vector of field names that may contain multiple
#'   values (GLiNER2 `list` type). Multiple values are collapsed with `"; "`.
#'   Default `character(0)`.
#' @param enum_fields Named character vector of enumerated field types:
#'   `field_name = "[val1|val2|val3]"`. Restricts what the model extracts to the
#'   listed choices. Default `character(0)`.
#' @param page Integer or integer vector of pages to extract from. Multiple
#'   pages use a single `batch_extract_json` call. Default `1L`.
#' @param gliner_model GLiNER2 model identifier.
#'   Default `"fastino/gliner2-base-v1"`.
#' @return `sess` invisibly (step recorded).
#' @export
select_struct <- function(sess, label, entity = label,
                           fields,
                           list_fields  = character(0),
                           enum_fields  = character(0),
                           page         = 1L,
                           gliner_model = "fastino/gliner2-base-v1") {
  stopifnot(
    is.character(fields), length(fields) >= 1L,
    !is.null(names(fields)), all(nzchar(names(fields)))
  )

  # Build GLiNER2 schema strings: "name::type::description"
  # enum:  "name::[v1|v2]::str::description"  (4-part)
  # list:  "name::list::description"           (3-part)
  # str:   "name::str::description"            (3-part)
  specs <- vapply(names(fields), function(nm) {
    if (nm %in% names(enum_fields)) {
      choices <- trimws(enum_fields[[nm]])
      paste0(nm, "::", choices, "::str::", fields[[nm]])
    } else if (nm %in% list_fields) {
      paste0(nm, "::list::", fields[[nm]])
    } else {
      paste0(nm, "::str::", fields[[nm]])
    }
  }, character(1))

  py        <- .ensure_gliner(gliner_model)
  all_pages <- pdftools::pdf_text(sess$path)
  pg        <- as.integer(unlist(page))
  pg        <- pg[pg >= 1L & pg <= length(all_pages)]
  text      <- if (length(pg) == 1L) all_pages[[pg]] else as.list(all_pages[pg])

  cli::cli_inform(c(
    "i" = "Extracting struct {.val {entity}} ({length(fields)} field{?s}) via GLiNER2..."
  ))

  raw_records <- tryCatch(
    reticulate::py_to_r(
      py$gliner_extract_struct(
        text        = text,
        entity      = entity,
        field_specs = as.list(specs)
      )
    ),
    error = function(e) cli::cli_abort(
      "GLiNER2 struct extraction failed: {conditionMessage(e)}"
    )
  )

  col_names <- names(fields)

  if (is.null(sess$structs)) sess$structs <- list()
  sess$structs[[label]] <- list(
    records  = raw_records,
    col_names = col_names,
    entity   = entity,
    specs    = specs
  )

  record_step(sess, list(
    step         = "select_struct",
    label        = label,
    entity       = entity,
    fields       = as.list(fields),
    list_fields  = as.character(list_fields),
    enum_fields  = as.list(enum_fields),
    page         = pg,
    gliner_model = gliner_model
  ))

  n <- length(raw_records)
  cli::cli_inform(c(
    "v" = "Struct {.val {label}}: {n} record{?s} ({length(col_names)} field{?s}). Run {.fn struct_to_df} to convert."
  ))
  invisible(sess)
}


#' Convert raw struct extraction to a data frame
#'
#' Takes the raw GLiNER2 records stored by [select_struct()] (which include
#' per-field confidence scores) and converts them to a data frame stored in
#' `sess$tables[[label]]`. Optionally filters out low-confidence records.
#'
#' @param sess A `macrox_session` object.
#' @param label Label matching a prior [select_struct()] call.
#' @param min_confidence Numeric in `[0, 1]`. Records whose mean field
#'   confidence is below this threshold are dropped. `NULL` (default) keeps all.
#' @return `sess` invisibly (step recorded).
#' @export
struct_to_df <- function(sess, label, min_confidence = NULL) {
  raw_obj <- sess$structs[[label]]
  if (is.null(raw_obj)) {
    cli::cli_abort(
      "No struct data for {.val {label}}. Run {.fn select_struct} first."
    )
  }

  records   <- raw_obj$records
  col_names <- raw_obj$col_names

  # GLiNER2 returns each field as a list of {text:..., confidence:...} dicts.
  # Multi-value fields (length > 1) are expanded into separate rows (parallel).
  # Fields with fewer values than the row count are padded with NA.

  .parse_field <- function(v) {
    if (is.null(v) || identical(v, FALSE) || (is.list(v) && length(v) == 0L))
      return(list(vals = NA_character_, confs = NA_real_))
    if (!is.list(v))
      return(list(vals = as.character(v), confs = NA_real_))
    if (!is.null(v[[1L]][["text"]])) {
      list(
        vals  = vapply(v, function(i) as.character(i[["text"]]  %||% NA_character_), character(1)),
        confs = vapply(v, function(i) as.numeric(i[["confidence"]] %||% NA_real_),   numeric(1))
      )
    } else {
      list(vals = paste(as.character(unlist(v)), collapse = "; "), confs = NA_real_)
    }
  }

  # Expand one GLiNER2 record into ≥1 data frame rows
  .expand_record <- function(rec) {
    parsed <- lapply(col_names, function(fn) .parse_field(rec[[fn]]))
    n      <- max(vapply(parsed, function(p) length(p$vals), integer(1)))
    lapply(seq_len(n), function(i) {
      vals  <- lapply(parsed, function(p) if (i <= length(p$vals))  p$vals[[i]]  else NA_character_)
      confs <- lapply(parsed, function(p) if (i <= length(p$confs)) p$confs[[i]] else NA_real_)
      as.data.frame(setNames(c(vals, confs), all_names), stringsAsFactors = FALSE)
    })
  }

  conf_names <- paste0(col_names, "_conf")
  all_names  <- c(col_names, conf_names)

  if (length(records) == 0L) {
    df <- as.data.frame(
      setNames(replicate(length(all_names), character(0), simplify = FALSE),
               all_names),
      stringsAsFactors = FALSE
    )
  } else {
    rows <- unlist(lapply(records, .expand_record), recursive = FALSE)

    # Filter by per-row mean confidence after expansion
    if (!is.null(min_confidence)) {
      conf_cols <- paste0(col_names, "_conf")
      keep <- vapply(rows, function(r) {
        mc <- mean(as.numeric(unlist(r[conf_cols])), na.rm = TRUE)
        if (is.nan(mc)) TRUE else mc >= min_confidence
      }, logical(1))
      rows <- rows[keep]
    }

    df <- if (length(rows) == 0L)
      as.data.frame(
        setNames(replicate(length(all_names), character(0), simplify = FALSE), all_names),
        stringsAsFactors = FALSE)
    else
      do.call(rbind, rows)
  }

  if (is.null(sess$tables)) sess$tables <- list()
  sess$tables[[label]] <- df

  record_step(sess, list(
    step           = "struct_to_df",
    label          = label,
    min_confidence = min_confidence
  ))

  cli::cli_inform(c(
    "v" = "Converted {.val {label}} to table: {nrow(df)} row{?s} × {ncol(df)} col{?s}."
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
#' @param sess A `macrox_session` object.
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
      gliner_model = step$gliner_model %||% "fastino/gliner2-base-v1",
      all_matches  = isTRUE(step$all_matches)
    )
    sess$.replaying <- old_rep
    sess$steps[[idx]] <- step   # restore the updated step
    cli::cli_inform(c("v" = "Re-extracted item {.val {label}}."))
  }
  invisible(sess)
}


#' Show all extracted items
#'
#' @param sess A `macrox_session` object.
#' @return `sess` invisibly. # not recorded
#' @export
show_items <- function(sess) {
  if (is.null(sess$items) || length(sess$items) == 0L) {
    cli::cli_inform("No items extracted yet. Use {.fn select_item} first.")
    return(invisible(sess))
  }
  cli::cli_rule(left = paste0("Items (", length(sess$items), ")"))
  for (lbl in names(sess$items)) {
    item    <- sess$items[[lbl]]
    val_str <- if (length(item$value) > 1L) {
      paste0("[", paste(as.character(item$value), collapse = ", "), "]")
    } else {
      as.character(item$value)
    }
    cli::cli_bullets(setNames(
      paste0("{.val {lbl}}: {.strong {val_str}}  [{item$cast}]"),
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
#' @param sess A `macrox_session` object.
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
