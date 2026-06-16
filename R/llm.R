# --------------------------------------------------------------------------- #
#  LLM table extraction via ellmer                                             #
#                                                                              #
#  select_table_llm() — extract a table by sending a page image to an LLM     #
#  update_llm_schema() — revise the schema and re-extract without re-browsing  #
# --------------------------------------------------------------------------- #

#' Extract a table using an LLM
#'
#' Renders the PDF page (optionally cropped to `area`), sends it to an LLM
#' with a structured schema, and returns a clean data frame.  Good for
#' multi-level headers, tables embedded in mixed-content pages, and any
#' situation where positional extraction fails.
#'
#' Requires the `ellmer` package (`install.packages("ellmer")`) and an API
#' key for the chosen provider set as an environment variable
#' (`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `GEMINI_API_KEY`).
#'
#' @param sess A `pdfmacro_session` object.
#' @param label Character label for the extracted table.
#' @param page Page number (integer).
#' @param area Named numeric vector `c(top, left, bottom, right)` in PDF
#'   points.  When supplied the image sent to the LLM is cropped to that
#'   region, which improves accuracy on pages with multiple tables.
#' @param chat An existing `ellmer` Chat object, e.g.
#'   `ellmer::chat_anthropic()`, `ellmer::chat_openrouter()`, or
#'   `ellmer::chat_openai_compatible(base_url = "http://localhost:11434/v1",
#'   model = "llama2")`.  The chat is cloned before use (its history is left
#'   untouched).  When supplied, `provider`, `model`, and `base_url` are
#'   ignored for the call but recorded (derived from the chat object) so the
#'   step can be replayed.
#' @param provider Name of an `ellmer` chat constructor, without the `chat_`
#'   prefix, e.g. `"anthropic"` (default), `"openai"`, `"google_gemini"`,
#'   `"openrouter"`, `"groq"`, `"ollama"`, or `"openai_compatible"`. Any
#'   provider for which `ellmer` exports `chat_<provider>()` is supported.
#'   Ignored when `chat` is supplied.
#' @param model Model name.  `NULL` uses a sensible default for `"anthropic"`,
#'   `"openai"`, and `"google_gemini"`; other providers require `model` to be
#'   set explicitly.  Ignored when `chat` is supplied.
#' @param schema Named character vector mapping column names to R types:
#'   `c(Month = "character", Male = "integer", Total = "integer")`.
#'   Supported types: `"character"`, `"integer"`, `"numeric"`.
#'   `NULL` (default) asks the LLM to auto-detect the columns.
#' @param prompt Optional extra instructions appended to the base prompt,
#'   e.g. `"Ignore the footnote row at the bottom."`.
#' @param dpi Render resolution for the page image (default 150).
#' @param header_rows Number of header rows.  When > 1 the prompt asks the
#'   LLM to flatten them with `_` separators.
#' @return `sess` invisibly (step is recorded).
#' @export
select_table_llm <- function(sess, label,
                              page,
                              area        = NULL,
                              chat        = NULL,
                              provider    = "anthropic",
                              model       = NULL,
                              base_url    = NULL,
                              schema      = NULL,
                              prompt      = NULL,
                              dpi         = 150L,
                              header_rows = 1L) {
  .check_ellmer()
  resolved <- .resolve_llm_chat(chat, provider, model, base_url)
  chat_obj <- resolved$chat
  provider <- resolved$provider
  model    <- resolved$model
  base_url <- resolved$base_url

  # ── Render page → temp PNG (cropped if area supplied) ─────────────────────
  cli::cli_inform(c("i" = "Rendering page {page} [{provider} / {model}]..."))
  img_path <- .render_page_for_llm(sess$path, page, area, dpi)
  on.exit(unlink(img_path), add = TRUE)

  # ── Build prompt ───────────────────────────────────────────────────────────
  base_prompt <- paste0(
    "Extract the table from this image as structured data. ",
    "Include every data row exactly as shown. Do not add or omit rows. ",
    if (header_rows > 1L)
      paste0("The table has ", header_rows,
             " header rows — combine them into a single column name joined by an underscore. ")
    else "",
    if (!is.null(schema))
      paste0("Columns: ", paste(names(schema), collapse = ", "), ". ")
    else
      "Identify the column names from the table header. ",
    if (!is.null(prompt)) prompt else ""
  )

  # ── Build ellmer type spec ─────────────────────────────────────────────────
  llm_type <- .build_llm_type(schema)

  # ── Call LLM ──────────────────────────────────────────────────────────────
  cli::cli_inform(c("i" = "Calling LLM..."))
  result <- tryCatch(
    chat_obj$chat_structured(
      ellmer::content_image_file(img_path),
      base_prompt,
      type = llm_type
    ),
    error = function(e) cli::cli_abort(
      "LLM extraction failed: {conditionMessage(e)}"
    )
  )

  # ── Convert result → data frame ────────────────────────────────────────────
  df <- .llm_result_to_df(result, schema)

  if (nrow(df) == 0L) {
    cli::cli_abort("LLM returned an empty table. Try a tighter area or a more explicit prompt.")
  }

  set_table(sess, label, df)

  record_step(sess, list(
    step        = "select_table_llm",
    label       = label,
    page        = page,
    area        = area,
    provider    = provider,
    model       = model,
    base_url    = base_url,
    schema      = if (!is.null(schema)) as.list(schema) else NULL,
    prompt      = prompt,
    dpi         = as.integer(dpi),
    header_rows = as.integer(header_rows)
  ))

  cli::cli_inform(c(
    "v" = "Table {.val {label}} extracted via LLM [{provider}]: {nrow(df)} x {ncol(df)}"
  ))
  invisible(sess)
}


#' Update the schema for a recorded LLM extraction step and re-extract
#'
#' Finds the most recent `select_table_llm` step for `label`, replaces its
#' schema, and re-runs the extraction against the same page/area.  Useful
#' when the auto-detected columns need renaming or the types need adjustment.
#'
#' @param sess A `pdfmacro_session` object.
#' @param label Label of the table to update.
#' @param schema New named character vector: `c(Month = "character", Male = "integer")`.
#' @param prompt Replacement prompt, or `NULL` to keep the existing one.
#' @param chat An existing `ellmer` Chat object to use for the re-extraction.
#'   See [select_table_llm()]. `NULL` (default) reuses the provider/model
#'   recorded for the step.
#' @param re_extract Re-run the LLM call immediately (default `TRUE`).
#' @return `sess` invisibly.
#' @export
update_llm_schema <- function(sess, label, schema,
                               prompt      = NULL,
                               chat        = NULL,
                               re_extract  = TRUE) {
  # Find the most recent select_table_llm step for this label
  idx <- rev(which(vapply(sess$steps, function(s)
    isTRUE(s$step == "select_table_llm") && identical(s$label, label),
    logical(1))))[1L]

  if (is.na(idx)) {
    cli::cli_abort(c(
      "No {.val select_table_llm} step found for label {.val {label}}.",
      "i" = "Run {.fn select_table_llm} first."
    ))
  }

  old_step <- sess$steps[[idx]]
  old_step$schema <- as.list(schema)
  if (!is.null(prompt)) old_step$prompt <- prompt
  sess$steps[[idx]] <- old_step

  cli::cli_inform(c("v" = "Schema updated for {.val {label}}."))

  if (re_extract) {
    # Re-run silently (don't double-record)
    old_replaying    <- sess$.replaying
    sess$.replaying  <- TRUE
    select_table_llm(sess,
      label       = label,
      page        = old_step$page,
      area        = old_step$area,
      chat        = chat,
      provider    = old_step$provider %||% "anthropic",
      model       = old_step$model,
      base_url    = old_step$base_url,
      schema      = schema,
      prompt      = old_step$prompt,
      dpi         = old_step$dpi    %||% 150L,
      header_rows = old_step$header_rows %||% 1L
    )
    sess$.replaying <- old_replaying
    # Put the updated step back (re-extract recorded its own clean step)
    sess$steps[[idx]] <- old_step
    cli::cli_inform(c("v" = "Re-extracted {.val {label}} with new schema."))
  }

  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  Internal helpers                                                            #
# --------------------------------------------------------------------------- #

.check_ellmer <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ellmer} is required for LLM extraction.",
      "i" = "Install with: {.code install.packages('ellmer')}"
    ))
  }
}

.llm_default_model <- function(provider) {
  default <- switch(provider,
    anthropic     = "claude-opus-4-5-20251001",
    openai        = "gpt-4o",
    google_gemini = "gemini-2.0-flash",
    NULL
  )
  if (is.null(default)) {
    cli::cli_abort(c(
      "No default model for provider {.val {provider}}.",
      "i" = "Specify {.arg model} explicitly."
    ))
  }
  default
}

# Dispatch to any ellmer::chat_<provider>() constructor.
.make_llm_chat <- function(provider, model, base_url = NULL) {
  fn_name <- paste0("chat_", provider)

  if (!fn_name %in% getNamespaceExports("ellmer")) {
    available <- sort(sub("^chat_", "", grep("^chat_", getNamespaceExports("ellmer"), value = TRUE)))
    cli::cli_abort(c(
      "Unknown LLM provider {.val {provider}}.",
      "i" = "{.arg provider} must match an {.pkg ellmer} chat constructor: {.code chat_<provider>()}.",
      "i" = "Available providers: {.val {available}}"
    ))
  }

  chat_fn <- getExportedValue("ellmer", fn_name)
  fn_args <- names(formals(chat_fn))

  if (!is.null(base_url) && !"base_url" %in% fn_args) {
    cli::cli_abort("Provider {.val {provider}} does not accept {.arg base_url}.")
  }
  if (provider == "openai_compatible" && (is.null(base_url) || nchar(trimws(base_url)) == 0)) {
    cli::cli_abort(c(
      "{.arg base_url} is required for {.val openai_compatible}.",
      "i" = "e.g. {.code base_url = 'http://localhost:1234/v1'}"
    ))
  }

  args <- list(model = model)
  if (!is.null(base_url)) args$base_url <- base_url

  do.call(chat_fn, args)
}

# Map an ellmer Provider S7 class to the slug used by chat_<slug>().
.provider_slug_map <- c(
  ProviderAnthropic        = "anthropic",
  ProviderOpenAI           = "openai",
  ProviderOpenAICompatible = "openai_compatible",
  ProviderGoogleGemini     = "google_gemini",
  ProviderGoogleVertex     = "google_vertex",
  ProviderOpenRouter       = "openrouter",
  ProviderGroq             = "groq",
  ProviderOllama           = "ollama",
  ProviderMistral          = "mistral",
  ProviderDeepSeek         = "deepseek",
  ProviderAzureOpenAI      = "azure_openai",
  ProviderAWSBedrock       = "aws_bedrock",
  ProviderDatabricks       = "databricks",
  ProviderSnowflake        = "snowflake",
  ProviderPerplexity       = "perplexity",
  ProviderHuggingFace      = "huggingface",
  ProviderGithub           = "github",
  ProviderPortkey          = "portkey",
  ProviderVLLM             = "vllm",
  ProviderCloudflare       = "cloudflare",
  ProviderLMStudio         = "lmstudio"
)

# Derive provider slug / model / base_url from an ellmer Chat object.
.chat_provider_info <- function(chat) {
  provider_obj <- chat$get_provider()
  cls          <- sub("^ellmer::", "", class(provider_obj)[[1]])
  slug         <- .provider_slug_map[[cls]]
  if (is.null(slug)) slug <- tolower(sub("^Provider", "", cls))

  base_url <- tryCatch(provider_obj@base_url, error = function(e) NULL)

  list(provider = slug, model = chat$get_model(), base_url = base_url)
}

# Resolve a Chat object + its provider/model/base_url, either from a
# user-supplied ellmer Chat or by constructing one from provider/model/base_url.
.resolve_llm_chat <- function(chat, provider, model, base_url) {
  if (!is.null(chat)) {
    if (!inherits(chat, "Chat")) {
      cli::cli_abort(c(
        "{.arg chat} must be an {.pkg ellmer} Chat object.",
        "i" = "e.g. {.code ellmer::chat_anthropic()} or {.code ellmer::chat_openai_compatible(...)}"
      ))
    }
    info <- .chat_provider_info(chat)
    return(list(
      chat     = chat$clone(),
      provider = info$provider,
      model    = info$model,
      base_url = info$base_url
    ))
  }

  model <- model %||% .llm_default_model(provider)
  list(
    chat     = .make_llm_chat(provider, model, base_url),
    provider = provider,
    model    = model,
    base_url = base_url
  )
}

# Render a PDF page to a temp PNG, optionally cropped to area (pts)
.render_page_for_llm <- function(path, page, area = NULL, dpi = 150L) {
  raw_png <- pdftools::pdf_render_page(path, page = page,
                                        dpi = as.integer(dpi),
                                        numeric = FALSE)
  tmp <- tempfile(fileext = ".png")

  if (!requireNamespace("magick", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg magick} is required to render PDF pages for PaddleOCR/LLM extraction.",
      "i" = "Install with: {.code install.packages('magick')}"
    ))
  }

  img <- magick::image_read(raw_png)

  if (!is.null(area)) {
    pg_sz <- tryCatch(pdftools::pdf_pagesize(path)[page, ], error = function(e) NULL)
    if (!is.null(pg_sz)) {
      info <- magick::image_info(img)
      sx   <- info$width  / pg_sz$width
      sy   <- info$height / pg_sz$height
      l    <- round(area[["left"]]   * sx)
      t    <- round(area[["top"]]    * sy)
      w    <- round((area[["right"]]  - area[["left"]])  * sx)
      h    <- round((area[["bottom"]] - area[["top"]])   * sy)
      img  <- magick::image_crop(img, paste0(w, "x", h, "+", l, "+", t))
    }
  }

  magick::image_write(img, path = tmp, format = "png")
  tmp
}

# Build ellmer type spec from schema (or flexible auto spec)
.build_llm_type <- function(schema = NULL) {
  if (!is.null(schema)) {
    # Known columns — each cell is a string; cast_types handles conversion
    col_types <- lapply(names(schema), function(nm)
      ellmer::type_string(paste("Cell value for column", nm)))
    names(col_types) <- names(schema)
    row_type <- do.call(ellmer::type_object, col_types)

    ellmer::type_object(
      rows = ellmer::type_array(
        items       = row_type,
        description = "All data rows from the table, one object per row"
      )
    )
  } else {
    # Auto-detect: LLM returns parallel headers + rows arrays
    ellmer::type_object(
      headers = ellmer::type_array(
        items       = ellmer::type_string(),
        description = "Column header names in left-to-right order"
      ),
      rows = ellmer::type_array(
        items       = ellmer::type_array(items = ellmer::type_string()),
        description = "Data rows; each inner array matches the headers order"
      )
    )
  }
}

# Convert ellmer structured result → data frame
.llm_result_to_df <- function(result, schema = NULL) {
  if (!is.null(schema)) {
    # result$rows is already a data frame (array of objects → df)
    df <- result$rows
    if (!is.data.frame(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
    # Ensure column names match schema
    if (!identical(sort(names(df)), sort(names(schema)))) {
      # Try to align
      common <- intersect(names(schema), names(df))
      df     <- df[, common, drop = FALSE]
    }
    rownames(df) <- NULL
    return(df)
  }

  # Auto mode: stitch headers + rows
  headers <- unlist(result$headers)
  rows    <- result$rows   # list of character vectors

  if (length(headers) == 0L || length(rows) == 0L) return(data.frame())

  mat <- do.call(rbind, lapply(rows, function(r) {
    v <- unlist(r)
    length(v) <- length(headers)  # pad / trim to match
    v
  }))

  df        <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- make.names(headers, unique = TRUE)
  rownames(df) <- NULL
  df
}

# Parse schema from a text area: "Month: character\nMale: integer\n..."
# Also accepts bare column names (type defaults to character)
.parse_schema_text <- function(txt) {
  if (is.null(txt) || nchar(trimws(txt)) == 0L) return(NULL)
  lines <- trimws(strsplit(txt, "\n")[[1]])
  lines <- lines[nchar(lines) > 0L]
  if (length(lines) == 0L) return(NULL)

  nms  <- vapply(lines, function(l) trimws(strsplit(l, ":")[[1]][[1]]), character(1))
  typs <- vapply(lines, function(l) {
    parts <- strsplit(l, ":")[[1]]
    if (length(parts) >= 2L) trimws(parts[[2L]]) else "character"
  }, character(1))

  setNames(typs, nms)
}
