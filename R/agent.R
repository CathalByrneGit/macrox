#' Profile a PDF for agent consumption
#'
#' Returns a structured machine-readable description of the PDF's tables and
#' surrounding text context. Pass the result to an LLM to generate a macro.
#'
#' @param path Path to a PDF file.
#' @param pages Integer vector of pages to scan. Defaults to all pages (capped
#'   at 60).
#' @param method Extraction method: `"lattice"` (default) or `"stream"`.
#' @param context_lines Number of text lines above/below each table's header
#'   row to include as context (default 3).
#' @return A `pdfmacro_profile` list.
#' @export
pdf_profile <- function(path, pages = NULL, method = "lattice", context_lines = 3) {
  info    <- pdftools::pdf_info(path)
  n_pages <- info$pages

  if (is.null(pages)) pages <- seq_len(min(n_pages, 60L))

  all_text <- pdftools::pdf_text(path)

  page_profiles <- lapply(pages, function(pg) {
    tbls <- tryCatch(
      if (!requireNamespace("tabulapdf", quietly=TRUE)) { tbls <- list() } else
      lapply(tabulapdf::extract_tables(file, pages = pg,
                                       method = ...,
                                       guess = TRUE, output = "matrix"), .matrix_to_df),
      error = function(e) list()
    )

    page_text  <- all_text[[pg]]
    text_lines <- strsplit(page_text, "\n")[[1]]
    text_lines <- trimws(text_lines)

    table_list <- lapply(seq_along(tbls), function(j) {
      df   <- .matrix_to_df(tbls[[j]])
      cols <- names(df)

      # Find text context near column header
      first_col <- if (length(cols) > 0) tolower(cols[[1]]) else ""
      ctx_idx   <- which(
        grepl(first_col, tolower(text_lines), fixed = TRUE)
      )
      if (length(ctx_idx) == 0) {
        context <- character(0)
      } else {
        ci      <- ctx_idx[[1]]
        lo      <- max(1L, ci - context_lines)
        hi      <- min(length(text_lines), ci + context_lines)
        context <- text_lines[lo:hi]
      }

      sample_rows <- lapply(seq_len(min(3L, nrow(df))), function(r) {
        as.list(df[r, , drop = FALSE])
      })

      list(
        index   = j,
        nrow    = nrow(df),
        ncol    = ncol(df),
        columns = cols,
        sample  = sample_rows,
        context = context
      )
    })

    list(
      page     = pg,
      n_tables = length(tbls),
      tables   = table_list
    )
  })

  names(page_profiles) <- as.character(pages)

  out <- structure(
    list(
      file     = basename(path),
      n_pages  = n_pages,
      pages    = page_profiles,
      all_text = all_text
    ),
    class = "pdfmacro_profile"
  )
  out
}

#' @export
print.pdfmacro_profile <- function(x, ...) {
  cat("pdfmacro_profile\n")
  cat("File:   ", x$file, "\n")
  cat("Pages:  ", x$n_pages, "total,", length(x$pages), "profiled\n")
  n_tbls <- sum(vapply(x$pages, function(p) p$n_tables, integer(1)))
  cat("Tables: ", n_tbls, "found\n")
  invisible(x)
}


# --------------------------------------------------------------------------- #
#  validate_macro()                                                             #
# --------------------------------------------------------------------------- #

#' Validate a macro against a PDF without extracting data
#'
#' Runs a pre-flight check on every step. Agents should call this after
#' generating a macro and fix any errors before the human runs [pdf_replay()].
#'
#' @param file Path to a PDF file.
#' @param macro Either a step list or a path to a `.yml` file.
#' @return A `pdfmacro_validation` list with `$valid`, `$steps` (data frame),
#'   and `$errors` (character vector).
#' @export
validate_macro <- function(file, macro) {
  if (is.character(macro) && length(macro) == 1 && grepl("\\.yml$", macro)) {
    steps <- load_macro(macro)
  } else if (is.character(macro) && length(macro) == 1) {
    steps <- load_macro(macro)
  } else {
    steps <- macro
  }

  info      <- pdftools::pdf_info(file)
  n_pages   <- info$pages
  errors    <- character(0)
  step_rows <- vector("list", length(steps))
  extracted_labels <- character(0)

  for (i in seq_along(steps)) {
    s      <- steps[[i]]
    status <- "ok"
    msg    <- ""

    if (s$step == "select_table") {
      if (!is.null(s$label_match)) {
        # Fuzzy — can't easily validate without running it; mark as ok
        msg <- "fuzzy match, skipped page check"
      } else if (!is.null(s$page)) {
        pg <- s$page
        if (pg < 1 || pg > n_pages) {
          status <- "error"
          msg    <- paste0("page ", pg, " out of range (1-", n_pages, ")")
          errors <- c(errors, paste0("[", i, "] ", msg))
        } else {
          # Light check without area
          if (is.null(s$area)) {
            tbls <- tryCatch(
              lapply(tabulapdf::extract_tables(file, pages = pg,
                                               method = ...,
                                               guess = TRUE, output = "matrix"), .matrix_to_df),
              error = function(e) list()
            )
            ti <- s$table_index %||% 1L
            if (ti > length(tbls)) {
              status <- "error"
              msg    <- paste0("table_index ", ti, " but only ", length(tbls),
                               " table(s) on page ", pg)
              errors <- c(errors, paste0("[", i, "] ", msg))
            }
          }
        }
      } else {
        status <- "error"
        msg    <- "no page or label_match"
        errors <- c(errors, paste0("[", i, "] ", msg))
      }
      if (status == "ok") extracted_labels <- c(extracted_labels, s$label)

    } else if (s$step %in% c("rename_columns", "cast_types", "filter_rows")) {
      tbl <- s$table
      if (is.null(tbl) || nchar(tbl) == 0) {
        status <- "error"
        msg    <- "missing $table field"
        errors <- c(errors, paste0("[", i, "] ", msg))
      } else if (!(tbl %in% extracted_labels)) {
        status <- "error"
        msg    <- paste0("table '", tbl, "' not yet extracted at this step")
        errors <- c(errors, paste0("[", i, "] ", msg))
      }
      if (s$step == "rename_columns" && (is.null(s$mapping) || length(s$mapping) == 0)) {
        status <- "error"; msg <- "empty mapping"
        errors <- c(errors, paste0("[", i, "] ", msg))
      }
      if (s$step == "cast_types" && (is.null(s$types) || length(s$types) == 0)) {
        status <- "error"; msg <- "empty types"
        errors <- c(errors, paste0("[", i, "] ", msg))
      }
      if (s$step == "filter_rows" && (is.null(s$exclude_where) || nchar(s$exclude_where) == 0)) {
        status <- "error"; msg <- "empty exclude_where"
        errors <- c(errors, paste0("[", i, "] ", msg))
      }

    } else {
      status <- "error"
      msg    <- paste0("unknown step type: '", s$step, "'")
      errors <- c(errors, paste0("[", i, "] ", msg))
    }

    step_rows[[i]] <- data.frame(
      index  = i,
      step   = s$step,
      target = s$label %||% s$table %||% "",
      status = status,
      message = msg,
      stringsAsFactors = FALSE
    )
  }

  steps_df <- do.call(rbind, step_rows)
  valid    <- length(errors) == 0

  structure(
    list(valid = valid, steps = steps_df, errors = errors),
    class = "pdfmacro_validation"
  )
}

#' @export
print.pdfmacro_validation <- function(x, ...) {
  icon <- if (x$valid) "\u2705" else "\u274c"
  cat(icon, if (x$valid) "Macro valid\n" else "Macro has errors\n")
  for (i in seq_len(nrow(x$steps))) {
    r <- x$steps[i, ]
    si <- if (r$status == "ok") "\u2714" else "\u2718"
    cat(sprintf("  %s [%d] %s / %s  %s\n", si, r$index, r$step, r$target, r$message))
  }
  if (length(x$errors) > 0) {
    cat("\nErrors:\n")
    for (e in x$errors) cat(" -", e, "\n")
  }
  invisible(x)
}


# --------------------------------------------------------------------------- #
#  test_extraction() — R-Core Judge for the MCP agent feedback loop           #
# --------------------------------------------------------------------------- #

#' Test extraction parameters and return a structured evaluation report
#'
#' The R-Core Judge for the MCP agent loop. Runs a single extraction pass with
#' the given parameters and returns a structured report indicating whether the
#' result is clean or needs adjustment. The MCP `test_parameters` tool calls
#' this in a loop until `status == "success"`.
#'
#' Returns a list that serialises cleanly to JSON via `jsonlite::toJSON()`.
#'
#' @param file Path to a PDF file.
#' @param page Page number (integer).
#' @param area Named `c(top, left, bottom, right)` in PDF points, or `NULL`
#'   for the full page.
#' @param method Extraction method: `"bbox"` (default), `"lattice"`,
#'   `"stream"`, or `"llm"`.
#' @param header_rows Number of header rows (default 1).
#' @param label Internal table label used during extraction (default
#'   `"target"`).
#' @param provider LLM provider, only used when `method = "llm"`.
#' @param schema Named character vector of column types, only used when
#'   `method = "llm"`.
#' @param preview_rows Number of data rows to include in the response preview
#'   (default 3). Keeps the agent context window small.
#' @param ... Additional arguments forwarded to the extraction function.
#' @return A list with:
#'   \describe{
#'     \item{`status`}{`"success"`, `"needs_alignment"`, `"empty"`, or
#'       `"crash"`.}
#'     \item{`metrics`}{Structural quality metrics: row/col counts, unnamed
#'       headers, empty columns, header-spill detection, likely-numeric
#'       character columns.}
#'     \item{`guidance`}{Plain-English action string for the agent when status
#'       is not `"success"`.}
#'     \item{`preview`}{First `preview_rows` rows as a list of named lists for
#'       JSON serialisation.}
#'   }
#' @export
test_extraction <- function(file,
                             page,
                             area        = NULL,
                             method      = c("bbox", "lattice", "stream", "llm"),
                             header_rows = 1L,
                             label       = "target",
                             provider    = "anthropic",
                             schema      = NULL,
                             preview_rows = 3L,
                             ...) {
  method <- match.arg(method)

  # ── Run extraction in a temporary session ─────────────────────────────────
  tmp <- new.env(parent = emptyenv())
  tmp$path   <- file
  tmp$tables <- list()
  tmp$items  <- list()
  tmp$steps  <- list()
  tmp$.replaying <- TRUE
  class(tmp) <- "pdfmacro_session"

  result <- tryCatch({
    switch(method,
      bbox = ,
      lattice = ,
      stream = select_table(tmp, label = label, page = page, area = area,
                             method = method, header_rows = header_rows, ...),
      llm    = select_table_llm(tmp, label = label, page = page, area = area,
                                 header_rows = header_rows, provider = provider,
                                 schema = schema, ...)
    )
    "ok"
  }, error = function(e) conditionMessage(e))

  if (!identical(result, "ok")) {
    return(list(
      status   = "crash",
      message  = result,
      metrics  = NULL,
      guidance = "Extraction engine threw an error. Check file path, page number, and area coordinates.",
      preview  = NULL
    ))
  }

  df <- tmp$tables[[label]]
  if (is.null(df) || nrow(df) == 0L) {
    return(list(
      status   = "empty",
      message  = "Extraction returned no rows.",
      metrics  = NULL,
      guidance = "No data found. Try a tighter area crop or a different method.",
      preview  = NULL
    ))
  }

  # ── Structural quality checks ──────────────────────────────────────────────

  # 1. Completely empty columns
  empty_cols <- names(df)[vapply(df, function(col)
    all(is.na(col) | trimws(as.character(col)) == ""), logical(1))]

  # 2. Auto-generated placeholder column names (V1, X1, ...1 etc.)
  unnamed_cols <- grep("^V[0-9]+$|^X[0-9]+$|^\\.\\.\\.[0-9]+$",
                        names(df), value = TRUE, perl = TRUE)

  # 3. Header-spill: structural keywords found in data rows suggests header_rows too low
  header_keywords <- "(?i)^(total|amount|description|quantity|price|month|year|date|number|name|type|code|unit|value|weight|count|rate|category)$"
  header_spill <- any(vapply(df, function(col) {
    vals <- trimws(as.character(col[!is.na(col)]))
    any(grepl(header_keywords, vals, perl = TRUE))
  }, logical(1)))

  # 4. Columns that look numeric but are character (likely need cast_types)
  likely_numeric <- names(df)[vapply(df, function(col) {
    if (!is.character(col)) return(FALSE)
    non_blank <- col[!is.na(col) & nchar(trimws(col)) > 0L]
    if (length(non_blank) < 2L) return(FALSE)
    # Remove common numeric formatting before testing
    cleaned <- gsub("[,\\s]", "", non_blank, perl = TRUE)
    mean(!is.na(suppressWarnings(as.numeric(cleaned)))) > 0.8
  }, logical(1))]

  is_clean <- length(unnamed_cols) == 0L &&
              length(empty_cols)   == 0L &&
              !header_spill

  metrics <- list(
    rows_extracted       = nrow(df),
    cols_extracted       = ncol(df),
    unnamed_headers      = as.list(unnamed_cols),
    empty_columns        = as.list(empty_cols),
    header_spill         = header_spill,
    likely_numeric_cols  = as.list(likely_numeric)
  )

  guidance <- if (is_clean) NULL else .extraction_guidance(
    unnamed_cols, empty_cols, header_spill, likely_numeric
  )

  # Preview: list of named lists (JSON-safe)
  n_prev  <- min(as.integer(preview_rows), nrow(df))
  preview <- lapply(seq_len(n_prev), function(i) as.list(df[i, , drop = FALSE]))

  list(
    status   = if (is_clean) "success" else "needs_alignment",
    metrics  = metrics,
    guidance = guidance,
    preview  = preview
  )
}


# Build plain-English guidance for the agent based on detected issues
.extraction_guidance <- function(unnamed_cols, empty_cols,
                                  header_spill, likely_numeric) {
  msgs <- character(0)

  if (header_spill)
    msgs <- c(msgs,
      "Header text detected in data rows — increase header_rows by 1.")

  if (length(unnamed_cols) > 0L)
    msgs <- c(msgs, paste0(
      "Unnamed columns (", paste(unnamed_cols, collapse = ", "),
      ") — adjust area coords or increase header_rows."))

  if (length(empty_cols) > 0L)
    msgs <- c(msgs, paste0(
      "Empty columns (", paste(empty_cols, collapse = ", "),
      ") — crop area more tightly on left/right edges to remove empty columns."))

  if (length(likely_numeric) > 0L)
    msgs <- c(msgs, paste0(
      "Columns that look numeric but are character (",
      paste(likely_numeric, collapse = ", "),
      ") — add cast_types step after extraction."))

  paste(msgs, collapse = " | ")
}
