#' Scan a PDF for tables
#'
#' Scans the specified pages of the PDF for tables and prints a summary.
#' This function is NOT recorded in the macro — it is for exploration only.
#'
#' Results are stored in `sess$detect` as a list of
#' `list(page, index, df)` entries that can be passed directly to
#' [select_table()] via `table_index`. When `method = "docling"`, pages are
#' converted in a single pass and the results are cached so that subsequent
#' [select_table_docling()] calls on those pages run instantly.
#'
#' @param sess A `macrox_session` object.
#' @param pages Integer vector of pages to scan. Defaults to all pages.
#' @param method Extraction method: `"lattice"` (default), `"stream"`, or
#'   `"docling"`. Docling converts all requested pages in one pass and is
#'   best for scanned PDFs or complex layouts.
#' @param min_rows Minimum number of data rows a table must have to be shown
#'   and stored. Default `1`. Set to `0` to see everything.
#' @param max_header_chars Maximum character length allowed for the first
#'   column name before the table is flagged as a likely chart/figure and
#'   skipped. Default `40`. Set to `Inf` to disable. (Lattice/stream only —
#'   Docling's layout model distinguishes charts from tables natively.)
#' @return `sess` invisibly (not recorded).
#' @export
detect_tables <- function(sess, pages = NULL,
                           method = c("lattice", "stream", "docling"),
                           min_rows = 1L, max_header_chars = 40L) {
  method  <- match.arg(method)
  info    <- pdftools::pdf_info(sess$path)
  n_pages <- info$pages

  if (is.null(pages)) pages <- seq_len(n_pages)
  pages <- pages[pages >= 1L & pages <= n_pages]

  results       <- list()
  n_shown       <- 0L
  n_skip_rows   <- 0L
  n_skip_header <- 0L

  if (method == "docling") {
    .require_docling()
    .load_docling_helpers()

    cli::cli_inform(c(
      "i" = "Running Docling on {length(pages)} page{?s} in one pass",
      "i" = "(first call loads models — allow 30-60 s)..."
    ))

    raw <- tryCatch(
      reticulate::py_to_r(
        reticulate::py$docling_detect_range(sess$path, as.integer(pages))
      ),
      error = function(e) cli::cli_abort("Docling detection failed: {conditionMessage(e)}")
    )

    for (item in raw) {
      pg   <- as.integer(item$page)
      tbls <- item$tables
      for (j in seq_along(tbls)) {
        df <- .docling_tbl_to_df(tbls[[j]])
        nr <- nrow(df); nc <- ncol(df)
        if (nr < min_rows) { n_skip_rows <- n_skip_rows + 1L; next }
        cols <- paste(head(names(df), 4L), collapse = ", ")
        if (nc > 4L) cols <- paste0(cols, "...")
        cli::cli_inform("Page {pg}, table {j}: {nr} x {nc}   | {cols}")
        results[[length(results) + 1L]] <- list(page = pg, index = j, df = df)
        n_shown <- n_shown + 1L
      }
    }

  } else {
    # ── tabulapdf path (lattice / stream) ──────────────────────────────────
    for (pg in pages) {
      if (is.null(sess$text)) sess$text <- vector("list", n_pages)
      if (is.null(sess$text[[pg]])) {
        tryCatch(
          sess$text[[pg]] <- pdftools::pdf_text(sess$path)[pg],
          error = function(e) NULL
        )
      }

      if (!requireNamespace("tabulapdf", quietly = TRUE)) {
        cli::cli_warn("tabulapdf not installed — skipping page {pg}.")
        next
      }
      tbls <- tryCatch(
        tabulapdf::extract_tables(sess$path, pages = pg, method = method,
                                   guess = TRUE, output = "matrix"),
        error = function(e) {
          cli::cli_warn("Page {pg}: extraction failed — {conditionMessage(e)}")
          list()
        }
      )

      for (j in seq_along(tbls)) {
        df        <- .matrix_to_df(tbls[[j]])
        nr        <- nrow(df); nc <- ncol(df)
        first_col <- if (nc > 0L) names(df)[[1L]] else ""

        if (nr < min_rows) { n_skip_rows <- n_skip_rows + 1L; next }
        if (nchar(first_col) > max_header_chars) { n_skip_header <- n_skip_header + 1L; next }

        cols <- paste(head(names(df), 4L), collapse = ", ")
        if (nc > 4L) cols <- paste0(cols, "...")
        cli::cli_inform("Page {pg}, table {j}: {nr} x {nc}   | {cols}")

        results[[length(results) + 1L]] <- list(page = pg, index = j, df = df)
        n_shown <- n_shown + 1L
      }
    }
  }

  # ── Summary footer ──────────────────────────────────────────────────────────
  skipped <- n_skip_rows + n_skip_header
  if (skipped > 0L) {
    parts <- character(0)
    if (n_skip_rows   > 0L) parts <- c(parts, paste0(n_skip_rows,   " empty/header-only"))
    if (n_skip_header > 0L) parts <- c(parts, paste0(n_skip_header, " likely chart/figure"))
    cli::cli_inform(c(
      "i" = "{n_shown} table{?s} shown, {skipped} skipped ({paste(parts, collapse = ', ')}).",
      "i" = "Adjust {.arg min_rows} or {.arg max_header_chars} to change filtering."
    ))
  }

  sess$detect <- results
  invisible(sess)
}


#' Detect tables without console output
#'
#' Like [detect_tables()] but returns a named list silently. Intended for
#' agent/programmatic use.
#'
#' @param path Path to a PDF file.
#' @param pages Integer vector of pages to scan. Defaults to all pages.
#' @param method Extraction method: `"lattice"` (default), `"stream"`, or
#'   `"docling"`.
#' @param min_rows Minimum data rows required (default `1`).
#' @param max_header_chars Maximum first-column name length before skipping
#'   (default `40`). Ignored when `method = "docling"`.
#' @return Named list: page number (character) → list of data frames.
#' @export
detect_tables_quietly <- function(path, pages = NULL,
                                   method = c("lattice", "stream", "docling"),
                                   min_rows = 1L, max_header_chars = 40L) {
  method  <- match.arg(method)
  info    <- pdftools::pdf_info(path)
  n_pages <- info$pages
  if (is.null(pages)) pages <- seq_len(n_pages)

  out <- list()

  if (method == "docling") {
    .require_docling()
    .load_docling_helpers()
    raw <- tryCatch(
      reticulate::py_to_r(
        reticulate::py$docling_detect_range(path, as.integer(pages))
      ),
      error = function(e) list()
    )
    for (item in raw) {
      pg   <- as.character(item$page)
      keep <- Filter(Negate(is.null), lapply(item$tables, function(tbl) {
        df <- .docling_tbl_to_df(tbl)
        if (nrow(df) < min_rows) NULL else df
      }))
      if (length(keep) > 0L) out[[pg]] <- keep
    }
  } else {
    for (pg in pages) {
      if (!requireNamespace("tabulapdf", quietly = TRUE)) next
      tbls <- tryCatch(
        tabulapdf::extract_tables(path, pages = pg, method = method,
                                   guess = TRUE, output = "matrix"),
        error = function(e) list()
      )
      keep <- Filter(Negate(is.null), lapply(tbls, function(m) {
        df        <- .matrix_to_df(m)
        first_col <- if (ncol(df) > 0L) names(df)[[1L]] else ""
        if (nrow(df) < min_rows)                 return(NULL)
        if (nchar(first_col) > max_header_chars) return(NULL)
        df
      }))
      if (length(keep) > 0L) out[[as.character(pg)]] <- keep
    }
  }

  out
}
