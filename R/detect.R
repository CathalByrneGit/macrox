#' Scan a PDF for tables
#'
#' Scans the specified pages of the PDF for tables and prints a summary.
#' This function is NOT recorded in the macro — it is for exploration only.
#'
#' @param sess A `pdfmacro_session` object.
#' @param pages Integer vector of pages to scan. Defaults to all pages.
#' @param method Extraction method: `"lattice"` (default) or `"stream"`.
#' @param min_rows Minimum number of data rows a table must have to be shown
#'   and stored. Tables with fewer rows are silently skipped. Default `1`
#'   (i.e. 0-row tables — header-only extractions — are dropped). Set to `0`
#'   to see everything.
#' @param max_header_chars Maximum character length allowed for the first column
#'   name before the table is flagged as a likely chart/figure and skipped.
#'   Long first-column names like
#'   `"X800.000.700.000.600.000..."` are a reliable signal that tabulapdf has
#'   picked up a bar/line chart axis rather than a real table. Default `40`.
#'   Set to `Inf` to disable.
#' @return `sess` invisibly. # not recorded
#' @export
detect_tables <- function(sess, pages = NULL, method = "lattice",
                           min_rows = 1L, max_header_chars = 40L) {
  info    <- pdftools::pdf_info(sess$path)
  n_pages <- info$pages

  if (is.null(pages)) pages <- seq_len(n_pages)
  pages <- pages[pages >= 1 & pages <= n_pages]

  results  <- list()
  n_shown  <- 0L
  n_skip_rows   <- 0L
  n_skip_header <- 0L

  for (pg in pages) {
    # Cache page text
    if (is.null(sess$text)) sess$text <- vector("list", n_pages)
    if (is.null(sess$text[[pg]])) {
      tryCatch(
        sess$text[[pg]] <- pdftools::pdf_text(sess$path)[pg],
        error = function(e) NULL
      )
    }

    if (!requireNamespace("tabulapdf", quietly = TRUE)) {
      cli::cli_warn("tabulapdf not installed — skipping page {pg}. Install tabulapdf for lattice/stream extraction.")
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
      df  <- .matrix_to_df(tbls[[j]])
      nr  <- nrow(df)
      nc  <- ncol(df)

      # ── Filter: too few rows ────────────────────────────────────────────
      if (nr < min_rows) {
        n_skip_rows <- n_skip_rows + 1L
        next
      }

      # ── Filter: first column name suspiciously long (chart signal) ──────
      first_col <- if (nc > 0) names(df)[[1L]] else ""
      if (nchar(first_col) > max_header_chars) {
        n_skip_header <- n_skip_header + 1L
        next
      }

      cols <- paste(head(names(df), 4), collapse = ", ")
      if (nc > 4) cols <- paste0(cols, "...")
      cli::cli_inform("Page {pg}, table {j}: {nr} x {nc}   | {cols}")

      results[[length(results) + 1]] <- list(page = pg, index = j, df = df)
      n_shown <- n_shown + 1L
    }
  }

  # ── Summary footer ────────────────────────────────────────────────────────
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
#' @param method Extraction method.
#' @param min_rows Minimum data rows required (default `1`).
#' @param max_header_chars Maximum first-column name length before skipping
#'   (default `40`).
#' @return Named list: page number (character) → list of data frames.
#' @export
detect_tables_quietly <- function(path, pages = NULL, method = "lattice",
                                   min_rows = 1L, max_header_chars = 40L) {
  info    <- pdftools::pdf_info(path)
  n_pages <- info$pages
  if (is.null(pages)) pages <- seq_len(n_pages)

  out <- list()
  for (pg in pages) {
    if (!requireNamespace("tabulapdf", quietly = TRUE)) next
    tbls <- tryCatch(
      tabulapdf::extract_tables(path, pages = pg, method = method,
                                 guess = TRUE, output = "matrix"),
      error = function(e) list()
    )
    keep <- lapply(tbls, function(m) {
      df        <- .matrix_to_df(m)
      first_col <- if (ncol(df) > 0) names(df)[[1L]] else ""
      if (nrow(df) < min_rows)                    return(NULL)
      if (nchar(first_col) > max_header_chars)    return(NULL)
      df
    })
    keep <- Filter(Negate(is.null), keep)
    if (length(keep) > 0) out[[as.character(pg)]] <- keep
  }
  out
}
