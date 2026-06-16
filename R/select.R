#' Extract a table from the PDF and record the step
#'
#' Three selection modes: by page + index, by bounding box (`area`), or by
#' fuzzy caption match (`label_match`). These can be mixed across different
#' tables in the same session.
#'
#' @param sess A `pdfmacro_session` object.
#' @param label Character label for the extracted table.
#' @param page Page number (integer). Required unless `label_match` is supplied.
#' @param table_index Which table on the page (1-based, tabulapdf methods only).
#'   Default 1.
#' @param area Named numeric vector `c(top, left, bottom, right)` in PDF points.
#'   Use [locate_area()] to capture interactively. Strongly recommended with
#'   `method = "bbox"` to isolate the table from surrounding charts/text.
#' @param label_match Text caption to fuzzy-search across all pages.
#' @param method Extraction engine:
#'   * `"lattice"` (default) — tabulapdf, best for grid-lined tables.
#'   * `"stream"` — tabulapdf, best for whitespace-separated tables.
#'   * `"bbox"` — `pdftools::pdf_data()` word-position engine. Handles PDFs
#'     where tabulapdf fails: charts on the same page, missing grid lines,
#'     numbers running together. Use with `area` to clip to the table region.
#' @param header_rows Number of header rows to merge (default 1). Use 2+ for
#'   multi-row spanning headers.
#' @param fuzzy_method stringdist method for caption matching (default `"jw"`).
#' @param max_dist Maximum normalised distance for a fuzzy match (default 0.2).
#' @param row_tol `bbox` method only. Vertical gap (pts) above which a new row
#'   is started. Default `NULL` auto-computes from median character height.
#' @param col_gap `bbox` method only. Horizontal gap (pts) between word groups
#'   that signals a new column. Default `NULL` auto-computes from median word
#'   width.
#' @return `sess` invisibly (step is recorded).
#' @export
select_table <- function(sess, label,
                          page = NULL, table_index = 1,
                          area = NULL, label_match = NULL,
                          method = c("lattice", "stream", "bbox"),
                          header_rows = 1,
                          fuzzy_method = "jw", max_dist = 0.2,
                          row_tol = NULL, col_gap = NULL) {
  method <- match.arg(method)

  # Fuzzy caption search
  if (!is.null(label_match)) {
    found       <- .fuzzy_find_page(sess, label_match, fuzzy_method, max_dist)
    page        <- found$page
    table_index <- found$table_index
    cli::cli_inform(c("i" = "Fuzzy match: page {page}, dist={round(found$dist, 3)} — '{found$line}'"))
  }

  if (is.null(page)) {
    cli::cli_abort("Argument {.arg page} is required (or supply {.arg label_match}).")
  }

  # ── bbox path ─────────────────────────────────────────────────────────────
  if (method == "bbox") {
    if (is.null(area)) {
      cli::cli_inform(c(
        "i" = "No {.arg area} supplied for bbox extraction — scanning full page {page}.",
        "i" = "Use {.fn locate_area} or draw a box in the Shiny module to isolate the table."
      ))
    }
    df <- tryCatch(
      .extract_bbox(sess$path, page,
                    area        = area,
                    header_rows = header_rows,
                    row_tol     = row_tol,
                    col_gap     = col_gap),
      error = function(e) cli::cli_abort(
        "bbox extraction failed on page {page}: {conditionMessage(e)}"
      )
    )
    if (nrow(df) == 0L) {
      cli::cli_abort("bbox extraction returned no data on page {page}. Try adjusting {.arg area}.")
    }

  # ── tabulapdf path (lattice / stream) ─────────────────────────────────────
  } else {
    if (!requireNamespace("tabulapdf", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg tabulapdf} is required for {.val {method}} extraction.",
        "i" = "Install with: {.code install.packages('tabulapdf')}",
        "i" = "Java is also required — see {.url https://www.java.com/download/}."
      ))
    }
    extract_args <- list(
      file   = sess$path,
      pages  = page,
      method = method,
      output = "matrix",
      guess  = is.null(area)
    )
    if (!is.null(area)) extract_args$area <- list(area)

    raw_list <- tryCatch(
      do.call(tabulapdf::extract_tables, extract_args),
      error = function(e) cli::cli_abort(
        "Extraction failed on page {page}: {conditionMessage(e)}"
      )
    )

    if (length(raw_list) == 0L) {
      cli::cli_abort("No tables found on page {page}.")
    }
    if (table_index > length(raw_list)) {
      cli::cli_abort(
        "table_index {table_index} exceeds tables found ({length(raw_list)}) on page {page}."
      )
    }

    df <- .matrix_to_df(raw_list[[table_index]])

    # Flatten multi-row headers (tabulapdf path only — bbox handles this itself)
    if (header_rows > 1L) {
      df <- .flatten_headers(df, header_rows)
    }
  }

  set_table(sess, label, df)

  record_step(sess, list(
    step         = "select_table",
    label        = label,
    page         = page,
    table_index  = table_index,
    area         = area,
    label_match  = label_match,
    method       = method,
    header_rows  = header_rows,
    fuzzy_method = fuzzy_method,
    max_dist     = max_dist,
    row_tol      = row_tol,
    col_gap      = col_gap
  ))

  cli::cli_inform(c("v" = "Table {.val {label}} extracted [{method}]: {nrow(df)} x {ncol(df)}"))
  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  .fuzzy_find_page() — internal                                               #
# --------------------------------------------------------------------------- #

.fuzzy_find_page <- function(sess, label_match, fuzzy_method = "jw", max_dist = 0.2) {
  if (is.null(sess$text)) {
    sess$text <- pdftools::pdf_text(sess$path)
  }

  best_dist  <- Inf
  best_page  <- NA_integer_
  best_line  <- ""

  for (pg in seq_along(sess$text)) {
    page_text <- sess$text[[pg]]
    if (is.null(page_text) || nchar(trimws(page_text)) == 0) next

    lines <- strsplit(page_text, "\n")[[1]]
    lines <- trimws(lines)
    lines <- lines[nchar(lines) >= 4]
    if (length(lines) == 0) next

    dists <- stringdist::stringdist(
      tolower(label_match),
      tolower(lines),
      method = fuzzy_method
    )
    # jw/cosine/jaccard are already bounded 0-1; only normalise raw edit distances
    if (fuzzy_method %in% c("jw", "cosine", "jaccard")) {
      norm_dists <- dists
    } else {
      norm_dists <- dists / pmax(nchar(label_match), nchar(lines), 1)
    }

    idx  <- which.min(norm_dists)
    dist <- norm_dists[[idx]]

    if (dist < best_dist) {
      best_dist <- dist
      best_page <- pg
      best_line <- lines[[idx]]
    }
  }

  if (is.infinite(best_dist) || best_dist > max_dist) {
    cli::cli_abort(c(
      "Fuzzy search failed: no match within max_dist={max_dist}.",
      "i" = "Best candidate: '{best_line}' on page {best_page} (dist={round(best_dist, 3)})"
    ))
  }

  list(page = best_page, table_index = 1, dist = best_dist, line = best_line)
}


# --------------------------------------------------------------------------- #
#  .flatten_headers() / .forward_fill() — internal                            #
# --------------------------------------------------------------------------- #

.flatten_headers <- function(df, header_rows) {
  if (nrow(df) <= header_rows) {
    cli::cli_warn("Table has only {nrow(df)} rows; can't extract {header_rows} header rows.")
    return(df)
  }

  header_mat <- as.matrix(df[seq_len(header_rows), , drop = FALSE])

  # Forward-fill each row (fill blanks from the left)
  for (i in seq_len(nrow(header_mat))) {
    header_mat[i, ] <- .forward_fill(header_mat[i, ])
  }

  # Paste levels per column
  new_names <- vapply(seq_len(ncol(header_mat)), function(j) {
    parts <- header_mat[, j]
    parts <- parts[nchar(trimws(parts)) > 0]
    if (length(parts) == 0) paste0("col_", j) else paste(parts, collapse = " ")
  }, character(1))

  # Make names unique and valid R identifiers (preserves original case)
  new_names <- make.names(new_names, unique = TRUE)

  data_df        <- df[-seq_len(header_rows), , drop = FALSE]
  names(data_df) <- new_names
  rownames(data_df) <- NULL
  data_df
}

.forward_fill <- function(x) {
  last <- ""
  for (i in seq_along(x)) {
    if (trimws(x[[i]]) == "") {
      x[[i]] <- last
    } else {
      last <- x[[i]]
    }
  }
  x
}


# --------------------------------------------------------------------------- #
#  stack_pages()                                                               #
# --------------------------------------------------------------------------- #

#' Extract and stack the same table across multiple PDF pages
#'
#' Extracts a table from each page in `pages` and row-binds them into a single
#' data frame. Handles repeated header rows automatically: when
#' `header_match = TRUE` (default), each page is extracted with the same
#' `header_rows` setting so the repeated header is consumed as column names and
#' dropped from the data automatically. Use `header_match = FALSE` when pages
#' 2+ start directly with data (no repeated header).
#'
#' @param sess A `pdfmacro_session` object.
#' @param label Character label for the combined table.
#' @param pages Integer vector of page numbers to extract and stack (minimum 2).
#' @param area Named `c(top, left, bottom, right)` in PDF points, or `NULL`
#'   for the full page. Applied identically to every page.
#' @param method Extraction engine: `"bbox"` (default), `"lattice"`, or
#'   `"stream"`.
#' @param header_rows Number of header rows (default 1). Applied to every page
#'   when `header_match = TRUE`, only to the first page when `FALSE`.
#' @param header_match Logical (default `TRUE`). When `TRUE` each page is
#'   extracted with `header_rows` so repeated headers are consumed into column
#'   names and dropped. Set `FALSE` when pages 2+ begin directly with data.
#' @param row_tol `bbox` method only — passed to [select_table()].
#' @param col_gap `bbox` method only — passed to [select_table()].
#' @return `sess` invisibly (step is recorded).
#' @export
stack_pages <- function(sess, label, pages,
                        area         = NULL,
                        method       = c("bbox", "lattice", "stream"),
                        header_rows  = 1L,
                        header_match = TRUE,
                        row_tol      = NULL,
                        col_gap      = NULL) {
  method <- match.arg(method)
  pages  <- as.integer(pages)
  if (length(pages) < 2L) {
    cli::cli_abort("{.arg pages} must contain at least 2 page numbers.")
  }

  cli::cli_inform(c("i" = "Stacking {length(pages)} page{?s} ({min(pages)}–{max(pages)}) [{method}]..."))

  # Temporary session: .replaying = TRUE suppresses record_step inside select_table
  tmp            <- new.env(parent = emptyenv())
  tmp$path       <- sess$path
  tmp$tables     <- list()
  tmp$steps      <- list()
  tmp$.replaying <- TRUE
  class(tmp)     <- "pdfmacro_session"

  # First page — defines column names and shape
  select_table(tmp, label = ".sp_p1", page = pages[[1L]], area = area,
               method = method, header_rows = header_rows,
               row_tol = row_tol, col_gap = col_gap)
  df1    <- tmp$tables[[".sp_p1"]]
  frames <- list(df1)
  n_skip <- 0L

  for (pg in pages[-1L]) {
    # header_match = TRUE  → extract with same header_rows (repeated header
    #   becomes column names, aligning naturally with df1).
    # header_match = FALSE → extract with header_rows = 0 then assign df1 names.
    hr <- if (isTRUE(header_match)) header_rows else 0L

    dfx <- tryCatch({
      select_table(tmp, label = ".sp_px", page = pg, area = area,
                   method = method, header_rows = hr,
                   row_tol = row_tol, col_gap = col_gap)
      tmp$tables[[".sp_px"]]
    }, error = function(e) {
      cli::cli_warn("Page {pg} skipped: {conditionMessage(e)}")
      n_skip <<- n_skip + 1L
      NULL
    })
    if (is.null(dfx)) next

    # When header didn't repeat, assign page-1 column names by position
    if (!isTRUE(header_match) && ncol(dfx) == ncol(df1)) {
      names(dfx) <- names(df1)
    }

    # Align to page-1 columns
    shared <- intersect(names(df1), names(dfx))
    if (length(shared) < ncol(df1)) {
      cli::cli_warn("Page {pg}: binding on {length(shared)}/{ncol(df1)} shared column{?s}.")
    }
    frames[[1L]] <- frames[[1L]][, shared, drop = FALSE]
    frames       <- c(frames, list(dfx[, shared, drop = FALSE]))
  }

  df           <- do.call(rbind, frames)
  rownames(df) <- NULL

  n_ok <- length(pages) - n_skip
  cli::cli_inform(c(
    "v" = "Table {.val {label}} stacked from {n_ok}/{length(pages)} page{?s}: {nrow(df)} × {ncol(df)}"
  ))

  set_table(sess, label, df)
  record_step(sess, list(
    step         = "stack_pages",
    label        = label,
    pages        = pages,
    area         = area,
    method       = method,
    header_rows  = as.integer(header_rows),
    header_match = isTRUE(header_match),
    row_tol      = row_tol,
    col_gap      = col_gap
  ))

  invisible(sess)
}
