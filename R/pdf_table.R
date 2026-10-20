#' Extract a table from a PDF page as a data frame
#'
#' Reads one table from one page of a PDF with the word-position (`bbox`)
#' engine and returns it as a data frame. It needs no session and records no
#' step, which makes it the building block for scripts and for the blockr
#' blocks (see [new_pdf_table_block()]).
#'
#' @param path Path to a PDF file.
#' @param page Page number.
#' @param area Optional named numeric vector `c(top, left, bottom, right)` in
#'   PDF points. `NULL` (the default) scans the whole page.
#' @param header_rows Number of header rows. Several rows are joined into a
#'   single column name.
#' @param row_tol,col_gap Optional tuning for row clustering and column gap
#'   detection. `NULL` (the default) detects them automatically.
#' @return A data frame.
#' @examples
#' \dontrun{
#' pdf_table("report.pdf", page = 9, header_rows = 4)
#' }
#' @export
pdf_table <- function(path, page, area = NULL, header_rows = 1L,
                      row_tol = NULL, col_gap = NULL) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg pdftools} is required to read PDFs.",
      "i" = "Install with {.code install.packages('pdftools')}"
    ))
  }
  df <- .extract_bbox(path, as.integer(page), area = area,
                      header_rows = as.integer(header_rows),
                      row_tol = row_tol, col_gap = col_gap)
  if (nrow(df) == 0L) {
    cli::cli_abort("No table found on page {page}. Adjust {.arg area} or the tuning.")
  }
  df
}
