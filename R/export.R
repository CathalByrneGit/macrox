# --------------------------------------------------------------------------- #
#  Export helpers                                                               #
# --------------------------------------------------------------------------- #

#' Export all extracted tables to CSV files
#'
#' Writes one `.csv` per table into `dir`. Existing files are overwritten.
#'
#' @param sess A `macrox_session` object.
#' @param dir Directory to write into (created if it doesn't exist).
#' @param tables Character vector of table labels to export. Default: all tables.
#' @return Named character vector of output file paths, invisibly.
#' @export
export_csv <- function(sess, dir = ".", tables = NULL) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  labels <- tables %||% names(sess$tables)
  if (length(labels) == 0L) cli::cli_abort("No tables extracted yet.")

  paths <- vapply(labels, function(lbl) {
    out <- file.path(dir, paste0(lbl, ".csv"))
    utils::write.csv(get_table(sess, lbl), out, row.names = FALSE)
    out
  }, character(1))

  cli::cli_inform(c("v" = "Exported {length(paths)} CSV{?s} to {.path {dir}}"))
  invisible(paths)
}


#' Export all extracted tables to an Excel workbook
#'
#' Writes one worksheet per table. Requires the `writexl` or `openxlsx`
#' package.
#'
#' @param sess A `macrox_session` object.
#' @param path Output `.xlsx` file path (default `"macrox_tables.xlsx"`).
#' @param tables Character vector of table labels to include. Default: all.
#' @return `path` invisibly.
#' @export
export_excel <- function(sess, path = "macrox_tables.xlsx", tables = NULL) {
  labels <- tables %||% names(sess$tables)
  if (length(labels) == 0L) cli::cli_abort("No tables extracted yet.")

  tbl_list <- setNames(
    lapply(labels, function(l) get_table(sess, l)),
    labels
  )

  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(tbl_list, path)
  } else if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    for (nm in names(tbl_list)) {
      openxlsx::addWorksheet(wb, nm)
      openxlsx::writeData(wb, nm, tbl_list[[nm]])
    }
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  } else {
    cli::cli_abort(c(
      "Install {.pkg writexl} or {.pkg openxlsx} for Excel export.",
      "i" = "{.code install.packages('writexl')}"
    ))
  }

  cli::cli_inform(c("v" = "Exported {length(labels)} sheet{?s} to {.path {path}}"))
  invisible(path)
}
