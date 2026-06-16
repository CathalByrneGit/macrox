# --------------------------------------------------------------------------- #
#  diff_replay() — compare macro outputs across two PDF files                 #
# --------------------------------------------------------------------------- #

#' Compare macro outputs across two PDF files
#'
#' Replays the same macro against two files and returns a structured diff
#' showing added, removed, and changed tables and rows. Useful for detecting
#' format changes between annual editions of the same report.
#'
#' @param file1 Path to the first (reference) PDF.
#' @param file2 Path to the second (new) PDF.
#' @param macro Macro name, path to a `.yml`, or a step list.
#' @param macro_path Directory for macro lookup (default `.`).
#' @return A `pdfmacro_diff` object (print method included).
#' @export
diff_replay <- function(file1, file2, macro, macro_path = ".") {
  cli::cli_inform(c("i" = "Replaying macro on {.file {basename(file1)}}..."))
  t1 <- pdf_replay(file1, macro, macro_path)
  cli::cli_inform(c("i" = "Replaying macro on {.file {basename(file2)}}..."))
  t2 <- pdf_replay(file2, macro, macro_path)

  all_labels <- union(names(t1), names(t2))

  diffs <- lapply(all_labels, function(nm) {
    in1 <- nm %in% names(t1)
    in2 <- nm %in% names(t2)

    if (!in1) return(list(table = nm, status = "added",
                           rows1 = NA_integer_, rows2 = nrow(t2[[nm]]),
                           cols1 = NA_integer_, cols2 = ncol(t2[[nm]]),
                           col_changed = FALSE, cell_changes = NA_integer_,
                           new_cols = character(0), dropped_cols = character(0)))

    if (!in2) return(list(table = nm, status = "removed",
                           rows1 = nrow(t1[[nm]]), rows2 = NA_integer_,
                           cols1 = ncol(t1[[nm]]), cols2 = NA_integer_,
                           col_changed = FALSE, cell_changes = NA_integer_,
                           new_cols = character(0), dropped_cols = character(0)))

    df1 <- t1[[nm]]
    df2 <- t2[[nm]]

    new_cols     <- setdiff(names(df2), names(df1))
    dropped_cols <- setdiff(names(df1), names(df2))
    col_changed  <- !identical(sort(names(df1)), sort(names(df2)))

    # Cell-level diff on common columns, common rows
    common_cols <- intersect(names(df1), names(df2))
    n_common_rows <- min(nrow(df1), nrow(df2))
    cell_changes <- 0L
    if (length(common_cols) > 0L && n_common_rows > 0L) {
      m1 <- as.matrix(df1[seq_len(n_common_rows), common_cols, drop = FALSE])
      m2 <- as.matrix(df2[seq_len(n_common_rows), common_cols, drop = FALSE])
      cell_changes <- sum(m1 != m2, na.rm = TRUE)
    }

    any_change <- col_changed || nrow(df1) != nrow(df2) || cell_changes > 0L

    list(
      table        = nm,
      status       = if (any_change) "changed" else "unchanged",
      rows1        = nrow(df1),
      rows2        = nrow(df2),
      cols1        = ncol(df1),
      cols2        = ncol(df2),
      col_changed  = col_changed,
      cell_changes = cell_changes,
      new_cols     = new_cols,
      dropped_cols = dropped_cols
    )
  })
  names(diffs) <- all_labels

  n_changed   <- sum(vapply(diffs, function(d) d$status == "changed",   logical(1)))
  n_added     <- sum(vapply(diffs, function(d) d$status == "added",     logical(1)))
  n_removed   <- sum(vapply(diffs, function(d) d$status == "removed",   logical(1)))
  n_unchanged <- sum(vapply(diffs, function(d) d$status == "unchanged", logical(1)))

  if (n_changed + n_added + n_removed == 0L) {
    cli::cli_inform(c("v" = "No differences detected across {length(all_labels)} table{?s}."))
  } else {
    cli::cli_inform(c(
      "i" = "{n_unchanged} unchanged, {n_changed} changed, {n_added} added, {n_removed} removed."
    ))
  }

  structure(
    list(file1 = basename(file1), file2 = basename(file2), tables = diffs),
    class = "pdfmacro_diff"
  )
}

#' @export
print.pdfmacro_diff <- function(x, ...) {
  cat("pdfmacro diff\n")
  cat("  Reference:", x$file1, "\n")
  cat("  New:      ", x$file2, "\n\n")

  icons <- c(unchanged = "\u2714", changed = "\u007e", added = "+", removed = "-")

  for (nm in names(x$tables)) {
    d    <- x$tables[[nm]]
    icon <- icons[[d$status]]
    cat(icon, nm, paste0("[", d$status, "]"))

    if (d$status == "changed") {
      parts <- character(0)
      if (!is.na(d$rows1) && d$rows1 != d$rows2)
        parts <- c(parts, paste0("rows: ", d$rows1, "\u2192", d$rows2))
      if (d$col_changed) {
        if (length(d$new_cols)     > 0L) parts <- c(parts, paste0("+cols: ", paste(d$new_cols,     collapse = ",")))
        if (length(d$dropped_cols) > 0L) parts <- c(parts, paste0("-cols: ", paste(d$dropped_cols, collapse = ",")))
      }
      if (!is.na(d$cell_changes) && d$cell_changes > 0L)
        parts <- c(parts, paste0(d$cell_changes, " cell change", if (d$cell_changes > 1L) "s"))
      if (length(parts) > 0L) cat(" (", paste(parts, collapse = ", "), ")", sep = "")
    } else if (d$status %in% c("added", "removed")) {
      rows <- if (!is.na(d$rows2)) d$rows2 else d$rows1
      cat(paste0(" [", rows, " rows]"))
    }
    cat("\n")
  }
  invisible(x)
}
