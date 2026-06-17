# --------------------------------------------------------------------------- #
#  diff_replay() — compare macro outputs across two PDF files                 #
# --------------------------------------------------------------------------- #

#' Compare macro outputs across two PDF files
#'
#' Replays the same macro against two files and returns a structured diff
#' showing added, removed, and changed tables. For changed tables the diff
#' includes per-column numeric delta statistics. Use [detail()] on the result
#' to inspect which cells changed.
#'
#' @param file1 Path to the reference PDF.
#' @param file2 Path to the new PDF.
#' @param macro Macro name, path to a `.yml`, or a step list.
#' @param macro_path Directory for macro lookup (default `.`).
#' @return A `macrox_diff` object. Print gives a summary; [detail()] drills
#'   into a single table.
#' @export
diff_replay <- function(file1, file2, macro, macro_path = ".") {
  cli::cli_inform(c("i" = "Replaying macro on {.file {basename(file1)}}..."))
  t1 <- mx_replay(file1, macro, macro_path)
  cli::cli_inform(c("i" = "Replaying macro on {.file {basename(file2)}}..."))
  t2 <- mx_replay(file2, macro, macro_path)

  all_labels <- union(names(t1), names(t2))

  diffs <- lapply(all_labels, function(nm) {
    in1 <- nm %in% names(t1)
    in2 <- nm %in% names(t2)

    if (!in1) return(list(
      table = nm, status = "added",
      rows1 = NA_integer_, rows2 = nrow(t2[[nm]]),
      cols1 = NA_integer_, cols2 = ncol(t2[[nm]]),
      col_changed = FALSE, cell_changes = NA_integer_,
      new_cols = character(0), dropped_cols = character(0),
      numeric_deltas = NULL
    ))

    if (!in2) return(list(
      table = nm, status = "removed",
      rows1 = nrow(t1[[nm]]), rows2 = NA_integer_,
      cols1 = ncol(t1[[nm]]), cols2 = NA_integer_,
      col_changed = FALSE, cell_changes = NA_integer_,
      new_cols = character(0), dropped_cols = character(0),
      numeric_deltas = NULL
    ))

    df1 <- t1[[nm]]
    df2 <- t2[[nm]]

    new_cols     <- setdiff(names(df2), names(df1))
    dropped_cols <- setdiff(names(df1), names(df2))
    col_changed  <- !identical(sort(names(df1)), sort(names(df2)))

    common_cols   <- intersect(names(df1), names(df2))
    n_common_rows <- min(nrow(df1), nrow(df2))
    cell_changes  <- 0L

    numeric_deltas <- NULL

    if (length(common_cols) > 0L && n_common_rows > 0L) {
      m1 <- as.matrix(df1[seq_len(n_common_rows), common_cols, drop = FALSE])
      m2 <- as.matrix(df2[seq_len(n_common_rows), common_cols, drop = FALSE])
      changed_mat  <- m1 != m2 & !(is.na(m1) & is.na(m2))
      cell_changes <- sum(changed_mat, na.rm = TRUE)

      # Per-column delta stats for columns with numeric content
      delta_rows <- lapply(common_cols, function(col) {
        v1 <- suppressWarnings(as.numeric(df1[[col]][seq_len(n_common_rows)]))
        v2 <- suppressWarnings(as.numeric(df2[[col]][seq_len(n_common_rows)]))
        is_num <- !all(is.na(v1)) && !all(is.na(v2))

        n_changed_col <- sum(changed_mat[, col], na.rm = TRUE)

        if (is_num) {
          delta <- v2 - v1
          list(
            col           = col,
            n_changed     = n_changed_col,
            mean_abs_delta = round(mean(abs(delta), na.rm = TRUE), 3),
            max_abs_delta  = round(max(abs(delta), na.rm = TRUE), 3),
            sum_delta      = round(sum(delta, na.rm = TRUE), 3)
          )
        } else {
          list(
            col           = col,
            n_changed     = n_changed_col,
            mean_abs_delta = NA_real_,
            max_abs_delta  = NA_real_,
            sum_delta      = NA_real_
          )
        }
      })

      delta_rows <- delta_rows[vapply(delta_rows, function(r) r$n_changed > 0L, logical(1))]
      if (length(delta_rows) > 0L) {
        numeric_deltas <- data.frame(
          col            = vapply(delta_rows, `[[`, character(1), "col"),
          n_changed      = vapply(delta_rows, `[[`, integer(1),   "n_changed"),
          mean_abs_delta = vapply(delta_rows, `[[`, double(1),    "mean_abs_delta"),
          max_abs_delta  = vapply(delta_rows, `[[`, double(1),    "max_abs_delta"),
          sum_delta      = vapply(delta_rows, `[[`, double(1),    "sum_delta"),
          stringsAsFactors = FALSE
        )
      }
    }

    any_change <- col_changed || nrow(df1) != nrow(df2) || cell_changes > 0L

    list(
      table          = nm,
      status         = if (any_change) "changed" else "unchanged",
      rows1          = nrow(df1),
      rows2          = nrow(df2),
      cols1          = ncol(df1),
      cols2          = ncol(df2),
      col_changed    = col_changed,
      cell_changes   = cell_changes,
      new_cols       = new_cols,
      dropped_cols   = dropped_cols,
      numeric_deltas = numeric_deltas
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
    list(
      file1  = basename(file1),
      file2  = basename(file2),
      tables = diffs,
      ref    = t1,
      new    = t2
    ),
    class = "macrox_diff"
  )
}


#' @export
print.macrox_diff <- function(x, ...) {
  cat("macrox diff\n")
  cat("  Reference:", x$file1, "\n")
  cat("  New:      ", x$file2, "\n\n")

  icons <- c(unchanged = "✔", changed = "~", added = "+", removed = "-")

  for (nm in names(x$tables)) {
    d    <- x$tables[[nm]]
    icon <- icons[[d$status]]
    cat(icon, nm, paste0("[", d$status, "]"))

    if (d$status == "changed") {
      parts <- character(0)
      if (!is.na(d$rows1) && d$rows1 != d$rows2)
        parts <- c(parts, paste0("rows: ", d$rows1, "→", d$rows2))
      if (d$col_changed) {
        if (length(d$new_cols)     > 0L) parts <- c(parts, paste0("+cols: ", paste(d$new_cols,     collapse = ",")))
        if (length(d$dropped_cols) > 0L) parts <- c(parts, paste0("-cols: ", paste(d$dropped_cols, collapse = ",")))
      }
      if (!is.na(d$cell_changes) && d$cell_changes > 0L)
        parts <- c(parts, paste0(d$cell_changes, " cell change", if (d$cell_changes > 1L) "s"))
      if (length(parts) > 0L) cat(" (", paste(parts, collapse = ", "), ")", sep = "")
      cat("\n")

      # Per-column numeric delta summary
      nd <- d$numeric_deltas
      if (!is.null(nd)) {
        num_nd <- nd[!is.na(nd$mean_abs_delta), ]
        for (i in seq_len(nrow(num_nd))) {
          cat(sprintf("    %-20s  Δ mean=%-10.3g  max=%-10.3g  sum=%-10.3g  (%d row%s)\n",
            num_nd$col[i],
            num_nd$mean_abs_delta[i],
            num_nd$max_abs_delta[i],
            num_nd$sum_delta[i],
            num_nd$n_changed[i],
            if (num_nd$n_changed[i] != 1L) "s" else ""))
        }
        char_nd <- nd[is.na(nd$mean_abs_delta), ]
        for (i in seq_len(nrow(char_nd))) {
          cat(sprintf("    %-20s  (text, %d change%s)\n",
            char_nd$col[i],
            char_nd$n_changed[i],
            if (char_nd$n_changed[i] != 1L) "s" else ""))
        }
      }

    } else if (d$status %in% c("added", "removed")) {
      rows <- if (!is.na(d$rows2)) d$rows2 else d$rows1
      cat(paste0(" [", rows, " rows]\n"))
    } else {
      cat("\n")
    }
  }
  invisible(x)
}


# --------------------------------------------------------------------------- #
#  detail() — cell-level diff for one table                                   #
# --------------------------------------------------------------------------- #

#' Inspect cell-level changes for one table in a diff
#'
#' Returns a data frame of every cell that changed between the two replays, with
#' columns `row`, `col`, `ref`, `new`, and `delta` (numeric difference, or `NA`
#' for non-numeric columns).
#'
#' @param x A `macrox_diff` object returned by [diff_replay()].
#' @param table Character name of the table to inspect.
#' @param ... Unused.
#' @return A data frame with one row per changed cell.
#' @export
detail <- function(x, ...) UseMethod("detail")

#' @export
detail.macrox_diff <- function(x, table, ...) {
  if (!table %in% names(x$tables)) {
    cli::cli_abort("Table {.val {table}} not in diff. Available: {.val {names(x$tables)}}")
  }
  d <- x$tables[[table]]
  if (d$status == "unchanged") {
    cli::cli_inform("Table {.val {table}} is unchanged.")
    return(invisible(
      data.frame(row = integer(0), col = character(0),
                 ref = character(0), new = character(0),
                 delta = numeric(0), stringsAsFactors = FALSE)
    ))
  }
  if (d$status %in% c("added", "removed")) {
    cli::cli_inform("Table {.val {table}} was {d$status} — no cell comparison available.")
    return(invisible(
      data.frame(row = integer(0), col = character(0),
                 ref = character(0), new = character(0),
                 delta = numeric(0), stringsAsFactors = FALSE)
    ))
  }

  df1 <- x$ref[[table]]
  df2 <- x$new[[table]]

  common_cols   <- intersect(names(df1), names(df2))
  n_common_rows <- min(nrow(df1), nrow(df2))

  if (length(common_cols) == 0L || n_common_rows == 0L) {
    return(data.frame(row = integer(0), col = character(0),
                      ref = character(0), new = character(0),
                      delta = numeric(0), stringsAsFactors = FALSE))
  }

  rows <- list()
  for (col in common_cols) {
    v1 <- as.character(df1[[col]][seq_len(n_common_rows)])
    v2 <- as.character(df2[[col]][seq_len(n_common_rows)])
    changed <- which(v1 != v2 | (is.na(v1) != is.na(v2)))
    if (length(changed) == 0L) next

    n1  <- suppressWarnings(as.numeric(v1[changed]))
    n2  <- suppressWarnings(as.numeric(v2[changed]))
    dlt <- ifelse(!is.na(n1) & !is.na(n2), n2 - n1, NA_real_)

    rows[[length(rows) + 1L]] <- data.frame(
      row   = changed,
      col   = col,
      ref   = v1[changed],
      new   = v2[changed],
      delta = dlt,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L) {
    return(data.frame(row = integer(0), col = character(0),
                      ref = character(0), new = character(0),
                      delta = numeric(0), stringsAsFactors = FALSE))
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$row, out$col), ]
  rownames(out) <- NULL
  out
}
