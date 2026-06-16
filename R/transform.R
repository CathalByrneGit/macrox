# --------------------------------------------------------------------------- #
#  rename_columns()                                                             #
# --------------------------------------------------------------------------- #

#' Rename columns in an extracted table
#'
#' @param sess A `pdfmacro_session` object.
#' @param table Character label of the target table.
#' @param mapping Named character vector: `c(OldName = "new_name", ...)`.
#' @return `sess` invisibly (step is recorded).
#' @export
rename_columns <- function(sess, table, mapping) {
  df      <- get_table(sess, table)
  present <- names(mapping) %in% names(df)

  if (!all(present)) {
    missing_cols <- names(mapping)[!present]
    cli::cli_warn(c(
      "!" = "Some columns not found in {.val {table}} and were skipped.",
      "i" = "Missing: {.val {missing_cols}}"
    ))
    mapping <- mapping[present]
  }

  if (length(mapping) > 0) {
    idx            <- match(names(mapping), names(df))
    names(df)[idx] <- unname(mapping)
    set_table(sess, table, df)
  }

  record_step(sess, list(
    step    = "rename_columns",
    table   = table,
    mapping = as.list(mapping)
  ))

  cli::cli_inform(c("v" = "Renamed {length(mapping)} column{?s} in {.val {table}}"))
  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  cast_types()                                                                 #
# --------------------------------------------------------------------------- #

#' Cast column types in an extracted table
#'
#' @param sess A `pdfmacro_session` object.
#' @param table Character label of the target table.
#' @param types Named character vector: `c(col = "integer")`. Supported types:
#'   `"numeric"`, `"integer"`, `"character"`, `"date:<fmt>"` e.g.
#'   `"date:%d/%m/%Y"`.
#' @return `sess` invisibly (step is recorded).
#' @export
cast_types <- function(sess, table, types) {
  df <- get_table(sess, table)

  for (col in names(types)) {
    if (!(col %in% names(df))) {
      cli::cli_warn("Column {.val {col}} not found in {.val {table}}, skipped.")
      next
    }
    result <- tryCatch(
      .cast_col(df[[col]], types[[col]]),
      error = function(e) {
        cli::cli_warn("Failed to cast {.val {col}} as {.val {types[[col]]}}: {conditionMessage(e)}")
        df[[col]]
      }
    )
    df[[col]] <- result
  }

  set_table(sess, table, df)

  record_step(sess, list(
    step  = "cast_types",
    table = table,
    types = as.list(types)
  ))

  cli::cli_inform(c("v" = "Cast {length(types)} column type{?s} in {.val {table}}"))
  invisible(sess)
}

.cast_col <- function(x, type_spec) {
  clean <- function(v) gsub("[,\\s ]", "", as.character(v))

  if (grepl("^date:", type_spec)) {
    fmt <- sub("^date:", "", type_spec)
    return(as.Date(as.character(x), format = fmt))
  }

  switch(type_spec,
    numeric   = suppressWarnings(as.numeric(clean(x))),
    integer   = suppressWarnings(as.integer(clean(x))),
    character = as.character(x),
    cli::cli_abort("Unknown type spec: {.val {type_spec}}")
  )
}


# --------------------------------------------------------------------------- #
#  filter_rows()                                                                #
# --------------------------------------------------------------------------- #

#' Remove rows matching an expression
#'
#' @param sess A `pdfmacro_session` object.
#' @param table Character label of the target table.
#' @param exclude_where Character string expression evaluated against the data
#'   frame columns. Rows where the expression is `TRUE` are removed.
#' @return `sess` invisibly (step is recorded).
#' @export
filter_rows <- function(sess, table, exclude_where) {
  df   <- get_table(sess, table)
  mask <- tryCatch(
    eval(parse(text = exclude_where), envir = df),
    error = function(e) cli::cli_abort(
      "Expression {.code {exclude_where}} failed: {conditionMessage(e)}"
    )
  )

  if (!is.logical(mask) || length(mask) != nrow(df)) {
    cli::cli_abort("Expression must return a logical vector of length {nrow(df)}.")
  }

  n_removed <- sum(mask, na.rm = TRUE)
  df        <- df[!mask, , drop = FALSE]
  rownames(df) <- NULL
  set_table(sess, table, df)

  record_step(sess, list(
    step          = "filter_rows",
    table         = table,
    exclude_where = exclude_where
  ))

  cli::cli_inform(c("v" = "Removed {n_removed} row{?s} from {.val {table}}"))
  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  preview() / preview_all()                                                    #
# --------------------------------------------------------------------------- #

#' Preview one extracted table
#'
#' @param sess A `pdfmacro_session` object.
#' @param table Character label of the table to preview.
#' @param n Number of rows to show (default 6).
#' @return `sess` invisibly. # not recorded
#' @export
preview <- function(sess, table, n = 6) {
  df <- get_table(sess, table)
  cli::cli_h3("Table: {table}  [{nrow(df)} x {ncol(df)}]")
  cat("Cols:", paste(names(df), collapse = ", "), "\n")
  print(utils::head(df, n))
  invisible(sess)
}


#' Preview all extracted tables with numeric totals
#'
#' @param sess A `pdfmacro_session` object.
#' @param n Number of rows to show per table (default 6).
#' @return `sess` invisibly. # not recorded
#' @export
preview_all <- function(sess, n = 6) {
  n_tables <- length(sess$tables)
  n_steps  <- length(sess$steps)
  n_flags  <- sum(vapply(sess$steps, function(s) isTRUE(s$.flagged), logical(1)))

  cli::cli_inform("Session summary: {n_tables} table{?s}, {n_steps} step{?s}, {n_flags} flag{?s}")

  if (n_tables == 0) {
    cli::cli_inform("No tables extracted yet.")
    return(invisible(sess))
  }

  for (lbl in names(sess$tables)) {
    df      <- sess$tables[[lbl]]
    flagged <- any(vapply(sess$steps, function(s) {
      isTRUE(s$.flagged) && (s$label %||% s$table %||% "") == lbl
    }, logical(1)))

    flag_str <- if (flagged) " \u26a0 FLAGGED" else ""
    cli::cli_rule(left = paste0("Table: ", lbl, "  [", nrow(df), " x ", ncol(df), "]", flag_str))
    cli::cli_inform("Cols: {.val {names(df)}}")

    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(num_cols) > 0) {
      totals <- vapply(num_cols, function(col) {
        s <- sum(df[[col]], na.rm = TRUE)
        formatC(s, format = "f", digits = 0, big.mark = ",")
      }, character(1))
      tot_str <- paste(paste0(num_cols, ": ", totals), collapse = "  |  ")
      cli::cli_inform("Totals: {tot_str}")
    }

    print(utils::head(df, n))
    cat("\n")
  }

  if (n_flags == 0) {
    cli::cli_inform(c("v" = "All tables look clean. Ready to {.fn save_macro}."))
  } else {
    cli::cli_warn(c(
      "!" = "Review {n_flags} flagged step{?s} before saving.",
      "i" = "Run {.fn show_steps} to inspect."
    ))
  }

  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  add_column()                                                                #
# --------------------------------------------------------------------------- #

#' Add a derived column to an extracted table
#'
#' @param sess A `pdfmacro_session` object.
#' @param table Character label of the target table.
#' @param name Name of the new column.
#' @param expr R expression (as a string) evaluated against the data frame,
#'   e.g. `"male_count + female_count"` or `"2024L"`.
#' @return `sess` invisibly (step is recorded).
#' @export
add_column <- function(sess, table, name, expr) {
  df <- get_table(sess, table)
  result <- tryCatch(
    eval(parse(text = expr), envir = df),
    error = function(e) cli::cli_abort(
      "Expression {.code {expr}} failed: {conditionMessage(e)}"
    )
  )
  if (length(result) == 1L) result <- rep(result, nrow(df))
  if (length(result) != nrow(df)) {
    cli::cli_abort("Expression must return length 1 or {nrow(df)}, got {length(result)}.")
  }
  df[[name]] <- result
  set_table(sess, table, df)
  record_step(sess, list(step = "add_column", table = table,
                          name = name, expr = expr))
  cli::cli_inform(c("v" = "Added column {.val {name}} to {.val {table}}"))
  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  stack_tables()                                                              #
# --------------------------------------------------------------------------- #

#' Row-bind multiple extracted tables into one
#'
#' Useful after batch replay to combine monthly or regional tables that share
#' the same schema.
#'
#' @param sess A `pdfmacro_session` object.
#' @param label Label for the combined output table.
#' @param tables Character vector of existing table labels to stack.
#' @param .fill Fill missing columns with `NA` when schemas differ
#'   (default `FALSE`).
#' @return `sess` invisibly (step is recorded).
#' @export
stack_tables <- function(sess, label, tables, .fill = FALSE) {
  dfs <- lapply(tables, function(l) get_table(sess, l))

  if (.fill) {
    all_cols <- unique(unlist(lapply(dfs, names)))
    dfs <- lapply(dfs, function(df) {
      missing <- setdiff(all_cols, names(df))
      for (col in missing) df[[col]] <- NA
      df[all_cols]
    })
  }

  combined        <- do.call(rbind, dfs)
  rownames(combined) <- NULL
  set_table(sess, label, combined)
  record_step(sess, list(step = "stack_tables", label = label,
                          tables = tables, .fill = .fill))
  cli::cli_inform(c(
    "v" = "Stacked {length(tables)} table{?s} into {.val {label}}: {nrow(combined)} rows"
  ))
  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  merge_tables()                                                              #
# --------------------------------------------------------------------------- #

#' Join two extracted tables
#'
#' Wraps `base::merge()` and records the step so it replays automatically.
#'
#' @param sess A `pdfmacro_session` object.
#' @param label Label for the merged output table.
#' @param left Label of the left table.
#' @param right Label of the right table.
#' @param by Character vector of column names to join on.
#' @param all Logical; `FALSE` (default) = inner join, `TRUE` = full outer.
#' @param all.x Left join when `TRUE`.
#' @param all.y Right join when `TRUE`.
#' @return `sess` invisibly (step is recorded).
#' @export
merge_tables <- function(sess, label, left, right, by,
                          all = FALSE, all.x = FALSE, all.y = FALSE) {
  df_left  <- get_table(sess, left)
  df_right <- get_table(sess, right)
  combined <- merge(df_left, df_right, by = by,
                    all = all, all.x = all.x, all.y = all.y)
  rownames(combined) <- NULL
  set_table(sess, label, combined)
  record_step(sess, list(step = "merge_tables", label = label,
                          left = left, right = right, by = by,
                          all = all, all.x = all.x, all.y = all.y))
  cli::cli_inform(c(
    "v" = "Merged {.val {left}} + {.val {right}} into {.val {label}}: {nrow(combined)} rows"
  ))
  invisible(sess)
}
