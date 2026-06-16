# --------------------------------------------------------------------------- #
#  validate_table() — data quality checks via the validate package            #
#                                                                              #
#  Rules are named R expressions stored as strings so they serialise into     #
#  the YAML macro and run automatically on replay.                             #
# --------------------------------------------------------------------------- #

#' Validate an extracted table against a set of rules
#'
#' Uses the `validate` package DSL. Rules are named character strings
#' containing R expressions evaluated against the data frame. Both row-level
#' predicates (`total_count > 0`) and table-level aggregates (`nrow(.) == 12`)
#' are supported.
#'
#' Results are stored in `sess$validations[[table]]` for later inspection.
#' On replay, failures produce warnings by default; set `strict = TRUE` to
#' abort.
#'
#' @param sess A `pdfmacro_session` object.
#' @param table Character label of the table to validate.
#' @param rules Named character vector or named list of R expression strings.
#'   Names become the rule identifiers shown in reports. Alternatively, a
#'   `validate::validator()` object.
#' @param strict If `TRUE`, any rule failure calls `cli_abort()` instead of
#'   `cli_warn()`. Default `FALSE`.
#' @return `sess` invisibly (step is recorded).
#'
#' @examples
#' \dontrun{
#' sess |> validate_table("calf_monthly", rules = c(
#'   no_na_month    = "!anyNA(month)",
#'   positive_males = "all(male_count > 0, na.rm = TRUE)",
#'   twelve_rows    = "nrow(.) == 12"
#' ))
#' }
#' @export
validate_table <- function(sess, table, rules, strict = FALSE) {
  if (!requireNamespace("validate", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg validate} is required.",
      "i" = "Install with: {.code install.packages('validate')}"
    ))
  }

  df <- get_table(sess, table)

  # ── Build validator ────────────────────────────────────────────────────────
  if (inherits(rules, "validator")) {
    v          <- rules
    rules_char <- vapply(
      validate::rules(v),
      function(r) paste(deparse(r@expr), collapse = ""),
      character(1)
    )
  } else {
    rules_char <- unlist(rules)
    rule_exprs <- lapply(rules_char, function(r) {
      tryCatch(
        parse(text = r)[[1]],
        error = function(e) cli::cli_abort(
          "Invalid rule expression: {.code {r}} — {conditionMessage(e)}"
        )
      )
    })
    names(rule_exprs) <- names(rules_char)
    v <- do.call(validate::validator, rule_exprs)
  }

  # ── Confront ───────────────────────────────────────────────────────────────
  cf      <- validate::confront(df, v)
  summ    <- as.data.frame(validate::summary(cf))
  failed  <- summ$name[summ$fails > 0 | isTRUE(summ$error)]
  n_pass  <- nrow(summ) - length(failed)

  # ── Store result ───────────────────────────────────────────────────────────
  if (is.null(sess$validations)) sess$validations <- list()
  sess$validations[[table]] <- list(
    confrontation = cf,
    summary       = summ,
    failed        = failed,
    passed        = setdiff(summ$name, failed)
  )

  # ── Report ─────────────────────────────────────────────────────────────────
  if (length(failed) == 0L) {
    cli::cli_inform(c(
      "v" = "All {n_pass} validation rule{?s} passed for {.val {table}}"
    ))
  } else {
    msg <- c(
      "!" = "{length(failed)} rule{?s} failed for {.val {table}}: {.val {failed}}",
      "i" = "Inspect with {.code sess$validations[['{table}']]}."
    )
    if (strict) cli::cli_abort(msg) else cli::cli_warn(msg)
  }

  record_step(sess, list(
    step   = "validate_table",
    table  = table,
    rules  = as.list(rules_char),
    strict = strict
  ))

  invisible(sess)
}


#' Show validation results for all tables
#'
#' Prints a summary of every `validate_table()` result stored in the session.
#'
#' @param sess A `pdfmacro_session` object.
#' @return `sess` invisibly. # not recorded
#' @export
show_validations <- function(sess) {
  if (is.null(sess$validations) || length(sess$validations) == 0L) {
    cli::cli_inform("No validations run yet. Use {.fn validate_table} first.")
    return(invisible(sess))
  }
  for (lbl in names(sess$validations)) {
    v <- sess$validations[[lbl]]
    n_pass <- length(v$passed)
    n_fail <- length(v$failed)
    cli::cli_rule(left = paste0("Table: ", lbl))
    cli::cli_inform(c(
      "v" = "{n_pass} passed",
      if (n_fail > 0) c("!" = "{n_fail} failed: {.val {v$failed}}") else character(0)
    ))
    print(v$summary[, c("name", "items", "passes", "fails", "nNA")])
    cat("\n")
  }
  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  expect_table_snapshot()                                                     #
# --------------------------------------------------------------------------- #

#' Snapshot-test an extracted table
#'
#' Wraps [testthat::expect_snapshot()] to lock in the printed representation of
#' a table stored in a session. On first run the snapshot is written to
#' `tests/testthat/_snaps/`; subsequent runs compare against that baseline.
#'
#' Must be called inside a `testthat` test (i.e., within `test_that()`).
#'
#' @param sess A `pdfmacro_session` object, or a plain data frame.
#' @param label Character label of the table to snapshot. Ignored when `sess`
#'   is already a data frame.
#' @param variant Optional variant string passed to
#'   [testthat::expect_snapshot()], allowing multiple snapshots per test.
#' @return The result of `expect_snapshot()`, invisibly.
#' @export
expect_table_snapshot <- function(sess, label = NULL, variant = NULL) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg testthat} is required.",
      "i" = "Install with: {.code install.packages('testthat')}"
    ))
  }
  df <- if (is.data.frame(sess)) sess else get_table(sess, label)
  testthat::expect_snapshot(df, variant = variant)
}
