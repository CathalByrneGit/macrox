# tests/testthat/test-validate.R

make_valid_sess <- function(df = NULL) {
  if (is.null(df))
    df <- data.frame(month = month.abb[1:12],
                     value = as.integer(seq(100, 1200, 100)),
                     stringsAsFactors = FALSE)
  sess <- new.env(parent = emptyenv())
  sess$path        <- "dummy.pdf"
  sess$tables      <- list(tbl = df)
  sess$validations <- list()
  sess$steps       <- list()
  class(sess) <- "pdfmacro_session"
  sess
}

# --------------------------------------------------------------------------- #
#  validate_table()                                                            #
# --------------------------------------------------------------------------- #

test_that("validate_table() passes all rules on clean data", {
  skip_if_not_installed("validate")
  sess <- make_valid_sess()
  expect_no_warning(
    validate_table(sess, "tbl", rules = c(
      twelve_rows = "nrow(.) == 12",
      no_na       = "!anyNA(month)"
    ))
  )
  expect_equal(sess$validations[["tbl"]]$failed, character(0))
})

test_that("validate_table() warns when a rule fails", {
  skip_if_not_installed("validate")
  sess <- make_valid_sess()
  expect_warning(
    validate_table(sess, "tbl", rules = c(
      wrong_row_count = "nrow(.) == 5"
    )),
    regexp = "failed"
  )
  expect_equal(unlist(sess$validations[["tbl"]]$failed), "wrong_row_count")
})

test_that("validate_table() aborts on failure when strict = TRUE", {
  skip_if_not_installed("validate")
  sess <- make_valid_sess()
  expect_error(
    validate_table(sess, "tbl",
                   rules = c(bad_rule = "nrow(.) == 0"),
                   strict = TRUE),
    regexp = "failed"
  )
})

test_that("validate_table() stores confrontation result in sess$validations", {
  skip_if_not_installed("validate")
  sess <- make_valid_sess()
  suppressWarnings(validate_table(sess, "tbl", rules = c(
    twelve_rows = "nrow(.) == 12",
    bad_rule    = "nrow(.) == 999"
  )))
  res <- sess$validations[["tbl"]]
  expect_true(!is.null(res$summary))
  expect_equal(nrow(res$summary), 2L)
  expect_true("twelve_rows" %in% res$passed)
  expect_true("bad_rule"    %in% res$failed)
})

test_that("validate_table() records the step", {
  skip_if_not_installed("validate")
  sess <- make_valid_sess()
  validate_table(sess, "tbl", rules = c(r = "nrow(.) == 12"))
  expect_equal(sess$steps[[1]]$step, "validate_table")
  expect_equal(sess$steps[[1]]$table, "tbl")
  expect_equal(as.character(sess$steps[[1]]$rules$r), "nrow(.) == 12")
})

test_that("validate_table() accepts a named character vector", {
  skip_if_not_installed("validate")
  sess  <- make_valid_sess()
  rules <- c(positive = "all(value > 0, na.rm = TRUE)")
  expect_no_warning(validate_table(sess, "tbl", rules = rules))
})

test_that("validate_table() errors when validate package absent", {
  sess <- make_valid_sess()
  # Mock validate as unavailable
  testthat::local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "validate") FALSE else TRUE,
    .package = "base"
  )
  expect_error(
    validate_table(sess, "tbl", rules = c(r = "nrow(.) > 0")),
    "validate"
  )
})

# --------------------------------------------------------------------------- #
#  show_validations()                                                          #
# --------------------------------------------------------------------------- #

test_that("show_validations() informs when no validations run", {
  sess <- make_valid_sess()
  out <- capture.output(show_validations(sess), type = "message")
  expect_true(any(grepl("No validations", out)))
})

test_that("show_validations() prints summary for each table", {
  skip_if_not_installed("validate")
  sess <- make_valid_sess()
  suppressWarnings(validate_table(sess, "tbl", rules = c(
    r1 = "nrow(.) == 12", r2 = "nrow(.) == 0"
  )))
  out <- capture.output(show_validations(sess))
  expect_true(any(grepl("tbl", out)))
  expect_true(any(grepl("passed|failed", tolower(out))))
})

# --------------------------------------------------------------------------- #
#  expect_table_snapshot()                                                     #
# --------------------------------------------------------------------------- #

test_that("expect_table_snapshot() errors when testthat absent", {
  sess <- make_valid_sess()
  testthat::local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "testthat") FALSE else TRUE,
    .package = "base"
  )
  expect_error(expect_table_snapshot(sess, "tbl"), "testthat")
})

test_that("expect_table_snapshot() accepts a plain data frame", {
  df <- data.frame(a = 1:3, stringsAsFactors = FALSE)
  expect_no_error(expect_table_snapshot(df))
})

test_that("expect_table_snapshot() retrieves table from session", {
  sess <- make_valid_sess()
  expect_no_error(expect_table_snapshot(sess, "tbl"))
})
