# tests/testthat/test-detect.R

# --------------------------------------------------------------------------- #
#  detect_tables() filtering — min_rows and max_header_chars                  #
# Tests use local_mocked_bindings to avoid real tabulapdf/pdftools calls.     #
# --------------------------------------------------------------------------- #

.fake_detect_results <- function(path, pages, method, guess, output) {
  # Returns 3 "tables":
  #  1. Good: 5 rows, short first column name
  #  2. Empty: 0 rows, short name  → filtered by min_rows
  #  3. Chart: 3 rows, very long first column name → filtered by max_header_chars
  list(
    data.frame(County = c("Cork","Dublin","Galway","Kerry","Mayo"),
               Total  = c(100L, 200L, 150L, 80L, 90L),
               stringsAsFactors = FALSE),
    data.frame(Month = character(0), stringsAsFactors = FALSE),
    data.frame(
      `X800.000.700.000.600.000.500.000.400.000.300.000` =
        c("a", "b", "c"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  )
}

test_that("detect_tables() shows only rows >= min_rows (default 1)", {
  skip_if_not_installed("tabulapdf")
  sess <- new.env(parent = emptyenv())
  sess$path  <- "dummy.pdf"
  sess$tables <- list(); sess$steps <- list(); sess$text <- NULL
  sess$detect <- NULL
  class(sess) <- "macrox_session"

  testthat::local_mocked_bindings(
    pdf_info           = function(...) list(pages = 1L),
    pdf_text           = function(...) list(""),
    .package = "pdftools"
  )
  testthat::local_mocked_bindings(
    extract_tables = .fake_detect_results,
    .package = "tabulapdf"
  )

  output <- capture.output(detect_tables(sess, pages = 1L, min_rows = 1L), type = "message")
  # The empty table (0 rows) should be skipped
  expect_true(any(grepl("skipped", output)))
  # The good table should appear
  expect_true(any(grepl("County", output)))
  # The empty table should NOT appear in output
  expect_false(any(grepl("0 x", output)))
})

test_that("detect_tables() suppresses chart-like tables via max_header_chars", {
  skip_if_not_installed("tabulapdf")
  sess <- new.env(parent = emptyenv())
  sess$path  <- "dummy.pdf"
  sess$tables <- list(); sess$steps <- list(); sess$text <- NULL
  sess$detect <- NULL
  class(sess) <- "macrox_session"

  testthat::local_mocked_bindings(
    pdf_info           = function(...) list(pages = 1L),
    pdf_text           = function(...) list(""),
    .package = "pdftools"
  )
  testthat::local_mocked_bindings(
    extract_tables = .fake_detect_results,
    .package = "tabulapdf"
  )

  output <- capture.output(detect_tables(sess, pages = 1L, min_rows = 1L, max_header_chars = 40L), type = "message")
  # Long-header chart table should not appear
  expect_false(any(grepl("800.000", output)))
  # Good table should appear
  expect_true(any(grepl("County", output)))
})

test_that("detect_tables() with max_header_chars = Inf shows everything with rows", {
  skip_if_not_installed("tabulapdf")
  sess <- new.env(parent = emptyenv())
  sess$path  <- "dummy.pdf"
  sess$tables <- list(); sess$steps <- list(); sess$text <- NULL
  sess$detect <- NULL
  class(sess) <- "macrox_session"

  testthat::local_mocked_bindings(
    pdf_info           = function(...) list(pages = 1L),
    pdf_text           = function(...) list(""),
    .package = "pdftools"
  )
  testthat::local_mocked_bindings(
    extract_tables = .fake_detect_results,
    .package = "tabulapdf"
  )

  output <- capture.output(detect_tables(sess, pages = 1L, min_rows = 1L, max_header_chars = Inf), type = "message")
  # Chart table should now appear (has 3 rows, header just long)
  expect_true(any(grepl("800.000", output)))
})

test_that("detect_tables() with min_rows = 0 shows empty tables", {
  skip_if_not_installed("tabulapdf")
  sess <- new.env(parent = emptyenv())
  sess$path  <- "dummy.pdf"
  sess$tables <- list(); sess$steps <- list(); sess$text <- NULL
  sess$detect <- NULL
  class(sess) <- "macrox_session"

  testthat::local_mocked_bindings(
    pdf_info           = function(...) list(pages = 1L),
    pdf_text           = function(...) list(""),
    .package = "pdftools"
  )
  testthat::local_mocked_bindings(
    extract_tables = .fake_detect_results,
    .package = "tabulapdf"
  )

  output <- capture.output(detect_tables(sess, pages = 1L, min_rows = 0L, max_header_chars = Inf), type = "message")
  expect_true(any(grepl("0 x", output)))
})

test_that("detect_tables() stores results in sess$detect", {
  skip_if_not_installed("tabulapdf")
  sess <- new.env(parent = emptyenv())
  sess$path  <- "dummy.pdf"
  sess$tables <- list(); sess$steps <- list(); sess$text <- NULL
  sess$detect <- NULL
  class(sess) <- "macrox_session"

  testthat::local_mocked_bindings(
    pdf_info       = function(...) list(pages = 1L),
    pdf_text       = function(...) list(""),
    .package = "pdftools"
  )
  testthat::local_mocked_bindings(
    extract_tables = .fake_detect_results,
    .package = "tabulapdf"
  )

  suppressMessages(detect_tables(sess, pages = 1L))
  expect_true(!is.null(sess$detect))
  expect_true(length(sess$detect) > 0)
})
