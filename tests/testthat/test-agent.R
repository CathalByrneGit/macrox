# tests/testthat/test-agent.R
# Tests for the R-Core Judge (test_extraction) and agent helper functions.
# All tests avoid real PDFs by mocking the extraction functions.

# ── Helper: minimal session-like env ────────────────────────────────────────
.make_agent_sess <- function(df = NULL, label = "target") {
  s <- new.env(parent = emptyenv())
  s$path   <- "dummy.pdf"
  s$tables <- if (!is.null(df)) setNames(list(df), label) else list()
  s$items  <- list()
  s$steps  <- list()
  s$.replaying <- TRUE
  class(s) <- "macrox_session"
  s
}

# --------------------------------------------------------------------------- #
#  test_extraction() — status codes                                            #
# --------------------------------------------------------------------------- #

test_that("test_extraction() returns 'success' for a clean data frame", {
  clean_df <- data.frame(
    Month  = c("January", "February"),
    Male   = c("100", "200"),
    Female = c("90", "180"),
    stringsAsFactors = FALSE
  )
  # Mock select_table to populate the temp session
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      sess$tables[[label]] <- clean_df
      invisible(sess)
    }
  )
  result <- test_extraction("dummy.pdf", page = 1L, method = "bbox")
  expect_equal(result$status, "success")
  expect_equal(result$metrics$rows_extracted, 2L)
  expect_equal(result$metrics$cols_extracted, 3L)
  expect_length(result$metrics$unnamed_headers, 0L)
  expect_false(result$metrics$header_spill)
})

test_that("test_extraction() detects header spill", {
  spill_df <- data.frame(
    V1 = c("Description", "Steel Bracket", "Bolt"),
    V2 = c("Amount",      "14.99",         "2.50"),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      sess$tables[[label]] <- spill_df; invisible(sess)
    }
  )
  result <- test_extraction("dummy.pdf", page = 1L, method = "bbox")
  expect_equal(result$status, "needs_alignment")
  expect_true(result$metrics$header_spill)
  expect_match(result$guidance, "header_rows")
})

test_that("test_extraction() detects unnamed columns", {
  df <- data.frame(
    V1 = c("Jan", "Feb"),
    V2 = c("100", "200"),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      sess$tables[[label]] <- df; invisible(sess)
    }
  )
  result <- test_extraction("dummy.pdf", page = 1L, method = "bbox")
  expect_equal(result$status, "needs_alignment")
  expect_true("V1" %in% unlist(result$metrics$unnamed_headers))
  expect_true("V2" %in% unlist(result$metrics$unnamed_headers))
  expect_match(result$guidance, "area coords")
})

test_that("test_extraction() detects empty columns", {
  df <- data.frame(
    Month  = c("Jan", "Feb"),
    Empty  = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      sess$tables[[label]] <- df; invisible(sess)
    }
  )
  result <- test_extraction("dummy.pdf", page = 1L, method = "bbox")
  expect_equal(result$status, "needs_alignment")
  expect_true("Empty" %in% unlist(result$metrics$empty_columns))
  expect_match(result$guidance, "empty")
})

test_that("test_extraction() flags likely-numeric character columns", {
  df <- data.frame(
    Month  = c("Jan", "Feb", "Mar"),
    Amount = c("1,234", "5,678", "9,012"),   # numeric but stored as character
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      sess$tables[[label]] <- df; invisible(sess)
    }
  )
  result <- test_extraction("dummy.pdf", page = 1L, method = "bbox")
  # May still be "success" (no structural problems) but numeric cols flagged
  expect_true("Amount" %in% unlist(result$metrics$likely_numeric_cols))
})

test_that("test_extraction() returns 'crash' on extraction error", {
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      stop("bbox returned no data. Adjust area or tuning.")
    }
  )
  result <- test_extraction("dummy.pdf", page = 1L, method = "bbox")
  expect_equal(result$status, "crash")
  expect_match(result$message, "bbox returned no data")
  expect_false(is.null(result$guidance))
})

test_that("test_extraction() returns 'empty' when no rows", {
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      sess$tables[[label]] <- data.frame(); invisible(sess)
    }
  )
  result <- test_extraction("dummy.pdf", page = 1L, method = "bbox")
  expect_equal(result$status, "empty")
})

# --------------------------------------------------------------------------- #
#  test_extraction() — preview and metrics structure                           #
# --------------------------------------------------------------------------- #

test_that("test_extraction() preview respects preview_rows", {
  df <- data.frame(
    A = letters[1:10], B = 1:10, stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      sess$tables[[label]] <- df; invisible(sess)
    }
  )
  result <- test_extraction("dummy.pdf", page = 1L, method = "bbox",
                             preview_rows = 3L)
  expect_length(result$preview, 3L)
})

test_that("test_extraction() preview is JSON-safe (list of named lists)", {
  df <- data.frame(Month = "Jan", Total = 100L, stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      sess$tables[[label]] <- df; invisible(sess)
    }
  )
  result <- test_extraction("dummy.pdf", page = 1L, method = "bbox")
  # Each preview row should be a named list (JSON object)
  expect_true(is.list(result$preview[[1]]))
  expect_named(result$preview[[1]])
})

test_that("test_extraction() result serialises to valid JSON", {
  skip_if_not_installed("jsonlite")
  df <- data.frame(Month = "Jan", Total = "100", stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      sess$tables[[label]] <- df; invisible(sess)
    }
  )
  result <- test_extraction("dummy.pdf", page = 1L, method = "bbox")
  json_str <- jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE)
  # Should round-trip cleanly
  expect_no_error(jsonlite::fromJSON(json_str))
})

# --------------------------------------------------------------------------- #
#  .extraction_guidance() — guidance message content                          #
# --------------------------------------------------------------------------- #

test_that(".extraction_guidance() mentions header_rows when spill detected", {
  msg <- macrox:::.extraction_guidance(
    unnamed_cols = character(0),
    empty_cols   = character(0),
    header_spill = TRUE,
    likely_numeric = character(0)
  )
  expect_match(msg, "header_rows")
})

test_that(".extraction_guidance() mentions area when unnamed columns present", {
  msg <- macrox:::.extraction_guidance(
    unnamed_cols   = c("V1", "V2"),
    empty_cols     = character(0),
    header_spill   = FALSE,
    likely_numeric = character(0)
  )
  expect_match(msg, "area")
  expect_match(msg, "V1")
})

test_that(".extraction_guidance() combines multiple issues", {
  msg <- macrox:::.extraction_guidance(
    unnamed_cols   = "V1",
    empty_cols     = "Empty",
    header_spill   = TRUE,
    likely_numeric = character(0)
  )
  # Should contain all three issues separated by |
  expect_match(msg, "\\|")
  expect_match(msg, "header_rows")
  expect_match(msg, "area")
  expect_match(msg, "empty")
})

# --------------------------------------------------------------------------- #
#  Agent loop simulation                                                       #
# --------------------------------------------------------------------------- #

test_that("agent loop converges from needs_alignment to success", {
  # Simulate: turn 1 has unnamed cols, turn 2 is clean after header_rows=2
  call_count <- 0L
  testthat::local_mocked_bindings(
    select_table = function(sess, label, header_rows = 1L, ...) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        # First call: header spilled into data
        sess$tables[[label]] <- data.frame(
          V1 = c("Description", "Steel Bracket"),
          V2 = c("Amount", "14.99"),
          stringsAsFactors = FALSE
        )
      } else {
        # Second call (header_rows = 2): clean
        sess$tables[[label]] <- data.frame(
          Description = "Steel Bracket",
          Amount      = "14.99",
          stringsAsFactors = FALSE
        )
      }
      invisible(sess)
    }
  )

  # Turn 1
  r1 <- test_extraction("f.pdf", page = 1L, method = "bbox", header_rows = 1L)
  expect_equal(r1$status, "needs_alignment")
  expect_match(r1$guidance, "header_rows")

  # Turn 2 — agent increased header_rows based on guidance
  r2 <- test_extraction("f.pdf", page = 1L, method = "bbox", header_rows = 2L)
  expect_equal(r2$status, "success")
  expect_equal(call_count, 2L)
})
