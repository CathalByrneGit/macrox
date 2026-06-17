# tests/testthat/test-docling.R

# --------------------------------------------------------------------------- #
#  Docling backend unit tests                                                  #
# --------------------------------------------------------------------------- #

test_that(".docling_tbl_to_df() converts headers + data to a data frame", {
  tbl <- list(
    headers = list("Name", "Age", "City"),
    data    = list(
      list("Alice", "30", "Dublin"),
      list("Bob",   "25", "Cork")
    )
  )
  df <- macrox:::.docling_tbl_to_df(tbl)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_equal(ncol(df), 3L)
  expect_equal(names(df), c("Name", "Age", "City"))
  expect_equal(df$Name, c("Alice", "Bob"))
})

test_that(".docling_tbl_to_df() makes names valid R identifiers", {
  tbl <- list(
    headers = list("Column 1", "Column 1"),
    data    = list(list("a", "b"))
  )
  df <- macrox:::.docling_tbl_to_df(tbl)
  expect_equal(names(df), c("Column.1", "Column.1.1"))
})

test_that(".docling_tbl_to_df() handles single-row data", {
  tbl <- list(
    headers = list("X", "Y"),
    data    = list(list("1", "2"))
  )
  df <- macrox:::.docling_tbl_to_df(tbl)
  expect_equal(nrow(df), 1L)
  expect_equal(df$X, "1")
})

test_that(".docling_tbl_to_df() pads short rows with empty strings", {
  tbl <- list(
    headers = list("A", "B", "C"),
    data    = list(
      list("1", "2"),
      list("3", "4", "5")
    )
  )
  df <- macrox:::.docling_tbl_to_df(tbl)
  expect_equal(ncol(df), 3L)
  expect_equal(nrow(df), 2L)
  expect_equal(df$C[1], "")
  expect_equal(df$C[2], "5")
})

test_that("close_docling() prints message when helpers not loaded", {
  msgs <- capture.output(close_docling(), type = "message")
  expect_true(any(grepl("not currently loaded", msgs)))
})
