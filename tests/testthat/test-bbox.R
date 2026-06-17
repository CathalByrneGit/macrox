# tests/testthat/test-bbox.R

# --------------------------------------------------------------------------- #
#  .bbox_mat_to_df()                                                           #
# --------------------------------------------------------------------------- #

test_that(".bbox_mat_to_df() uses row 1 as column names", {
  mat <- matrix(c("Month", "Male", "Female",
                  "Jan",   "100",  "200",
                  "Feb",   "150",  "250"),
                nrow = 3, byrow = TRUE)
  df <- macrox:::.bbox_mat_to_df(mat, header_rows = 1L)
  expect_equal(nrow(df), 2L)
  expect_equal(ncol(df), 3L)
  expect_true("Month" %in% names(df))
  expect_true("Male"  %in% names(df))
})

test_that(".bbox_mat_to_df() preserves original case — does NOT lowercase", {
  mat <- matrix(c("Breed.of.Dam", "Total",
                  "Friesian",     "1234"),
                nrow = 2, byrow = TRUE)
  df <- macrox:::.bbox_mat_to_df(mat, header_rows = 1L)
  expect_equal(names(df)[[1]], "Breed.of.Dam")
  expect_equal(names(df)[[2]], "Total")
})

test_that(".bbox_mat_to_df() returns empty df for 0-row matrix", {
  mat <- matrix(character(0), nrow = 0, ncol = 3)
  df  <- macrox:::.bbox_mat_to_df(mat, header_rows = 1L)
  expect_true(is.data.frame(df))
})

test_that(".bbox_mat_to_df() handles header_rows = 0 (no header)", {
  mat <- matrix(c("a", "b", "c", "d"), nrow = 2, byrow = TRUE)
  df  <- macrox:::.bbox_mat_to_df(mat, header_rows = 0L)
  expect_equal(nrow(df), 2L)
  # Names should be make.names generics
  expect_true(all(nchar(names(df)) > 0))
})

test_that(".bbox_mat_to_df() flattens multi-row headers with space join", {
  # Two header rows: "Calves born to a Beef Bull" + "Male" / "Female"
  mat <- matrix(
    c("Calves born to a Beef Bull", "Calves born to a Beef Bull",
      "Male",                        "Female",
      "100",                         "200"),
    nrow = 3, byrow = TRUE
  )
  df <- macrox:::.bbox_mat_to_df(mat, header_rows = 2L)
  expect_equal(nrow(df), 1L)
  # Both combined names should contain parts of both rows
  expect_true(any(grepl("Beef.Bull", names(df))))
})

test_that(".bbox_mat_to_df() makes duplicate column names unique", {
  mat <- matrix(c("Total", "Total",
                  "100",   "200"),
                nrow = 2, byrow = TRUE)
  df <- macrox:::.bbox_mat_to_df(mat, header_rows = 1L)
  expect_equal(length(names(df)), 2L)
  expect_false(identical(names(df)[[1]], names(df)[[2]]))
})

# --------------------------------------------------------------------------- #
#  .forward_fill() — already tested in test-select.R but worth a targeted     #
#  test for the bbox-specific usage                                            #
# --------------------------------------------------------------------------- #

test_that(".forward_fill() handles all-empty input", {
  expect_equal(macrox:::.forward_fill(c("", "", "")), c("", "", ""))
})

test_that(".forward_fill() fills trailing empties", {
  expect_equal(
    macrox:::.forward_fill(c("A", "B", "")),
    c("A", "B", "B")
  )
})
