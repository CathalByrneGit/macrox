# tests/testthat/test-diff.R

# All tests mock pdf_replay() to avoid real PDFs.

make_tables <- function(...) {
  args <- list(...)
  setNames(
    lapply(args, function(x) as.data.frame(x, stringsAsFactors = FALSE)),
    names(args)
  )
}

# --------------------------------------------------------------------------- #
#  diff_replay() — status classification                                      #
# --------------------------------------------------------------------------- #

test_that("diff_replay() marks unchanged tables as 'unchanged'", {
  df <- data.frame(a = 1:3, b = c("x","y","z"), stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    pdf_replay = function(file, ...) list(tbl = df)
  )
  result <- diff_replay("f1.pdf", "f2.pdf", macro = list())
  expect_equal(result$tables$tbl$status, "unchanged")
})

test_that("diff_replay() detects row count change as 'changed'", {
  df1 <- data.frame(a = 1:3, stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1:5, stringsAsFactors = FALSE)
  call_n <- 0L
  testthat::local_mocked_bindings(
    pdf_replay = function(file, ...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) list(tbl = df1) else list(tbl = df2)
    }
  )
  result <- diff_replay("f1.pdf", "f2.pdf", macro = list())
  expect_equal(result$tables$tbl$status, "changed")
  expect_equal(result$tables$tbl$rows1, 3L)
  expect_equal(result$tables$tbl$rows2, 5L)
})

test_that("diff_replay() detects cell-level changes", {
  df1 <- data.frame(a = 1:3, b = c("x","y","z"), stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1:3, b = c("x","Y","z"), stringsAsFactors = FALSE)  # Y changed
  call_n <- 0L
  testthat::local_mocked_bindings(
    pdf_replay = function(file, ...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) list(tbl = df1) else list(tbl = df2)
    }
  )
  result <- diff_replay("f1.pdf", "f2.pdf", macro = list())
  expect_equal(result$tables$tbl$status, "changed")
  expect_equal(result$tables$tbl$cell_changes, 1L)
})

test_that("diff_replay() marks new table as 'added'", {
  df <- data.frame(a = 1:2, stringsAsFactors = FALSE)
  call_n <- 0L
  testthat::local_mocked_bindings(
    pdf_replay = function(file, ...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) list()           # file1 has no tables
      else              list(new_tbl = df) # file2 has one
    }
  )
  result <- diff_replay("f1.pdf", "f2.pdf", macro = list())
  expect_equal(result$tables$new_tbl$status, "added")
  expect_true(is.na(result$tables$new_tbl$rows1))
  expect_equal(result$tables$new_tbl$rows2, 2L)
})

test_that("diff_replay() marks removed table as 'removed'", {
  df <- data.frame(a = 1:2, stringsAsFactors = FALSE)
  call_n <- 0L
  testthat::local_mocked_bindings(
    pdf_replay = function(file, ...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) list(old_tbl = df)
      else              list()
    }
  )
  result <- diff_replay("f1.pdf", "f2.pdf", macro = list())
  expect_equal(result$tables$old_tbl$status, "removed")
  expect_equal(result$tables$old_tbl$rows1, 2L)
  expect_true(is.na(result$tables$old_tbl$rows2))
})

test_that("diff_replay() detects column additions", {
  df1 <- data.frame(a = 1:2, stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1:2, b = 3:4, stringsAsFactors = FALSE)
  call_n <- 0L
  testthat::local_mocked_bindings(
    pdf_replay = function(file, ...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) list(t = df1) else list(t = df2)
    }
  )
  result <- diff_replay("f1.pdf", "f2.pdf", macro = list())
  expect_true(result$tables$t$col_changed)
  expect_equal(result$tables$t$new_cols, "b")
})

# --------------------------------------------------------------------------- #
#  diff_replay() — return structure                                            #
# --------------------------------------------------------------------------- #

test_that("diff_replay() returns a pdfmacro_diff object", {
  testthat::local_mocked_bindings(
    pdf_replay = function(file, ...) list()
  )
  result <- diff_replay("f1.pdf", "f2.pdf", macro = list())
  expect_s3_class(result, "pdfmacro_diff")
  expect_equal(result$file1, "f1.pdf")
  expect_equal(result$file2, "f2.pdf")
})

# --------------------------------------------------------------------------- #
#  print.pdfmacro_diff()                                                      #
# --------------------------------------------------------------------------- #

test_that("print.pdfmacro_diff() shows file names and table statuses", {
  diff_obj <- structure(
    list(
      file1 = "old.pdf",
      file2 = "new.pdf",
      tables = list(
        t1 = list(status = "unchanged", rows1 = 5L, rows2 = 5L,
                  cols1 = 3L, cols2 = 3L, col_changed = FALSE,
                  cell_changes = 0L, new_cols = character(0),
                  dropped_cols = character(0)),
        t2 = list(status = "changed", rows1 = 10L, rows2 = 12L,
                  cols1 = 3L, cols2 = 4L, col_changed = TRUE,
                  cell_changes = 2L, new_cols = "d",
                  dropped_cols = character(0))
      )
    ),
    class = "pdfmacro_diff"
  )
  out <- capture.output(print(diff_obj))
  expect_true(any(grepl("old.pdf", out)))
  expect_true(any(grepl("new.pdf", out)))
  expect_true(any(grepl("unchanged", out)))
  expect_true(any(grepl("changed", out)))
  expect_true(any(grepl("rows:", out)))
})
