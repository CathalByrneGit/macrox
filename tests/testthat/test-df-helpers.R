test_that("split_column_df() splits on whitespace and keeps column order", {
  df  <- data.frame(county = c("Carlow", "Cavan"),
                    merged = c("10,443 21,997 3,753", "28,788 59,603 6,300"),
                    other  = 1:2, stringsAsFactors = FALSE)
  out <- split_column_df(df, "merged", c("male", "female", "total"))
  expect_equal(names(out), c("county", "male", "female", "total", "other"))
  expect_equal(out$female, c("21,997", "59,603"))
  expect_equal(out$other, 1:2)
})

test_that("split_column_df() pads short cells with NA and truncates long ones", {
  df  <- data.frame(x = c("a b", "a b c d"), stringsAsFactors = FALSE)
  out <- split_column_df(df, "x", c("p", "q", "r"))
  expect_true(is.na(out$r[1]))
  expect_equal(out$r[2], "c")
})

test_that("split_column_df() can keep the original column", {
  df  <- data.frame(x = "a b", stringsAsFactors = FALSE)
  out <- split_column_df(df, "x", c("p", "q"), keep = TRUE)
  expect_equal(names(out), c("x", "p", "q"))
})

test_that("split_column_df() uses a custom separator", {
  df  <- data.frame(x = "a|b", stringsAsFactors = FALSE)
  out <- split_column_df(df, "x", c("p", "q"), sep = "\\|")
  expect_equal(c(out$p, out$q), c("a", "b"))
})

test_that("split_column_df() validates its arguments", {
  df <- data.frame(x = "a b", stringsAsFactors = FALSE)
  expect_error(split_column_df(df, "zzz", c("p", "q")), "not found")
  expect_error(split_column_df(df, "x", "p"), "at least 2")
})

test_that("split_column() on a session matches split_column_df()", {
  df   <- data.frame(x = c("1 2", "3 4"), stringsAsFactors = FALSE)
  sess <- make_sess(df = df)
  suppressMessages(split_column(sess, "tbl", "x", c("a", "b")))
  expect_equal(sess$tables$tbl, split_column_df(df, "x", c("a", "b")))
  expect_equal(sess$steps[[1]]$step, "split_column")
})

test_that("clean_numbers_df() matches clean_numbers() on a session", {
  df   <- data.frame(price = c("£100", "$2,000", "(50)", "n/a"),
                     name  = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  sess <- make_sess(df = df)
  suppressMessages(clean_numbers(sess, "tbl", cols = "price"))
  out <- clean_numbers_df(df, cols = "price")
  expect_equal(out$price, c(100, 2000, -50, NA))
  expect_equal(out, sess$tables$tbl)
  expect_equal(out$name, df$name)
})

test_that("clean_numbers_df() defaults to the non-numeric columns", {
  df  <- data.frame(num = 1:2, txt = c("1,000", "2,000"), stringsAsFactors = FALSE)
  out <- clean_numbers_df(df)
  expect_type(out$num, "integer")
  expect_equal(out$txt, c(1000, 2000))
})

test_that("clean_numbers_df() warns about missing columns", {
  df <- data.frame(a = "1", stringsAsFactors = FALSE)
  expect_warning(clean_numbers_df(df, cols = c("a", "zzz")), "not found")
})

test_that("pdf_table() reads a table from a PDF", {
  skip_if_not_installed("pdftools")
  pdf <- test_path("..", "..", "AIM_Stats_Report_060525.pdf")
  skip_if_not(file.exists(pdf), "sample PDF not available")
  df <- pdf_table(pdf, page = 9, header_rows = 4)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 20L)
  expect_gt(ncol(df), 5L)
})

test_that("pdf_table() fails clearly on an out-of-range page", {
  skip_if_not_installed("pdftools")
  pdf <- test_path("..", "..", "AIM_Stats_Report_060525.pdf")
  skip_if_not(file.exists(pdf), "sample PDF not available")
  expect_error(pdf_table(pdf, page = 9999), "exceeds")
})
