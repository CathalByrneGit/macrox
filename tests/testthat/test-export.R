# tests/testthat/test-export.R

make_export_sess <- function() {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(
    monthly = data.frame(
      month = month.abb[1:3],
      count = 1:3L,
      stringsAsFactors = FALSE
    ),
    annual = data.frame(
      year  = 2022:2024,
      total = c(100L, 200L, 300L),
      stringsAsFactors = FALSE
    )
  )
  sess$steps <- list()
  class(sess) <- "macrox_session"
  sess
}

# --------------------------------------------------------------------------- #
#  export_csv()                                                                #
# --------------------------------------------------------------------------- #

test_that("export_csv() writes one CSV per table", {
  sess <- make_export_sess()
  tmp  <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  paths <- export_csv(sess, dir = tmp)

  expect_length(paths, 2L)
  expect_true(file.exists(file.path(tmp, "monthly.csv")))
  expect_true(file.exists(file.path(tmp, "annual.csv")))
})

test_that("export_csv() CSV content matches the table", {
  sess <- make_export_sess()
  tmp  <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  export_csv(sess, dir = tmp)
  df_back <- utils::read.csv(file.path(tmp, "monthly.csv"),
                              stringsAsFactors = FALSE)
  expect_equal(nrow(df_back), 3L)
  expect_equal(names(df_back), c("month", "count"))
})

test_that("export_csv() respects the tables argument", {
  sess <- make_export_sess()
  tmp  <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  export_csv(sess, dir = tmp, tables = "monthly")
  expect_true( file.exists(file.path(tmp, "monthly.csv")))
  expect_false(file.exists(file.path(tmp, "annual.csv")))
})

test_that("export_csv() creates dir if it doesn't exist", {
  sess <- make_export_sess()
  tmp  <- file.path(tempdir(), paste0("macrox_test_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE))

  expect_false(dir.exists(tmp))
  export_csv(sess, dir = tmp)
  expect_true(dir.exists(tmp))
})

test_that("export_csv() errors when no tables extracted", {
  sess <- make_export_sess()
  sess$tables <- list()
  expect_error(export_csv(sess, dir = tempdir()), "No tables")
})

test_that("export_csv() returns paths invisibly", {
  sess <- make_export_sess()
  tmp  <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  paths <- export_csv(sess, dir = tmp)
  expect_type(paths, "character")
  expect_length(paths, 2L)
})

# --------------------------------------------------------------------------- #
#  export_excel()                                                              #
# --------------------------------------------------------------------------- #

test_that("export_excel() creates an xlsx file", {
  skip_if_not_installed("writexl")
  sess <- make_export_sess()
  tmp  <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  export_excel(sess, path = tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0L)
})

test_that("export_excel() errors when no tables extracted", {
  sess <- make_export_sess()
  sess$tables <- list()
  expect_error(export_excel(sess), "No tables")
})

test_that("export_excel() respects the tables argument", {
  skip_if_not_installed("writexl")
  sess <- make_export_sess()
  tmp  <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  export_excel(sess, path = tmp, tables = "monthly")
  expect_true(file.exists(tmp))
})

test_that("export_excel() returns path invisibly", {
  skip_if_not_installed("writexl")
  sess <- make_export_sess()
  tmp  <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  result <- export_excel(sess, path = tmp)
  expect_equal(result, tmp)
})
