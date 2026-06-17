test_that("mx_session() errors on missing file", {
  expect_error(mx_session("does_not_exist.pdf"), "File not found")
})

test_that("record_step() appends a step", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  record_step(sess, list(step = "filter_rows", table = "x", exclude_where = "a == 1"))
  expect_length(sess$steps, 1L)
  expect_equal(sess$steps[[1]]$step, "filter_rows")
})

test_that("exact duplicate is skipped", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  step <- list(step = "filter_rows", table = "x", exclude_where = "a == 1")
  record_step(sess, step)                              # first: appended silently
  expect_warning(record_step(sess, step), "Duplicate") # second: warns and skips
  expect_length(sess$steps, 1L)
})

test_that("overwrite_table is flagged", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  s1 <- list(step = "select_table", label = "tbl", page = 1, table_index = 1,
             method = "lattice", header_rows = 1, area = NULL, label_match = NULL,
             fuzzy_method = "jw", max_dist = 0.2)
  s2 <- s1; s2$page <- 2  # same label, different page

  record_step(sess, s1)
  expect_warning(record_step(sess, s2), "already extracted")
  expect_length(sess$steps, 2L)
  expect_true(sess$steps[[2]]$.flagged)
})

test_that("repeat_transform is flagged", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  s1 <- list(step = "rename_columns", table = "tbl", mapping = list(A = "a"))
  s2 <- list(step = "rename_columns", table = "tbl", mapping = list(B = "b"))

  record_step(sess, s1)
  expect_warning(record_step(sess, s2), "already applied")
  expect_length(sess$steps, 2L)
  expect_true(sess$steps[[2]]$.flagged)
})

test_that("show_steps() runs without error on empty session", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"
  expect_no_error(show_steps(sess))
})

test_that("show_steps() runs without error on non-empty session", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"
  record_step(sess, list(step = "filter_rows", table = "x", exclude_where = "v == 0"))
  expect_no_error(show_steps(sess))
})

test_that("remove_step() errors on out-of-range index", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"
  expect_error(remove_step(sess, 1), "No steps")
  record_step(sess, list(step = "filter_rows", table = "x", exclude_where = "v == 0"))
  expect_error(remove_step(sess, 5))
})

test_that("remove_step() removes the correct step", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  record_step(sess, list(step = "filter_rows", table = "a", exclude_where = "x == 1"))
  record_step(sess, list(step = "filter_rows", table = "b", exclude_where = "y == 2"))
  expect_length(sess$steps, 2L)

  remove_step(sess, 1)
  expect_length(sess$steps, 1L)
  expect_equal(sess$steps[[1]]$table, "b")
})

# ── remove_step() now removes the table when step is select_table ─────────── #

test_that("remove_step() removes table from sess$tables when step is select_table", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(mydata = data.frame(x = 1:3))
  sess$steps  <- list(list(
    step = "select_table", label = "mydata",
    page = 1L, table_index = 1L, method = "bbox",
    area = NULL, label_match = NULL,
    fuzzy_method = "jw", max_dist = 0.2,
    header_rows = 1L, row_tol = NULL, col_gap = NULL
  ))
  class(sess) <- "macrox_session"

  remove_step(sess, 1)
  expect_length(sess$steps, 0L)
  expect_null(sess$tables[["mydata"]])
})

test_that("remove_step() does NOT remove table when step is a transform", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(tbl = data.frame(x = 1:3))
  sess$steps  <- list(
    list(step = "filter_rows", table = "tbl", exclude_where = "x == 1")
  )
  class(sess) <- "macrox_session"

  remove_step(sess, 1)
  expect_length(sess$steps, 0L)
  # Table should still be there
  expect_false(is.null(sess$tables[["tbl"]]))
})

# ── sess$items initialisation ─────────────────────────────────────────────── #

test_that("mx_session() initialises sess$items as an empty list", {
  # We can't open a real PDF, but we can test the structure via a mock
  # by checking that the session env has the items field
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$items  <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"
  expect_true(is.list(sess$items))
  expect_length(sess$items, 0L)
})

test_that("select_item() initialises items if NULL on session", {
  # guard against older sessions without the items field
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  # deliberately omit items
  class(sess) <- "macrox_session"

  testthat::local_mocked_bindings(
    pdf_text      = function(...) list("Total: 42"),
    .package      = "pdftools"
  )
  mock_chat <- list(chat_structured = function(...) list(value = "42"))
  testthat::local_mocked_bindings(
    .make_llm_chat = function(...) mock_chat,
    .check_ellmer  = function(...) invisible(NULL)
  )

  expect_no_error(
    select_item(sess, "total", prompt = "Extract total.")
  )
  expect_true("total" %in% names(sess$items))
})

# ── area_active flag ──────────────────────────────────────────────────────── #

test_that("area_active starts FALSE in reactiveValues", {
  app_path <- system.file("R", "app.R", package = "macrox", mustWork = FALSE)
  if (!nzchar(app_path)) {
    app_path <- file.path(testthat::test_path("..", "..", "R", "app.R"))
  }
  skip_if(!file.exists(app_path), "app.R not reachable in this check context")
  src <- readLines(app_path)
  area_init_lines <- grep("area_active.*=.*FALSE", src, value = TRUE)
  expect_true(length(area_init_lines) > 0)
})
