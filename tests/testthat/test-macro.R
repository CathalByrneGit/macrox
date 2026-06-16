make_sess_with_steps <- function() {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(
    tbl = data.frame(Month = c("Jan","Feb"), Val = c("1,000","2,000"),
                     stringsAsFactors = FALSE)
  )
  sess$steps <- list(
    list(step = "rename_columns", table = "tbl",
         mapping = list(Month = "month", Val = "val")),
    list(step = "cast_types",     table = "tbl",
         types   = list(val = "integer")),
    list(step = "filter_rows",    table = "tbl",
         exclude_where = "month == 'Total'")
  )
  class(sess) <- "pdfmacro_session"
  sess
}

test_that("save_macro() writes valid YAML", {
  sess    <- make_sess_with_steps()
  tmp_dir <- tempdir()
  out     <- save_macro(sess, "test_macro", path = tmp_dir, overwrite = TRUE)
  expect_true(file.exists(out))
  content <- yaml::read_yaml(out)
  expect_equal(content$macro$name, "test_macro")
  expect_length(content$steps, 3L)
})

test_that("load_macro() reads back the step list", {
  sess    <- make_sess_with_steps()
  tmp_dir <- tempdir()
  save_macro(sess, "test_macro2", path = tmp_dir, overwrite = TRUE)
  steps <- load_macro("test_macro2", path = tmp_dir)
  expect_type(steps, "list")
  expect_length(steps, 3L)
})

test_that("load_macro() errors on missing file", {
  expect_error(load_macro("no_such_macro_xyz", path = tempdir()), "not found")
})

test_that(".dispatch_step() calls correct function for rename_columns", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(
    tbl = data.frame(OldName = 1:3, stringsAsFactors = FALSE)
  )
  sess$steps  <- list()
  class(sess) <- "pdfmacro_session"

  step <- list(step = "rename_columns", table = "tbl", mapping = list(OldName = "new_name"))
  pdfmacro:::.dispatch_step(sess, step)
  expect_equal(names(sess$tables$tbl), "new_name")
})

test_that(".dispatch_step() calls correct function for filter_rows", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(
    tbl = data.frame(x = c(1, 2, 99), stringsAsFactors = FALSE)
  )
  sess$steps  <- list()
  class(sess) <- "pdfmacro_session"

  step <- list(step = "filter_rows", table = "tbl", exclude_where = "x == 99")
  pdfmacro:::.dispatch_step(sess, step)
  expect_equal(nrow(sess$tables$tbl), 2L)
  expect_false(99 %in% sess$tables$tbl$x)
})

test_that(".dispatch_step() errors on unknown step type", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "pdfmacro_session"

  expect_error(
    pdfmacro:::.dispatch_step(sess, list(step = "nonexistent_step")),
    "Unknown step type"
  )
})

test_that("save/load round-trip preserves all step fields", {
  sess    <- make_sess_with_steps()
  tmp_dir <- tempdir()
  save_macro(sess, "roundtrip_test", path = tmp_dir, overwrite = TRUE)
  steps <- load_macro("roundtrip_test", path = tmp_dir)

  expect_equal(steps[[1]]$step,    "rename_columns")
  expect_equal(steps[[1]]$table,   "tbl")
  expect_equal(steps[[2]]$step,    "cast_types")
  expect_equal(steps[[2]]$types$val, "integer")
  expect_equal(steps[[3]]$step,    "filter_rows")
  expect_equal(steps[[3]]$exclude_where, "month == 'Total'")
})

# ── Dispatch coverage for newer step types ───────────────────────────────────

.make_dispatch_sess <- function(df = NULL) {
  s <- new.env(parent = emptyenv())
  s$path   <- "dummy.pdf"
  s$tables <- list(tbl = if (!is.null(df)) df else
    data.frame(a = 1:3, b = c("x","y","z"), stringsAsFactors = FALSE))
  s$items  <- list()
  s$steps  <- list()
  class(s) <- "pdfmacro_session"
  s
}

test_that(".dispatch_step() handles add_column", {
  sess <- .make_dispatch_sess()
  pdfmacro:::.dispatch_step(sess,
    list(step = "add_column", table = "tbl", name = "flag", expr = "TRUE"))
  expect_true("flag" %in% names(sess$tables$tbl))
})

test_that(".dispatch_step() handles stack_tables", {
  sess <- .make_dispatch_sess()
  sess$tables$tbl2 <- data.frame(a = 4:5, b = c("p","q"),
                                  stringsAsFactors = FALSE)
  pdfmacro:::.dispatch_step(sess,
    list(step = "stack_tables", label = "combined",
         tables = list("tbl", "tbl2"), .fill = FALSE))
  expect_equal(nrow(sess$tables$combined), 5L)
})

test_that(".dispatch_step() handles merge_tables", {
  sess <- .make_dispatch_sess()
  sess$tables$right <- data.frame(a = 1:3, score = 10:12,
                                   stringsAsFactors = FALSE)
  pdfmacro:::.dispatch_step(sess,
    list(step = "merge_tables", label = "merged",
         left = "tbl", right = "right", by = list("a"),
         all = FALSE, all.x = FALSE, all.y = FALSE))
  expect_true("merged" %in% names(sess$tables))
  expect_true("score" %in% names(sess$tables$merged))
})

test_that(".dispatch_step() handles validate_table without validate pkg gracefully", {
  skip_if(requireNamespace("validate", quietly = TRUE),
          "validate installed — skipping absence test")
  sess <- .make_dispatch_sess()
  expect_error(
    pdfmacro:::.dispatch_step(sess,
      list(step = "validate_table", table = "tbl",
           rules = list(r = "nrow(.) > 0"), strict = FALSE)),
    "validate"
  )
})

test_that(".dispatch_step() handles validate_table when validate available", {
  skip_if_not_installed("validate")
  sess <- .make_dispatch_sess()
  expect_no_error(
    pdfmacro:::.dispatch_step(sess,
      list(step = "validate_table", table = "tbl",
           rules = list(has_rows = "nrow(.) > 0"), strict = FALSE))
  )
})

test_that(".dispatch_step() handles select_item via mocked LLM", {
  sess <- .make_dispatch_sess()
  testthat::local_mocked_bindings(
    pdf_text      = function(...) list("Ref: ABC-001"),
    .package      = "pdftools"
  )
  mock_chat <- list(chat_structured = function(...) list(value = "ABC-001"))
  testthat::local_mocked_bindings(
    .make_llm_chat = function(...) mock_chat,
    .check_ellmer  = function(...) invisible(NULL)
  )
  pdfmacro:::.dispatch_step(sess,
    list(step = "select_item", label = "ref",
         prompt = "Extract reference.", cast = "character",
         page = NULL, area = NULL, provider = "anthropic",
         model = NULL, base_url = NULL, dpi = 120L))
  expect_true("ref" %in% names(sess$items))
})

# ── pdf_replay() end-to-end with mixed step types ────────────────────────────

test_that("pdf_replay() replays add_column and stack_tables steps", {
  df1 <- data.frame(month = month.abb[1:3], n = 1:3L, stringsAsFactors = FALSE)
  df2 <- data.frame(month = month.abb[4:6], n = 4:6L, stringsAsFactors = FALSE)

  call_n <- 0L
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      call_n <<- call_n + 1L
      sess$tables[[label]] <- if (call_n == 1L) df1 else df2
      invisible(sess)
    },
    # Mock pdf_session so no real file is needed
    pdf_session = function(path) {
      s <- new.env(parent = emptyenv())
      s$path <- path; s$tables <- list(); s$items <- list()
      s$steps <- list(); s$.replaying <- FALSE
      class(s) <- "pdfmacro_session"
      s
    }
  )

  steps <- list(
    list(step = "select_table", label = "t1", page = 1L,
         method = "bbox", area = NULL, table_index = 1L,
         label_match = NULL, header_rows = 1L),
    list(step = "select_table", label = "t2", page = 2L,
         method = "bbox", area = NULL, table_index = 1L,
         label_match = NULL, header_rows = 1L),
    list(step = "add_column", table = "t1", name = "year", expr = "2024L"),
    list(step = "stack_tables", label = "all",
         tables = list("t1", "t2"), .fill = TRUE)
  )

  result <- pdf_replay("dummy.pdf", macro = steps)
  expect_true("all" %in% names(result))
  expect_equal(nrow(result$all), 6L)
  expect_true("year" %in% names(result$t1))
})
