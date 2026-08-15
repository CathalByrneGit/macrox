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
  class(sess) <- "macrox_session"
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
  class(sess) <- "macrox_session"

  step <- list(step = "rename_columns", table = "tbl", mapping = list(OldName = "new_name"))
  macrox:::.dispatch_step(sess, step)
  expect_equal(names(sess$tables$tbl), "new_name")
})

test_that(".dispatch_step() calls correct function for filter_rows", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(
    tbl = data.frame(x = c(1, 2, 99), stringsAsFactors = FALSE)
  )
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  step <- list(step = "filter_rows", table = "tbl", exclude_where = "x == 99")
  macrox:::.dispatch_step(sess, step)
  expect_equal(nrow(sess$tables$tbl), 2L)
  expect_false(99 %in% sess$tables$tbl$x)
})

test_that(".dispatch_step() errors on unknown step type", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  expect_error(
    macrox:::.dispatch_step(sess, list(step = "nonexistent_step")),
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
  class(s) <- "macrox_session"
  s
}

test_that(".dispatch_step() handles add_column", {
  sess <- .make_dispatch_sess()
  macrox:::.dispatch_step(sess,
    list(step = "add_column", table = "tbl", name = "flag", expr = "TRUE"))
  expect_true("flag" %in% names(sess$tables$tbl))
})

test_that(".dispatch_step() handles stack_tables", {
  sess <- .make_dispatch_sess()
  sess$tables$tbl2 <- data.frame(a = 4:5, b = c("p","q"),
                                  stringsAsFactors = FALSE)
  macrox:::.dispatch_step(sess,
    list(step = "stack_tables", label = "combined",
         tables = list("tbl", "tbl2"), .fill = FALSE))
  expect_equal(nrow(sess$tables$combined), 5L)
})

test_that(".dispatch_step() handles merge_tables", {
  sess <- .make_dispatch_sess()
  sess$tables$right <- data.frame(a = 1:3, score = 10:12,
                                   stringsAsFactors = FALSE)
  macrox:::.dispatch_step(sess,
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
    macrox:::.dispatch_step(sess,
      list(step = "validate_table", table = "tbl",
           rules = list(r = "nrow(.) > 0"), strict = FALSE)),
    "validate"
  )
})

test_that(".dispatch_step() handles validate_table when validate available", {
  skip_if_not_installed("validate")
  sess <- .make_dispatch_sess()
  expect_no_error(
    macrox:::.dispatch_step(sess,
      list(step = "validate_table", table = "tbl",
           rules = list(has_rows = "nrow(.) > 0"), strict = FALSE))
  )
})

test_that(".dispatch_step() handles select_item with gliner backend", {
  sess    <- .make_dispatch_sess()
  mock_py <- list(gliner_extract_item = function(...) "REF-001")

  testthat::local_mocked_bindings(
    pdf_text       = function(...) list("Reference: REF-001"),
    .package       = "pdftools"
  )
  testthat::local_mocked_bindings(
    .ensure_gliner = function(...) mock_py
  )

  macrox:::.dispatch_step(sess, list(
    step         = "select_item",
    label        = "ref",
    prompt       = "Reference number",
    cast         = NULL,
    page         = NULL,
    backend      = "gliner",
    gliner_model = "fastino/gliner2-base-v1"
  ))

  expect_true("ref" %in% names(sess$items))
  expect_equal(as.character(sess$items$ref$value), "REF-001")
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
  macrox:::.dispatch_step(sess,
    list(step = "select_item", label = "ref",
         prompt = "Extract reference.", cast = "character",
         page = NULL, area = NULL, provider = "anthropic",
         model = NULL, base_url = NULL, dpi = 120L))
  expect_true("ref" %in% names(sess$items))
})

# ── mx_replay() end-to-end with mixed step types ────────────────────────────

test_that(".dispatch_step() handles fill_down", {
  df   <- data.frame(grp = c("A", "", ""), stringsAsFactors = FALSE)
  sess <- .make_dispatch_sess(df)
  macrox:::.dispatch_step(sess,
    list(step = "fill_down", table = "tbl", cols = list("grp")))
  expect_equal(sess$tables$tbl$grp, c("A", "A", "A"))
})

test_that(".dispatch_step() handles clean_numbers", {
  df   <- data.frame(v = c("£1,000", "(500)"), stringsAsFactors = FALSE)
  sess <- .make_dispatch_sess(df)
  macrox:::.dispatch_step(sess,
    list(step = "clean_numbers", table = "tbl", cols = list("v"),
         currency = list("£"), na_strings = list("-"),
         negative_parens = TRUE, convert = TRUE))
  expect_type(sess$tables$tbl$v, "double")
  expect_equal(sess$tables$tbl$v, c(1000, -500))
})

test_that("mx_replay_batch() returns NULL for files that fail to replay", {
  steps  <- list()
  result <- suppressWarnings(mx_replay_batch("nonexistent.pdf", macro = steps))
  expect_null(result[[1]])
  expect_named(result, "nonexistent.pdf")
})

test_that("mx_replay_batch() .parallel = TRUE with empty file list succeeds", {
  skip_if_not_installed("purrr")
  steps  <- list()
  result <- mx_replay_batch(character(0), macro = steps, .parallel = TRUE)
  expect_equal(length(result), 0L)
})

test_that("mx_replay() replays add_column and stack_tables steps", {
  df1 <- data.frame(month = month.abb[1:3], n = 1:3L, stringsAsFactors = FALSE)
  df2 <- data.frame(month = month.abb[4:6], n = 4:6L, stringsAsFactors = FALSE)

  call_n <- 0L
  testthat::local_mocked_bindings(
    select_table = function(sess, label, ...) {
      call_n <<- call_n + 1L
      sess$tables[[label]] <- if (call_n == 1L) df1 else df2
      invisible(sess)
    },
    # Mock mx_session so no real file is needed
    mx_session = function(path) {
      s <- new.env(parent = emptyenv())
      s$path <- path; s$tables <- list(); s$items <- list()
      s$steps <- list(); s$.replaying <- FALSE
      class(s) <- "macrox_session"
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

  result <- mx_replay("dummy.pdf", macro = steps)
  expect_true("all" %in% names(result))
  expect_equal(nrow(result$all), 6L)
  expect_true("year" %in% names(result$t1))
})


# --------------------------------------------------------------------------- #
#  Parameterised macros                                                        #
# --------------------------------------------------------------------------- #

test_that(".substitute_params() replaces $name in character fields", {
  steps <- list(
    list(step = "add_column", table = "tbl", name = "yr", expr = "$year"),
    list(step = "filter_rows", table = "tbl",
         exclude_where = "month != '$month_name'")
  )
  result <- macrox:::.apply_params(
    steps,
    list(year = 2025L, month_name = "Jan")
  )
  expect_equal(result[[1]]$expr, "2025")
  expect_equal(result[[2]]$exclude_where, "month != 'Jan'")
})

test_that(".substitute_params() leaves non-character fields untouched", {
  steps <- list(list(step = "select_table", page = 3L,
                     label = "tbl", area = c(10, 20, 30, 40)))
  result <- macrox:::.apply_params(steps, list(p = 99L))
  expect_equal(result[[1]]$page, 3L)   # integer unchanged
  expect_equal(result[[1]]$area, c(10, 20, 30, 40))
})

test_that(".substitute_params() handles nested list fields", {
  steps <- list(list(
    step   = "cast_types",
    table  = "tbl",
    types  = list(year_col = "$type_spec")
  ))
  result <- macrox:::.apply_params(steps, list(type_spec = "integer"))
  expect_equal(result[[1]]$types$year_col, "integer")
})

test_that("save_macro() stores params declaration in YAML", {
  sess    <- make_sess_with_steps()
  tmp_dir <- tempdir()
  out     <- save_macro(sess, "param_macro", path = tmp_dir, overwrite = TRUE,
                        params = c(year = "integer", region = "character"))
  content <- yaml::read_yaml(out)
  expect_true(!is.null(content$macro$params))
  expect_equal(content$macro$params$year$type,   "integer")
  expect_equal(content$macro$params$region$type, "character")
})

test_that("save_macro() stores bare param names without type info", {
  sess    <- make_sess_with_steps()
  tmp_dir <- tempdir()
  out     <- save_macro(sess, "bare_params", path = tmp_dir, overwrite = TRUE,
                        params = c("year", "month"))
  content <- yaml::read_yaml(out)
  expect_true(!is.null(content$macro$params))
  expect_true("year"  %in% names(content$macro$params))
  expect_true("month" %in% names(content$macro$params))
})

test_that("load_macro() attaches params attribute", {
  sess    <- make_sess_with_steps()
  tmp_dir <- tempdir()
  save_macro(sess, "with_params", path = tmp_dir, overwrite = TRUE,
             params = c(year = "integer"))
  steps <- load_macro("with_params", path = tmp_dir)
  p     <- attr(steps, "params")
  expect_false(is.null(p))
  expect_true("year" %in% names(p))
})

test_that("mx_replay() substitutes params into step fields", {
  testthat::local_mocked_bindings(
    mx_session = function(path) {
      s <- new.env(parent = emptyenv())
      s$path <- path; s$tables <- list(); s$items <- list()
      s$steps <- list(); s$.replaying <- FALSE
      class(s) <- "macrox_session"
      s
    }
  )

  captured_expr <- NULL
  testthat::local_mocked_bindings(
    add_column = function(sess, table, name, expr) {
      captured_expr <<- expr
      sess$tables[[table]][[name]] <- rep(eval(parse(text = expr)), nrow(sess$tables[[table]]))
      invisible(sess)
    }
  )

  steps <- list(
    list(step = "add_column", table = "scores",
         name = "year", expr = "$year")
  )

  # Pre-populate a table so add_column has something to act on
  preseed <- function(path) {
    s <- new.env(parent = emptyenv())
    s$path <- path; s$items <- list(); s$steps <- list()
    s$tables <- list(scores = data.frame(x = 1:3, stringsAsFactors = FALSE))
    s$.replaying <- FALSE
    class(s) <- "macrox_session"
    s
  }
  testthat::local_mocked_bindings(mx_session = preseed)

  mx_replay("dummy.pdf", steps, params = list(year = 2025L))
  expect_equal(captured_expr, "2025")
})

test_that("mx_replay() warns when declared params are missing", {
  steps <- list(list(step = "add_column", table = "t", name = "yr", expr = "$year"))
  attr(steps, "params") <- list(year = list(type = "integer"))

  testthat::local_mocked_bindings(
    mx_session = function(path) {
      s <- new.env(parent = emptyenv())
      s$path <- path; s$tables <- list(t = data.frame(x = 1L));
      s$items <- list(); s$steps <- list(); s$.replaying <- FALSE
      class(s) <- "macrox_session"
      s
    },
    add_column = function(sess, ...) invisible(sess)
  )

  expect_warning(
    mx_replay("dummy.pdf", steps),  # no params supplied
    "not supplied"
  )
})


# --------------------------------------------------------------------------- #
#  test_macro() — snapshot testing                                              #
# --------------------------------------------------------------------------- #

test_that("test_macro() writes a snapshot on first call", {
  tables <- list(
    monthly = data.frame(month = month.abb[1:3], count = 1:3L,
                         stringsAsFactors = FALSE)
  )
  snap_dir <- file.path(tempdir(), "snaps_new")
  unlink(snap_dir, recursive = TRUE)

  result <- test_macro(tables = tables, name = "first_run",
                       snapshot_dir = snap_dir)

  snap_file <- file.path(snap_dir, "first_run.snap.yml")
  expect_true(file.exists(snap_file))
  content <- yaml::read_yaml(snap_file)
  expect_equal(content$tables$monthly$nrow, 3L)
  expect_equal(unlist(content$tables$monthly$cols), c("month", "count"))
})

test_that("test_macro() passes when output matches snapshot", {
  tables <- list(
    monthly = data.frame(month = month.abb[1:3], count = 1:3L,
                         stringsAsFactors = FALSE)
  )
  snap_dir <- file.path(tempdir(), "snaps_match")
  unlink(snap_dir, recursive = TRUE)

  # First call: write snapshot
  test_macro(tables = tables, name = "match_run", snapshot_dir = snap_dir)

  # Second call: compare — should pass silently
  expect_no_error(
    test_macro(tables = tables, name = "match_run", snapshot_dir = snap_dir)
  )
})

test_that("test_macro() fails when row count changes", {
  tables_ref <- list(
    t = data.frame(a = 1:3, stringsAsFactors = FALSE)
  )
  tables_new <- list(
    t = data.frame(a = 1:5, stringsAsFactors = FALSE)
  )
  snap_dir <- file.path(tempdir(), "snaps_rowchange")
  unlink(snap_dir, recursive = TRUE)

  test_macro(tables = tables_ref, name = "row_chg", snapshot_dir = snap_dir)

  expect_error(
    test_macro(tables = tables_new, name = "row_chg", snapshot_dir = snap_dir),
    "row count mismatch"
  )
})

test_that("test_macro() fails when column names change", {
  tables_ref <- list(
    t = data.frame(a = 1:3, b = 4:6, stringsAsFactors = FALSE)
  )
  tables_new <- list(
    t = data.frame(a = 1:3, c = 4:6, stringsAsFactors = FALSE)
  )
  snap_dir <- file.path(tempdir(), "snaps_colchange")
  unlink(snap_dir, recursive = TRUE)

  test_macro(tables = tables_ref, name = "col_chg", snapshot_dir = snap_dir)

  expect_error(
    test_macro(tables = tables_new, name = "col_chg", snapshot_dir = snap_dir),
    "column mismatch"
  )
})

test_that("test_macro() fails when first rows change", {
  tables_ref <- list(
    t = data.frame(v = c("A", "B", "C"), stringsAsFactors = FALSE)
  )
  tables_new <- list(
    t = data.frame(v = c("X", "B", "C"), stringsAsFactors = FALSE)
  )
  snap_dir <- file.path(tempdir(), "snaps_headchange")
  unlink(snap_dir, recursive = TRUE)

  test_macro(tables = tables_ref, name = "head_chg", snapshot_dir = snap_dir)

  expect_error(
    test_macro(tables = tables_new, name = "head_chg", snapshot_dir = snap_dir),
    "first rows differ"
  )
})

test_that("test_macro() update = TRUE overwrites snapshot", {
  tables_ref <- list(t = data.frame(x = 1:3, stringsAsFactors = FALSE))
  tables_new <- list(t = data.frame(x = 1:5, stringsAsFactors = FALSE))

  snap_dir <- file.path(tempdir(), "snaps_update")
  unlink(snap_dir, recursive = TRUE)

  test_macro(tables = tables_ref, name = "upd", snapshot_dir = snap_dir)

  # Update with new tables — should succeed and overwrite
  expect_no_error(
    test_macro(tables = tables_new, name = "upd", snapshot_dir = snap_dir,
               update = TRUE)
  )

  # Now matching new snapshot: 5 rows — should pass
  expect_no_error(
    test_macro(tables = tables_new, name = "upd", snapshot_dir = snap_dir)
  )
})

test_that("test_macro() detects added tables", {
  snap_dir <- file.path(tempdir(), "snaps_added")
  unlink(snap_dir, recursive = TRUE)

  test_macro(tables = list(t1 = data.frame(x = 1L)),
             name = "added_tbl", snapshot_dir = snap_dir)

  expect_error(
    test_macro(tables = list(t1 = data.frame(x = 1L),
                             t2 = data.frame(y = 2L)),
               name = "added_tbl", snapshot_dir = snap_dir),
    "New tables not in snapshot"
  )
})

test_that("test_macro() detects removed tables", {
  snap_dir <- file.path(tempdir(), "snaps_removed")
  unlink(snap_dir, recursive = TRUE)

  test_macro(tables = list(t1 = data.frame(x = 1L),
                           t2 = data.frame(y = 2L)),
             name = "rm_tbl", snapshot_dir = snap_dir)

  expect_error(
    test_macro(tables = list(t1 = data.frame(x = 1L)),
               name = "rm_tbl", snapshot_dir = snap_dir),
    "removed since snapshot"
  )
})

test_that("test_macro() errors without file+macro or tables", {
  expect_error(
    test_macro(name = "no_input"),
    "either"
  )
})

test_that("test_macro() errors when tables supplied without name and macro is not a string", {
  expect_error(
    test_macro(tables = list(t = data.frame(x = 1L)),
               snapshot_dir = tempdir()),
    "Cannot derive a snapshot name"
  )
})

test_that(".apply_params() substitutes longer keys before shorter prefix keys", {
  steps <- list(
    list(step = "add_column", table = "t", name = "a", expr = "$year"),
    list(step = "add_column", table = "t", name = "b", expr = "$year_end")
  )
  result <- macrox:::.apply_params(steps, list(year = "2025", year_end = "December"))
  expect_equal(result[[1]]$expr, "2025")
  expect_equal(result[[2]]$expr, "December")
})

test_that(".apply_params() aborts with a clear message on non-scalar param", {
  steps <- list(list(step = "add_column", table = "t", name = "a", expr = "$yr"))
  expect_error(
    macrox:::.apply_params(steps, list(yr = c(2024L, 2025L))),
    "scalar"
  )
})
