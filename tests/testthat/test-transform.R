test_that("rename_columns() renames correctly", {
  df   <- data.frame(Month = 1:3, Total = 4:6, stringsAsFactors = FALSE)
  sess <- make_sess(df = df)
  rename_columns(sess, "tbl", c(Month = "month", Total = "total"))
  expect_equal(names(sess$tables$tbl), c("month", "total"))
})

test_that("rename_columns() warns on missing column", {
  df   <- data.frame(A = 1:3, stringsAsFactors = FALSE)
  sess <- make_sess(df = df)
  expect_warning(
    rename_columns(sess, "tbl", c(ZZZ = "zzz")),
    "not found"
  )
  # Original column untouched
  expect_equal(names(sess$tables$tbl), "A")
})

test_that(".cast_col() strips commas before numeric conversion", {
  result <- pdfmacro:::.cast_col(c("1,234", "5,678"), "integer")
  expect_equal(result, c(1234L, 5678L))
})

test_that(".cast_col() handles date format strings", {
  result <- pdfmacro:::.cast_col("31/12/2024", "date:%d/%m/%Y")
  expect_s3_class(result, "Date")
  expect_equal(format(result, "%Y-%m-%d"), "2024-12-31")
})

test_that(".cast_col() handles numeric type", {
  result <- pdfmacro:::.cast_col(c("1.5", "2.7"), "numeric")
  expect_equal(result, c(1.5, 2.7))
})

test_that("filter_rows() removes correct rows", {
  df   <- data.frame(
    month = c("Jan", "Feb", "Total"),
    n     = c(10L, 20L, 30L),
    stringsAsFactors = FALSE
  )
  sess <- make_sess(df = df)
  filter_rows(sess, "tbl", "month == 'Total'")
  expect_equal(nrow(sess$tables$tbl), 2L)
  expect_false("Total" %in% sess$tables$tbl$month)
})

test_that("preview_all() runs without error on session with tables", {
  df   <- data.frame(x = 1:3, y = c(1.1, 2.2, 3.3), stringsAsFactors = FALSE)
  sess <- make_sess(df = df)
  expect_no_error(preview_all(sess))
})

test_that("cast_types() records step", {
  df   <- data.frame(val = c("1,000", "2,000"), stringsAsFactors = FALSE)
  sess <- make_sess(df = df)
  cast_types(sess, "tbl", c(val = "integer"))
  expect_length(sess$steps, 1L)
  expect_equal(sess$steps[[1]]$step, "cast_types")
})

# ── Column casing — all engines preserve original case ──────────────────────

test_that("rename_columns() finds original-case column names from bbox extraction", {
  # bbox now preserves case (make.names, no tolower)
  df   <- data.frame(Month = 1:3, Male.Count = 4:6, stringsAsFactors = FALSE)
  sess <- make_sess(df = df)
  rename_columns(sess, "tbl", c(Month = "month", Male.Count = "male_count"))
  expect_equal(names(sess$tables$tbl), c("month", "male_count"))
})

test_that(".flatten_headers() preserves case", {
  df <- data.frame(
    V1 = c("Calves Born", "Male",  "10", "20"),
    V2 = c("Calves Born", "Female","15", "25"),
    stringsAsFactors = FALSE
  )
  result <- pdfmacro:::.flatten_headers(df, header_rows = 2)
  # Should NOT be lowercased
  expect_true(any(grepl("Calves", names(result))))
})

# ── .build_filter_expr() — GUI filter expression builder ─────────────────── #

test_that(".build_filter_expr() builds == expression with string value", {
  expr <- pdfmacro:::.build_filter_expr("month", "==", "Total")
  expect_equal(expr, "`month` == 'Total'")
})

test_that(".build_filter_expr() builds == expression with numeric value", {
  expr <- pdfmacro:::.build_filter_expr("total", "==", "0")
  expect_equal(expr, "`total` == 0")
})

test_that(".build_filter_expr() builds != expression", {
  expr <- pdfmacro:::.build_filter_expr("status", "!=", "Active")
  expect_equal(expr, "`status` != 'Active'")
})

test_that(".build_filter_expr() builds grepl expression", {
  expr <- pdfmacro:::.build_filter_expr("month", "grepl", "Total")
  expect_match(expr, "grepl\\('Total'")
  expect_match(expr, "ignore.case = TRUE")
})

test_that(".build_filter_expr() builds !grepl expression", {
  expr <- pdfmacro:::.build_filter_expr("month", "!grepl", "Total")
  expect_match(expr, "^!grepl")
})

test_that(".build_filter_expr() builds is.na expression (no value needed)", {
  expr <- pdfmacro:::.build_filter_expr("value", "is.na", "")
  expect_equal(expr, "is.na(`value`)")
})

test_that(".build_filter_expr() builds !is.na expression", {
  expr <- pdfmacro:::.build_filter_expr("value", "!is.na", "")
  expect_equal(expr, "!is.na(`value`)")
})

test_that(".build_filter_expr() handles column names with spaces via backticks", {
  expr <- pdfmacro:::.build_filter_expr("Breed of Dam", "==", "Friesian")
  expect_match(expr, "`Breed of Dam`")
})

test_that(".build_filter_expr() result evaluates correctly on a data frame", {
  df   <- data.frame(month = c("Jan", "Feb", "Total"), n = 1:3,
                     stringsAsFactors = FALSE)
  expr <- pdfmacro:::.build_filter_expr("month", "==", "Total")
  mask <- eval(parse(text = expr), envir = df)
  expect_equal(sum(mask), 1L)
  expect_equal(which(mask), 3L)
})

# ── add_column() ─────────────────────────────────────────────────────────── #

test_that("add_column() adds a scalar column to the table", {
  sess <- make_sess()
  add_column(sess, "tbl", "year", "2024L")
  expect_true("year" %in% names(sess$tables$tbl))
  expect_true(all(sess$tables$tbl$year == 2024L))
})

test_that("add_column() evaluates vector expression against the data frame", {
  df   <- data.frame(a = 1:3, b = 4:6, stringsAsFactors = FALSE)
  sess <- make_sess(df = df)
  add_column(sess, "tbl", "sum_ab", "a + b")
  expect_equal(sess$tables$tbl$sum_ab, c(5L, 7L, 9L))
})

test_that("add_column() records the step", {
  sess <- make_sess()
  add_column(sess, "tbl", "flag", "TRUE")
  expect_equal(length(sess$steps), 1L)
  expect_equal(sess$steps[[1]]$step, "add_column")
  expect_equal(sess$steps[[1]]$name, "flag")
})

test_that("add_column() errors on bad expression", {
  sess <- make_sess()
  expect_error(add_column(sess, "tbl", "bad", "nonexistent_col + 1"),
               regexp = NULL)
})

# ── stack_tables() ────────────────────────────────────────────────────────── #

test_that("stack_tables() row-binds tables with matching schemas", {
  df1  <- data.frame(a = 1:2, b = 3:4, stringsAsFactors = FALSE)
  df2  <- data.frame(a = 5:6, b = 7:8, stringsAsFactors = FALSE)
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(t1 = df1, t2 = df2)
  sess$steps  <- list()
  class(sess) <- "pdfmacro_session"

  stack_tables(sess, "combined", c("t1", "t2"))
  combined <- sess$tables[["combined"]]
  expect_equal(nrow(combined), 4L)
  expect_equal(names(combined), c("a", "b"))
  expect_equal(combined$a, c(1L, 2L, 5L, 6L))
})

test_that("stack_tables() with .fill pads missing columns", {
  df1  <- data.frame(a = 1:2, b = 3:4, stringsAsFactors = FALSE)
  df2  <- data.frame(a = 5:6, c = 9:10, stringsAsFactors = FALSE)
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(t1 = df1, t2 = df2)
  sess$steps  <- list()
  class(sess) <- "pdfmacro_session"

  stack_tables(sess, "combined", c("t1", "t2"), .fill = TRUE)
  combined <- sess$tables[["combined"]]
  expect_equal(nrow(combined), 4L)
  expect_true(all(c("a", "b", "c") %in% names(combined)))
  expect_true(any(is.na(combined$c[1:2])))   # t1 has no c col
})

test_that("stack_tables() records the step", {
  df   <- data.frame(x = 1, stringsAsFactors = FALSE)
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(t1 = df, t2 = df)
  sess$steps  <- list()
  class(sess) <- "pdfmacro_session"

  stack_tables(sess, "out", c("t1", "t2"))
  expect_equal(sess$steps[[1]]$step, "stack_tables")
  expect_equal(unlist(sess$steps[[1]]$tables), c("t1", "t2"))
})

# ── merge_tables() ────────────────────────────────────────────────────────── #

test_that("merge_tables() performs an inner join on by column", {
  left  <- data.frame(id = 1:3, val = c("a","b","c"), stringsAsFactors = FALSE)
  right <- data.frame(id = 2:4, score = c(10,20,30),  stringsAsFactors = FALSE)
  sess  <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(left = left, right = right)
  sess$steps  <- list()
  class(sess) <- "pdfmacro_session"

  merge_tables(sess, "merged", left = "left", right = "right", by = "id")
  merged <- sess$tables[["merged"]]
  expect_equal(nrow(merged), 2L)   # inner join on ids 2 and 3
  expect_true(all(c("id","val","score") %in% names(merged)))
})

test_that("merge_tables() records step with by and all arguments", {
  df   <- data.frame(x = 1:2, stringsAsFactors = FALSE)
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(a = df, b = df)
  sess$steps  <- list()
  class(sess) <- "pdfmacro_session"

  merge_tables(sess, "out", left = "a", right = "b", by = "x", all = FALSE)
  s <- sess$steps[[1]]
  expect_equal(s$step, "merge_tables")
  expect_equal(unlist(s$by), "x")
  expect_false(isTRUE(s$all))
})
