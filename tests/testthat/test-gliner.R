# tests/testthat/test-gliner.R

# --------------------------------------------------------------------------- #
#  GLiNER2 backend unit tests                                                  #
# --------------------------------------------------------------------------- #

test_that("close_gliner() runs without error", {
  expect_no_error(close_gliner())
})

test_that("select_items_batch() validates items argument", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$items  <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  expect_error(
    select_items_batch(sess, items = c("unnamed_value")),
    "named character"
  )
})

test_that("select_items_batch() extracts fields and records steps", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$items  <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  raw_result <- list(
    inv_no = "INV-001",
    total  = "5000"
  )

  mock_py <- new.env(parent = emptyenv())
  mock_py$gliner_batch_extract <- function(text, items, all_matches = FALSE) raw_result

  testthat::local_mocked_bindings(
    pdf_text       = function(...) list("Invoice: INV-001\nTotal: 5000"),
    .package       = "pdftools"
  )
  testthat::local_mocked_bindings(
    .ensure_gliner = function(...) mock_py
  )
  testthat::local_mocked_bindings(
    py_to_r = function(x) x,
    .package = "reticulate"
  )

  select_items_batch(sess,
    items = c(inv_no = "Invoice number", total = "Total amount"),
    cast  = c(total = "numeric"))

  expect_true("inv_no" %in% names(sess$items))
  expect_true("total"  %in% names(sess$items))
  expect_equal(as.character(sess$items$inv_no$value), "INV-001")
  expect_equal(sess$items$total$value, 5000)

  expect_length(sess$steps, 2L)
  expect_equal(sess$steps[[1]]$step, "select_item")
  expect_equal(sess$steps[[2]]$step, "select_item")
})

test_that("select_struct() stores raw records in sess$structs", {
  sess <- new.env(parent = emptyenv())
  sess$path    <- "dummy.pdf"
  sess$tables  <- list()
  sess$structs <- list()
  sess$steps   <- list()
  class(sess)  <- "macrox_session"

  fake_records <- list(
    list(
      desc     = list(list(text = "Widget A", confidence = 0.9)),
      quantity = list(list(text = "10", confidence = 0.85))
    ),
    list(
      desc     = list(list(text = "Widget B", confidence = 0.7)),
      quantity = list(list(text = "5", confidence = 0.8))
    )
  )

  mock_py <- new.env(parent = emptyenv())
  mock_py$gliner_extract_struct <- function(text, entity, field_specs) fake_records

  testthat::local_mocked_bindings(
    pdf_text       = function(...) list("Widget A qty 10, Widget B qty 5"),
    .package       = "pdftools"
  )
  testthat::local_mocked_bindings(
    .ensure_gliner = function(...) mock_py
  )
  testthat::local_mocked_bindings(
    py_to_r = function(x) x,
    .package = "reticulate"
  )

  select_struct(sess, "products",
    entity = "Product",
    fields = c(desc = "Product description", quantity = "Number of units"),
    page   = 1L)

  expect_true("products" %in% names(sess$structs))
  expect_length(sess$structs$products$records, 2L)

  s <- sess$steps[[1]]
  expect_equal(s$step,   "select_struct")
  expect_equal(s$label,  "products")
  expect_equal(s$entity, "Product")
})

test_that("struct_to_df() converts records to a data frame", {
  sess <- new.env(parent = emptyenv())
  sess$path    <- "dummy.pdf"
  sess$tables  <- list()
  sess$structs <- list(
    items = list(
      records = list(
        list(
          name  = list(list(text = "Alice", confidence = 0.95)),
          score = list(list(text = "90",    confidence = 0.88))
        ),
        list(
          name  = list(list(text = "Bob",   confidence = 0.80)),
          score = list(list(text = "75",    confidence = 0.70))
        )
      ),
      col_names = c("name", "score"),
      entity    = "Person",
      specs     = c("name::str::Name", "score::str::Score")
    )
  )
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  struct_to_df(sess, "items")

  expect_true("items" %in% names(sess$tables))
  df <- sess$tables$items
  expect_equal(nrow(df), 2L)
  expect_true("name" %in% names(df))
  expect_true("score" %in% names(df))
  expect_true("name_conf" %in% names(df))
  expect_equal(df$name, c("Alice", "Bob"))
})

test_that("struct_to_df() filters by min_confidence", {
  sess <- new.env(parent = emptyenv())
  sess$path    <- "dummy.pdf"
  sess$tables  <- list()
  sess$structs <- list(
    items = list(
      records = list(
        list(
          name  = list(list(text = "High", confidence = 0.95)),
          val   = list(list(text = "100",  confidence = 0.90))
        ),
        list(
          name  = list(list(text = "Low",  confidence = 0.20)),
          val   = list(list(text = "10",   confidence = 0.30))
        )
      ),
      col_names = c("name", "val"),
      entity    = "Item",
      specs     = c("name::str::Name", "val::str::Value")
    )
  )
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  struct_to_df(sess, "items", min_confidence = 0.5)

  df <- sess$tables$items
  expect_equal(nrow(df), 1L)
  expect_equal(df$name, "High")
})

test_that("struct_to_df() errors when no struct data exists", {
  sess <- new.env(parent = emptyenv())
  sess$path    <- "dummy.pdf"
  sess$tables  <- list()
  sess$structs <- list()
  sess$steps   <- list()
  class(sess)  <- "macrox_session"

  expect_error(struct_to_df(sess, "nonexistent"), "select_struct")
})

test_that("struct_to_df() handles empty record list", {
  sess <- new.env(parent = emptyenv())
  sess$path    <- "dummy.pdf"
  sess$tables  <- list()
  sess$structs <- list(
    empty_struct = list(
      records   = list(),
      col_names = c("a", "b"),
      entity    = "Thing",
      specs     = c("a::str::A", "b::str::B")
    )
  )
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  struct_to_df(sess, "empty_struct")

  df <- sess$tables$empty_struct
  expect_equal(nrow(df), 0L)
  expect_true(all(c("a", "b", "a_conf", "b_conf") %in% names(df)))
})
