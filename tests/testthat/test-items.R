# tests/testthat/test-items.R

make_item_sess <- function() {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$items  <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"
  sess
}

# --------------------------------------------------------------------------- #
#  select_item()                                                               #
# --------------------------------------------------------------------------- #

test_that("select_item() stores result in sess$items", {
  sess <- make_item_sess()
  # Mock: text-based path — mock pdf_text and LLM chat
  testthat::local_mocked_bindings(
    pdf_text = function(...) list("Invoice Number: INV-9821\nDate: 01/05/2025"),
    .package = "pdftools"
  )
  mock_chat <- list(
    chat_structured = function(...) list(value = "INV-9821")
  )
  testthat::local_mocked_bindings(
    .make_llm_chat = function(...) mock_chat,
    .check_ellmer  = function(...) invisible(NULL)
  )

  select_item(sess, "invoice_number",
              prompt = "Extract the invoice number.")

  expect_true("invoice_number" %in% names(sess$items))
  expect_equal(as.character(sess$items$invoice_number$value), "INV-9821")
})

test_that("select_item() records the step", {
  sess <- make_item_sess()
  testthat::local_mocked_bindings(
    pdf_text = function(...) list("Total: 1234.56"),
    .package = "pdftools"
  )
  mock_chat <- list(chat_structured = function(...) list(value = "1234.56"))
  testthat::local_mocked_bindings(
    .make_llm_chat = function(...) mock_chat,
    .check_ellmer  = function(...) invisible(NULL)
  )

  select_item(sess, "total", prompt = "Grand total.", cast = "numeric")

  s <- sess$steps[[1]]
  expect_equal(s$step,   "select_item")
  expect_equal(s$label,  "total")
  expect_equal(s$cast,   "numeric")
})

test_that("select_item() applies cast to the returned value", {
  sess <- make_item_sess()
  testthat::local_mocked_bindings(
    pdf_text = function(...) list("Count: 42"),
    .package = "pdftools"
  )
  mock_chat <- list(chat_structured = function(...) list(value = "42"))
  testthat::local_mocked_bindings(
    .make_llm_chat = function(...) mock_chat,
    .check_ellmer  = function(...) invisible(NULL)
  )

  select_item(sess, "count", prompt = "Count.", cast = "integer")
  expect_equal(sess$items$count$value, 42L)
})

test_that("select_item() stores raw value alongside cast value", {
  sess <- make_item_sess()
  testthat::local_mocked_bindings(
    pdf_text = function(...) list("Amount: 99.9"),
    .package = "pdftools"
  )
  mock_chat <- list(chat_structured = function(...) list(value = "99.9"))
  testthat::local_mocked_bindings(
    .make_llm_chat = function(...) mock_chat,
    .check_ellmer  = function(...) invisible(NULL)
  )

  select_item(sess, "amount", prompt = "Amount.", cast = "numeric")
  expect_equal(sess$items$amount$raw, "99.9")
})

# ── GLiNER backend ────────────────────────────────────────────────────────── #

test_that("select_item() gliner backend stores value and backend tag", {
  sess    <- make_item_sess()
  mock_py <- list(gliner_extract_item = function(text, label, description, all_matches = FALSE) "INV-9821")

  testthat::local_mocked_bindings(
    pdf_text      = function(...) list("Invoice Number: INV-9821"),
    .package      = "pdftools"
  )
  testthat::local_mocked_bindings(
    .ensure_gliner = function(...) mock_py
  )

  select_item(sess, "invoice_number",
              prompt  = "Invoice ID or reference number",
              backend = "gliner")

  expect_equal(as.character(sess$items$invoice_number$value), "INV-9821")
  expect_equal(sess$items$invoice_number$backend, "gliner")
})

test_that("select_item() gliner records step with backend and gliner_model", {
  sess    <- make_item_sess()
  mock_py <- list(gliner_extract_item = function(text, label, description, all_matches = FALSE) "ABC")

  testthat::local_mocked_bindings(
    pdf_text       = function(...) list("ref: ABC"),
    .package       = "pdftools"
  )
  testthat::local_mocked_bindings(
    .ensure_gliner = function(...) mock_py
  )

  select_item(sess, "ref",
              prompt       = "Reference code",
              backend      = "gliner",
              gliner_model = "fastino/gliner2-large-v1")

  s <- sess$steps[[1]]
  expect_equal(s$backend,      "gliner")
  expect_equal(s$gliner_model, "fastino/gliner2-large-v1")
  expect_null(s$provider)
  expect_null(s$dpi)
})

test_that("select_item() gliner uses only the specified page's text", {
  sess    <- make_item_sess()
  pages   <- list("page one text", "page two text", "page three text")
  seen_text <- NULL
  mock_py <- list(
    gliner_extract_item = function(text, label, description, all_matches = FALSE) {
      seen_text <<- text
      "page two text"
    }
  )
  testthat::local_mocked_bindings(
    pdf_text       = function(...) pages,
    .package       = "pdftools"
  )
  testthat::local_mocked_bindings(
    .ensure_gliner = function(...) mock_py
  )

  select_item(sess, "excerpt", prompt = "text", backend = "gliner", page = 2L)
  expect_equal(seen_text, "page two text")
})

test_that("select_item() gliner returns empty string when model finds nothing", {
  sess    <- make_item_sess()
  mock_py <- list(gliner_extract_item = function(text, label, description, all_matches = FALSE) NULL)

  testthat::local_mocked_bindings(
    pdf_text       = function(...) list("no match here"),
    .package       = "pdftools"
  )
  testthat::local_mocked_bindings(
    .ensure_gliner = function(...) mock_py
  )

  select_item(sess, "missing", prompt = "nonexistent field", backend = "gliner")
  expect_equal(as.character(sess$items$missing$value), "")
})

test_that("select_item() llm records backend = 'llm' in step", {
  sess <- make_item_sess()
  testthat::local_mocked_bindings(
    pdf_text = function(...) list("Total: 100"),
    .package = "pdftools"
  )
  mock_chat <- list(chat_structured = function(...) list(value = "100"))
  testthat::local_mocked_bindings(
    .make_llm_chat = function(...) mock_chat,
    .check_ellmer  = function(...) invisible(NULL)
  )

  select_item(sess, "total", prompt = "Grand total.", backend = "llm")
  expect_equal(sess$steps[[1]]$backend, "llm")
})

# --------------------------------------------------------------------------- #
#  update_item()                                                               #
# --------------------------------------------------------------------------- #

test_that("update_item() errors when no select_item step exists", {
  sess <- make_item_sess()
  expect_error(update_item(sess, "missing", "new prompt"), "select_item")
})

test_that("update_item() updates the prompt in the step", {
  sess <- make_item_sess()
  sess$items[["num"]] <- list(value = "1", raw = "1", cast = "character",
                               prompt = "old", provider = "anthropic", model = "x")
  sess$steps <- list(list(
    step = "select_item", label = "num", prompt = "old",
    cast = "character", page = NULL, area = NULL,
    provider = "anthropic", model = "x", base_url = NULL, dpi = 120L
  ))
  class(sess) <- "macrox_session"

  update_item(sess, "num", prompt = "new prompt", re_extract = FALSE)
  expect_equal(sess$steps[[1]]$prompt, "new prompt")
})

# --------------------------------------------------------------------------- #
#  show_items()                                                                #
# --------------------------------------------------------------------------- #

test_that("show_items() reports when no items exist", {
  sess <- make_item_sess()
  # cli::cli_inform writes to the message connection, not stdout
  msgs <- capture.output(show_items(sess), type = "message")
  expect_true(any(grepl("No items", msgs)))
})

test_that("show_items() prints each item label and value", {
  sess <- make_item_sess()
  sess$items <- list(
    invoice_number = list(value = "INV-9821", cast = "character"),
    total          = list(value = 1234.56,    cast = "numeric")
  )
  msgs <- capture.output(show_items(sess), type = "message")
  expect_true(any(grepl("invoice_number", msgs)))
  expect_true(any(grepl("INV-9821",       msgs)))
  expect_true(any(grepl("total",           msgs)))
})

# --------------------------------------------------------------------------- #
#  export_json()                                                               #
# --------------------------------------------------------------------------- #

test_that("export_json() returns a JSON string", {
  skip_if_not_installed("jsonlite")
  sess <- make_item_sess()
  sess$tables <- list(data = data.frame(x = 1:2, stringsAsFactors = FALSE))
  sess$items  <- list(ref = list(value = "ABC123"))

  json <- export_json(sess)
  expect_true(nchar(json) > 0)
  parsed <- jsonlite::fromJSON(json)
  expect_true(!is.null(parsed$tables))
  expect_true(!is.null(parsed$items))
})

test_that("export_json() includes item values at top level", {
  skip_if_not_installed("jsonlite")
  sess <- make_item_sess()
  sess$items <- list(inv = list(value = "INV-001"))

  json   <- export_json(sess)
  parsed <- jsonlite::fromJSON(json)
  expect_equal(parsed$items$inv, "INV-001")
})

test_that("export_json() converts tables to row-arrays", {
  skip_if_not_installed("jsonlite")
  sess <- make_item_sess()
  sess$tables <- list(
    t1 = data.frame(a = 1:2, b = c("x","y"), stringsAsFactors = FALSE)
  )
  json   <- export_json(sess)
  parsed <- jsonlite::fromJSON(json)
  # t1 should be a list of row objects
  expect_equal(length(parsed$tables$t1), 2L)
})

test_that("export_json() writes file when path supplied", {
  skip_if_not_installed("jsonlite")
  sess <- make_item_sess()
  tmp  <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  export_json(sess, path = tmp)
  expect_true(file.exists(tmp))
  expect_true(file.size(tmp) > 0)
})

test_that("export_json() includes steps when include_steps = TRUE", {
  skip_if_not_installed("jsonlite")
  sess <- make_item_sess()
  sess$steps <- list(list(step = "select_table", label = "t1"))

  json   <- export_json(sess, include_steps = TRUE)
  parsed <- jsonlite::fromJSON(json)
  expect_true(!is.null(parsed$steps))
})

test_that("export_json() omits steps by default", {
  skip_if_not_installed("jsonlite")
  sess   <- make_item_sess()
  json   <- export_json(sess)
  parsed <- jsonlite::fromJSON(json)
  expect_true(is.null(parsed$steps))
})
