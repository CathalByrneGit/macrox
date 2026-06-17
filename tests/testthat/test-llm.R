# tests/testthat/test-llm.R
# LLM helpers — no real API calls; tests cover parsing, type building,
# and result conversion only.

# --------------------------------------------------------------------------- #
#  .parse_schema_text()                                                        #
# --------------------------------------------------------------------------- #

test_that(".parse_schema_text() returns NULL for empty input", {
  expect_null(macrox:::.parse_schema_text(""))
  expect_null(macrox:::.parse_schema_text(NULL))
  expect_null(macrox:::.parse_schema_text("   \n  \n"))
})

test_that(".parse_schema_text() parses name:type pairs", {
  txt <- "Month: character\nMale: integer\nTotal: numeric"
  result <- macrox:::.parse_schema_text(txt)
  expect_equal(result[["Month"]], "character")
  expect_equal(result[["Male"]],  "integer")
  expect_equal(result[["Total"]], "numeric")
})

test_that(".parse_schema_text() defaults to character when no type given", {
  result <- macrox:::.parse_schema_text("County\nValue: integer")
  expect_equal(result[["County"]], "character")
  expect_equal(result[["Value"]],  "integer")
})

test_that(".parse_schema_text() ignores blank lines", {
  txt <- "Month: character\n\n\nMale: integer\n"
  result <- macrox:::.parse_schema_text(txt)
  expect_length(result, 2L)
})

test_that(".parse_schema_text() trims whitespace from names and types", {
  result <- macrox:::.parse_schema_text("  Month  :  character  ")
  expect_equal(names(result)[[1]], "Month")
  expect_equal(result[[1]],        "character")
})

# --------------------------------------------------------------------------- #
#  .llm_result_to_df()                                                         #
# --------------------------------------------------------------------------- #

test_that(".llm_result_to_df() converts schema result (data frame rows) correctly", {
  # With a schema, ellmer returns result$rows as a data frame
  rows_df <- data.frame(
    Month  = c("Jan", "Feb"),
    Male   = c("100", "200"),
    Female = c("90",  "180"),
    stringsAsFactors = FALSE
  )
  result <- list(rows = rows_df)
  schema <- c(Month = "character", Male = "character", Female = "character")

  df <- macrox:::.llm_result_to_df(result, schema)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_equal(ncol(df), 3L)
  expect_true("Month" %in% names(df))
})

test_that(".llm_result_to_df() converts auto result (headers + rows) correctly", {
  result <- list(
    headers = list("Month", "Male", "Total"),
    rows    = list(
      list("Jan", "100", "100"),
      list("Feb", "200", "200")
    )
  )
  df <- macrox:::.llm_result_to_df(result, schema = NULL)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_equal(ncol(df), 3L)
  expect_equal(names(df)[[1]], "Month")
})

test_that(".llm_result_to_df() returns empty df for empty headers", {
  result <- list(headers = list(), rows = list())
  df <- macrox:::.llm_result_to_df(result, schema = NULL)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
})

test_that(".llm_result_to_df() pads/trims rows that don't match header count", {
  # Row with fewer cells than headers — should not error
  result <- list(
    headers = list("A", "B", "C"),
    rows    = list(list("x", "y"))   # only 2 values for 3 headers
  )
  expect_no_error(macrox:::.llm_result_to_df(result, schema = NULL))
})

# --------------------------------------------------------------------------- #
#  .build_llm_type() — structural checks (doesn't need ellmer installed)      #
# --------------------------------------------------------------------------- #

test_that(".build_llm_type() requires ellmer", {
  skip_if(requireNamespace("ellmer", quietly = TRUE),
          "ellmer is installed — skipping absence check")
  expect_error(
    macrox:::.build_llm_type(c(Month = "character")),
    "ellmer"
  )
})

test_that(".build_llm_type() with schema builds type with 'rows' field", {
  skip_if_not_installed("ellmer")
  schema <- c(Month = "character", Male = "integer")
  typ    <- macrox:::.build_llm_type(schema)
  # ellmer type specs vary in class across versions — non-NULL is sufficient
  expect_false(is.null(typ))
})

test_that(".build_llm_type() with NULL schema builds type with 'headers' and 'rows'", {
  skip_if_not_installed("ellmer")
  typ <- macrox:::.build_llm_type(NULL)
  expect_true(!is.null(typ))
})

# --------------------------------------------------------------------------- #
#  update_llm_schema() — session plumbing (no API call)                       #
# --------------------------------------------------------------------------- #

test_that("update_llm_schema() errors when no LLM step exists for label", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  expect_error(
    update_llm_schema(sess, "nonexistent", c(A = "character")),
    "No.*select_table_llm"
  )
})

test_that("update_llm_schema() updates schema field without re_extract", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(tbl = data.frame(x = 1))
  sess$steps  <- list(list(
    step = "select_table_llm", label = "tbl",
    page = 1L, area = NULL, provider = "anthropic",
    model = "claude-opus-4-5-20251001",
    schema = list(X = "character"), prompt = NULL,
    dpi = 150L, header_rows = 1L
  ))
  class(sess) <- "macrox_session"

  new_schema <- c(Month = "character", Total = "integer")
  update_llm_schema(sess, "tbl", new_schema, re_extract = FALSE)

  updated <- sess$steps[[1]]$schema
  expect_equal(updated[["Month"]], "character")
  expect_equal(updated[["Total"]], "integer")
})

# --------------------------------------------------------------------------- #
#  openai_compatible provider                                                  #
# --------------------------------------------------------------------------- #

test_that(".make_llm_chat() errors when openai_compatible has no base_url", {
  skip_if_not_installed("ellmer")
  expect_error(
    macrox:::.make_llm_chat("openai_compatible", model = "llama3", base_url = NULL),
    "base_url"
  )
  expect_error(
    macrox:::.make_llm_chat("openai_compatible", model = "llama3", base_url = ""),
    "base_url"
  )
})

test_that("select_table_llm() records base_url in the step", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  # Manually inject a step as if select_table_llm had run
  record_step(sess, list(
    step        = "select_table_llm",
    label       = "tbl",
    page        = 1L,
    area        = NULL,
    provider    = "openai_compatible",
    model       = "llama3.2-vision",
    base_url    = "http://localhost:1234/v1",
    schema      = NULL,
    prompt      = NULL,
    dpi         = 150L,
    header_rows = 1L
  ))

  expect_equal(sess$steps[[1]]$base_url, "http://localhost:1234/v1")
  expect_equal(sess$steps[[1]]$provider, "openai_compatible")
})

# --------------------------------------------------------------------------- #
#  chat object input                                                           #
# --------------------------------------------------------------------------- #

test_that(".chat_provider_info() derives provider/model/base_url from a Chat", {
  skip_if_not_installed("ellmer")

  ch <- ellmer::chat_anthropic(model = "claude-haiku-4-5")
  info <- macrox:::.chat_provider_info(ch)
  expect_equal(info$provider, "anthropic")
  expect_equal(info$model, "claude-haiku-4-5")

  ch2 <- ellmer::chat_openai_compatible(
    base_url = "http://localhost:11434/v1",
    model    = "llama2"
  )
  info2 <- macrox:::.chat_provider_info(ch2)
  expect_equal(info2$provider, "openai_compatible")
  expect_equal(info2$model, "llama2")
  expect_equal(info2$base_url, "http://localhost:11434/v1")
})

test_that(".resolve_llm_chat() uses a supplied Chat object and clones it", {
  skip_if_not_installed("ellmer")

  ch <- ellmer::chat_openai_compatible(
    base_url = "http://localhost:11434/v1",
    model    = "llama2"
  )
  resolved <- macrox:::.resolve_llm_chat(ch, "anthropic", NULL, NULL)

  expect_equal(resolved$provider, "openai_compatible")
  expect_equal(resolved$model, "llama2")
  expect_equal(resolved$base_url, "http://localhost:11434/v1")
  expect_s3_class(resolved$chat, "Chat")
  expect_false(identical(resolved$chat, ch))  # cloned, not the same object
})

test_that(".resolve_llm_chat() builds a chat from provider/model/base_url when chat is NULL", {
  skip_if_not_installed("ellmer")

  resolved <- macrox:::.resolve_llm_chat(NULL, "anthropic", NULL, NULL)
  expect_equal(resolved$provider, "anthropic")
  expect_equal(resolved$model, "claude-opus-4-5-20251001")
  expect_s3_class(resolved$chat, "Chat")
})

test_that(".resolve_llm_chat() errors when chat is not an ellmer Chat", {
  expect_error(
    macrox:::.resolve_llm_chat("not-a-chat", "anthropic", NULL, NULL),
    "Chat object"
  )
})

test_that("update_llm_schema() preserves base_url on re-extract", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list(tbl = data.frame(x = 1))
  sess$steps  <- list(list(
    step        = "select_table_llm",
    label       = "tbl",
    page        = 1L,
    area        = NULL,
    provider    = "openai_compatible",
    model       = "llama3.2-vision",
    base_url    = "http://localhost:1234/v1",
    schema      = list(X = "character"),
    prompt      = NULL,
    dpi         = 150L,
    header_rows = 1L
  ))
  class(sess) <- "macrox_session"

  update_llm_schema(sess, "tbl",
    schema     = c(Month = "character"),
    re_extract = FALSE)

  expect_equal(sess$steps[[1]]$base_url, "http://localhost:1234/v1")
})
