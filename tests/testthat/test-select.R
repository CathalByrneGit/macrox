test_that(".forward_fill() fills empty strings from previous value", {
  expect_equal(
    macrox:::.forward_fill(c("A", "", "", "B", "")),
    c("A", "A", "A", "B", "B")
  )
})

test_that(".forward_fill() leaves leading non-empty alone", {
  expect_equal(
    macrox:::.forward_fill(c("X", "Y", "")),
    c("X", "Y", "Y")
  )
})

test_that(".flatten_headers() produces correct pasted column names", {
  df <- data.frame(
    V1 = c("Group A", "",    "Jan", "Feb"),
    V2 = c("Group A", "Sub1","1",   "2"),
    V3 = c("Group B", "Sub2","3",   "4"),
    stringsAsFactors = FALSE
  )
  result <- macrox:::.flatten_headers(df, header_rows = 2)
  expect_equal(ncol(result), 3L)
  expect_equal(nrow(result), 2L)  # 4 rows - 2 header rows
  # V1 forward-fills "Group A" into the blank, so col names should contain "Group"
  expect_true(any(grepl("Group", names(result))))
})

test_that(".flatten_headers() drops header rows from data", {
  df <- data.frame(
    V1 = c("Month", "Jan", "Feb", "Mar"),
    V2 = c("Count", "10",  "20",  "30"),
    stringsAsFactors = FALSE
  )
  result <- macrox:::.flatten_headers(df, header_rows = 1)
  expect_equal(nrow(result), 3L)
})

test_that(".fuzzy_find_page() finds best matching page", {
  # Mock session-like list with page text
  fake_sess <- list(
    text = list(
      "page one text without the caption",
      "Some header\nMart Movements by Breed\nCounty  Count  Total",
      "Another page entirely"
    )
  )

  result <- macrox:::.fuzzy_find_page(
    fake_sess,
    label_match  = "Mart Movements by Breed",
    fuzzy_method = "jw",
    max_dist     = 0.2
  )

  expect_equal(result$page, 2L)
  expect_lt(result$dist, 0.1)
})

test_that(".fuzzy_find_page() aborts when no match within max_dist", {
  fake_sess <- list(
    text = list("completely unrelated text here")
  )
  expect_error(
    macrox:::.fuzzy_find_page(fake_sess, "Very Specific Table Caption",
                                 "jw", max_dist = 0.05),
    "Fuzzy search failed"
  )
})


# --------------------------------------------------------------------------- #
#  stack_pages()                                                               #
# --------------------------------------------------------------------------- #

test_that("stack_pages() extracts and row-binds across pages", {
  page_data <- list(
    data.frame(Name = c("Alice", "Bob"),   Score = c("90", "80"),
               stringsAsFactors = FALSE),
    data.frame(Name = c("Carol", "Dave"),  Score = c("70", "60"),
               stringsAsFactors = FALSE),
    data.frame(Name = c("Eve"),            Score = c("95"),
               stringsAsFactors = FALSE)
  )
  call_idx <- 0L

  testthat::local_mocked_bindings(
    select_table = function(sess, label, page, ...) {
      call_idx <<- call_idx + 1L
      sess$tables[[label]] <- page_data[[call_idx]]
      invisible(sess)
    }
  )

  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  stack_pages(sess, "all_scores", pages = c(1L, 2L, 3L), method = "bbox")

  expect_true("all_scores" %in% names(sess$tables))
  df <- sess$tables[["all_scores"]]
  expect_equal(nrow(df), 5L)
  expect_equal(names(df), c("Name", "Score"))
})

test_that("stack_pages() records a single step", {
  testthat::local_mocked_bindings(
    select_table = function(sess, label, page, ...) {
      sess$tables[[label]] <- data.frame(x = 1:2, stringsAsFactors = FALSE)
      invisible(sess)
    }
  )

  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  stack_pages(sess, "combined", pages = c(1L, 2L), method = "bbox")

  expect_length(sess$steps, 1L)
  s <- sess$steps[[1]]
  expect_equal(s$step,  "stack_pages")
  expect_equal(s$label, "combined")
  expect_equal(s$pages, c(1L, 2L))
})

test_that("stack_pages() errors with fewer than 2 pages", {
  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  expect_error(stack_pages(sess, "tbl", pages = 1L), "at least 2")
})

test_that("stack_pages() continues when a page fails", {
  call_idx <- 0L
  testthat::local_mocked_bindings(
    select_table = function(sess, label, page, ...) {
      call_idx <<- call_idx + 1L
      if (call_idx == 2L) stop("extraction failed")
      sess$tables[[label]] <- data.frame(a = call_idx, stringsAsFactors = FALSE)
      invisible(sess)
    }
  )

  sess <- new.env(parent = emptyenv())
  sess$path   <- "dummy.pdf"
  sess$tables <- list()
  sess$steps  <- list()
  class(sess) <- "macrox_session"

  expect_warning(
    stack_pages(sess, "partial", pages = c(1L, 2L, 3L), method = "bbox"),
    "skipped"
  )
  expect_true("partial" %in% names(sess$tables))
  expect_equal(nrow(sess$tables$partial), 2L)
})


# --------------------------------------------------------------------------- #
#  stack_pages() — LLM method                                                  #
# --------------------------------------------------------------------------- #

test_that("stack_pages() with method='llm' records step with LLM params", {
  skip_if_not_installed("ellmer")

  call_count <- 0L
  testthat::local_mocked_bindings(
    select_table_llm = function(sess, label, page, ...) {
      call_count <<- call_count + 1L
      sess$tables[[label]] <- data.frame(
        Month = c("Jan", "Feb"), Total = c("10", "20"),
        stringsAsFactors = FALSE
      )
      invisible(sess)
    }
  )

  sess <- new.env(parent = emptyenv())
  sess$path       <- "dummy.pdf"
  sess$tables     <- list()
  sess$steps      <- list()
  sess$llm_config <- NULL
  class(sess)     <- "macrox_session"

  ch <- ellmer::chat_openai_compatible(base_url = "http://localhost:11434/v1", model = "llava")
  stack_pages(sess, "multi_page", pages = c(5L, 6L), method = "llm",
              chat   = ch,
              schema = c(Month = "character", Total = "integer"))

  expect_equal(call_count, 2L)
  expect_true("multi_page" %in% names(sess$tables))
  df <- sess$tables[["multi_page"]]
  expect_equal(nrow(df), 4L)

  s <- sess$steps[[1]]
  expect_equal(s$step,   "stack_pages")
  expect_equal(s$method, "llm")
  expect_equal(s$provider, "openai_compatible")
  expect_equal(s$model,    "llava")
  expect_equal(unlist(s$schema), c(Month = "character", Total = "integer"))
})

test_that("stack_pages() with method='llm' aborts when all pages fail", {
  skip_if_not_installed("ellmer")

  testthat::local_mocked_bindings(
    select_table_llm = function(sess, label, page, ...) stop("LLM error")
  )

  sess <- new.env(parent = emptyenv())
  sess$path       <- "dummy.pdf"
  sess$tables     <- list()
  sess$steps      <- list()
  sess$llm_config <- NULL
  class(sess)     <- "macrox_session"

  ch <- ellmer::chat_openai_compatible(base_url = "http://localhost:11434/v1", model = "llava")
  expect_error(
    suppressWarnings(
      stack_pages(sess, "bad", pages = c(1L, 2L), method = "llm", chat = ch)
    ),
    "no data on any"
  )
})
