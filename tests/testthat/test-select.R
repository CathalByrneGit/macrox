test_that(".forward_fill() fills empty strings from previous value", {
  expect_equal(
    pdfmacro:::.forward_fill(c("A", "", "", "B", "")),
    c("A", "A", "A", "B", "B")
  )
})

test_that(".forward_fill() leaves leading non-empty alone", {
  expect_equal(
    pdfmacro:::.forward_fill(c("X", "Y", "")),
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
  result <- pdfmacro:::.flatten_headers(df, header_rows = 2)
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
  result <- pdfmacro:::.flatten_headers(df, header_rows = 1)
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

  result <- pdfmacro:::.fuzzy_find_page(
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
    pdfmacro:::.fuzzy_find_page(fake_sess, "Very Specific Table Caption",
                                 "jw", max_dist = 0.05),
    "Fuzzy search failed"
  )
})
