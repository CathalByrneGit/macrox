test_that("blockr entry points fail clearly when blockr.core is missing", {
  skip_if(requireNamespace("blockr.core", quietly = TRUE),
          "blockr.core is installed")
  expect_error(new_pdf_table_block(), "blockr.core")
  expect_error(new_split_column_block(), "blockr.core")
  expect_error(new_clean_numbers_block(), "blockr.core")
  expect_error(register_macrox_blocks(), "blockr.core")
  expect_error(mx_blockr_app(), "blockr.core")
})

test_that("macrox blocks can be constructed", {
  skip_if_not_installed("blockr.core")
  expect_s3_class(new_pdf_table_block("a.pdf", page = 2L), "pdf_table_block")
  expect_s3_class(new_pdf_table_block(), "data_block")
  expect_s3_class(new_split_column_block(), "split_column_block")
  expect_s3_class(new_split_column_block(), "transform_block")
  expect_s3_class(new_clean_numbers_block(), "clean_numbers_block")
})

test_that("macrox blocks round-trip through blockr serialisation", {
  skip_if_not_installed("blockr.core")
  blk <- new_split_column_block(col = "x", into = "a, b", sep = "\\|")
  back <- blockr.core::blockr_deser(blockr.core::blockr_ser(blk))
  expect_s3_class(back, "split_column_block")
})

test_that("macrox blocks can be registered", {
  skip_if_not_installed("blockr.core")
  expect_silent(register_macrox_blocks())
})

with_data <- function(expr, data) {
  eval(do.call(bquote, list(expr, list(data = data))))
}

test_that("pdf table block returns state and an evaluable expression", {
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("pdftools")
  pdf <- test_path("..", "..", "AIM_Stats_Report_060525.pdf")
  skip_if_not(file.exists(pdf), "sample PDF not available")

  shiny::testServer(
    new_pdf_table_block(pdf, page = 9L, header_rows = 4L)$expr_server,
    expr = {
      expect_equal(session$returned$state$page(), 9L)
      expect_equal(session$returned$state$header_rows(), 4L)
      expect_match(deparse1(session$returned$expr()), "macrox::pdf_table", fixed = TRUE)
      res <- eval(session$returned$expr())
      expect_s3_class(res, "data.frame")
      expect_equal(dim(res), c(28L, 8L))
    }
  )
})

test_that("split column block builds and evaluates its expression", {
  skip_if_not_installed("blockr.core")
  df <- data.frame(county = c("a", "b"), m = c("1 2 3", "4 5 6"),
                   stringsAsFactors = FALSE)

  shiny::testServer(
    new_split_column_block()$expr_server,
    args = list(data = shiny::reactive(df)),
    expr = {
      session$setInputs(col = "m", into = "x, y, z", sep = "\\s+", keep = FALSE)
      expect_equal(session$returned$state$col(), "m")
      expect_equal(session$returned$state$into(), "x, y, z")
      out <- with_data(session$returned$expr(), df)
      expect_equal(names(out), c("county", "x", "y", "z"))
      expect_equal(out$y, c("2", "5"))
    }
  )
})

test_that("clean numbers block omits cols when none are chosen", {
  skip_if_not_installed("blockr.core")
  df <- data.frame(price = c("£1,000", "$2,000"), name = c("a", "b"),
                   stringsAsFactors = FALSE)

  shiny::testServer(
    new_clean_numbers_block()$expr_server,
    args = list(data = shiny::reactive(df)),
    expr = {
      session$setInputs(convert = TRUE)
      expect_false(grepl("cols", deparse1(session$returned$expr()), fixed = TRUE))
      expect_equal(with_data(session$returned$expr(), df)$price, c(1000, 2000))

      session$setInputs(cols = "price")
      expect_match(deparse1(session$returned$expr()), "cols = \"price\"", fixed = TRUE)
      expect_equal(with_data(session$returned$expr(), df)$name, c("a", "b"))
    }
  )
})

test_that("macrox blocks register under blockr's suggested categories", {
  skip_if_not_installed("blockr.core")
  expect_no_warning(register_macrox_blocks())
  avail <- blockr.core::available_blocks()
  expect_true(all(c("pdf_table_block", "split_column_block",
                    "clean_numbers_block") %in% names(avail)))
})
