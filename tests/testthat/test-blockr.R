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
