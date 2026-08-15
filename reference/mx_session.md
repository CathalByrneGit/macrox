# Open a macrox session

Creates a session object that holds the PDF path, extracted tables, and
a running log of every recorded step.

## Usage

``` r
mx_session(path = NULL)
```

## Arguments

- path:

  Path to a PDF file, or an image file (PNG, JPG, TIFF, BMP, GIF, WEBP).
  Image files are automatically converted to PDF; only
  [`select_table_llm()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_llm.md)
  and
  [`select_table_docling()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_docling.md)
  work on the resulting session.

## Value

An invisible `macrox_session` environment.
