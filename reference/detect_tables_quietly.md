# Detect tables without console output

Like
[`detect_tables()`](https://cathalbyrnegit.github.io/macrox/reference/detect_tables.md)
but returns a named list silently. Intended for agent/programmatic use.

## Usage

``` r
detect_tables_quietly(
  path,
  pages = NULL,
  method = c("lattice", "stream", "docling"),
  min_rows = 1L,
  max_header_chars = 40L
)
```

## Arguments

- path:

  Path to a PDF file.

- pages:

  Integer vector of pages to scan. Defaults to all pages.

- method:

  Extraction method: `"lattice"` (default), `"stream"`, or `"docling"`.

- min_rows:

  Minimum data rows required (default `1`).

- max_header_chars:

  Maximum first-column name length before skipping (default `40`).
  Ignored when `method = "docling"`.

## Value

Named list: page number (character) → list of data frames.
