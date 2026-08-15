# Scan a PDF for tables

Scans the specified pages of the PDF for tables and prints a summary.
This function is NOT recorded in the macro — it is for exploration only.

## Usage

``` r
detect_tables(
  sess,
  pages = NULL,
  method = c("lattice", "stream", "docling"),
  min_rows = 1L,
  max_header_chars = 40L
)
```

## Arguments

- sess:

  A `macrox_session` object.

- pages:

  Integer vector of pages to scan. Defaults to all pages.

- method:

  Extraction method: `"lattice"` (default), `"stream"`, or `"docling"`.
  Docling converts all requested pages in one pass and is best for
  scanned PDFs or complex layouts.

- min_rows:

  Minimum number of data rows a table must have to be shown and stored.
  Default `1`. Set to `0` to see everything.

- max_header_chars:

  Maximum character length allowed for the first column name before the
  table is flagged as a likely chart/figure and skipped. Default `40`.
  Set to `Inf` to disable. (Lattice/stream only — Docling's layout model
  distinguishes charts from tables natively.)

## Value

`sess` invisibly (not recorded).

## Details

Results are stored in `sess$detect` as a list of `list(page, index, df)`
entries that can be passed directly to
[`select_table()`](https://cathalbyrnegit.github.io/macrox/reference/select_table.md)
via `table_index`. When `method = "docling"`, pages are converted in a
single pass and the results are cached so that subsequent
[`select_table_docling()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_docling.md)
calls on those pages run instantly.
