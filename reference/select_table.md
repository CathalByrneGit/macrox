# Extract a table from the PDF and record the step

Three selection modes: by page + index, by bounding box (`area`), or by
fuzzy caption match (`label_match`). These can be mixed across different
tables in the same session.

## Usage

``` r
select_table(
  sess,
  label,
  page = NULL,
  table_index = 1,
  area = NULL,
  label_match = NULL,
  method = c("lattice", "stream", "bbox"),
  header_rows = 1,
  fuzzy_method = "jw",
  max_dist = 0.2,
  row_tol = NULL,
  col_gap = NULL
)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Character label for the extracted table.

- page:

  Page number (integer). Required unless `label_match` is supplied.

- table_index:

  Which table on the page (1-based, tabulapdf methods only). Default 1.

- area:

  Named numeric vector `c(top, left, bottom, right)` in PDF points. Use
  [`locate_area()`](https://cathalbyrnegit.github.io/macrox/reference/locate_area.md)
  to capture interactively. Strongly recommended with `method = "bbox"`
  to isolate the table from surrounding charts/text.

- label_match:

  Text caption to fuzzy-search across all pages.

- method:

  Extraction engine:

  - `"lattice"` (default) — tabulapdf, best for grid-lined tables.

  - `"stream"` — tabulapdf, best for whitespace-separated tables.

  - `"bbox"` —
    [`pdftools::pdf_data()`](https://docs.ropensci.org/pdftools//reference/pdftools.html)
    word-position engine. Handles PDFs where tabulapdf fails: charts on
    the same page, missing grid lines, numbers running together. Use
    with `area` to clip to the table region.

- header_rows:

  Number of header rows to merge (default 1). Use 2+ for multi-row
  spanning headers.

- fuzzy_method:

  stringdist method for caption matching (default `"jw"`).

- max_dist:

  Maximum normalised distance for a fuzzy match (default 0.2).

- row_tol:

  `bbox` method only. Vertical gap (pts) above which a new row is
  started. Default `NULL` auto-computes from median character height.

- col_gap:

  `bbox` method only. Horizontal gap (pts) between word groups that
  signals a new column. Default `NULL` auto-computes from median word
  width.

## Value

`sess` invisibly (step is recorded).
