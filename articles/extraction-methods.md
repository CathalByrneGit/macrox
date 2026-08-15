# Extraction Methods

macrox ships five extraction engines. They can be mixed freely within
the same session and macro — use whichever gives the cleanest result for
each table.

------------------------------------------------------------------------

## Choosing a method

| Situation                           | Recommended                                                                                               |
|-------------------------------------|-----------------------------------------------------------------------------------------------------------|
| Clean PDF with visible grid lines   | `lattice`                                                                                                 |
| Whitespace-aligned columns, no grid | `stream`                                                                                                  |
| Charts and tables on the same page  | `bbox` + `area =`                                                                                         |
| Numbers running together            | `bbox` + `col_gap =`                                                                                      |
| Scanned PDF, no selectable text     | `docling` or `llm`                                                                                        |
| Complex layout, must work offline   | `docling`                                                                                                 |
| Multi-level / spanning headers      | `llm` with `schema =`                                                                                     |
| Highly irregular layout             | `llm`                                                                                                     |
| Multi-page table, complex layout    | [`stack_pages()`](https://cathalbyrnegit.github.io/macrox/reference/stack_pages.md) with `method = "llm"` |
| No API key / air-gapped             | `bbox`, tabulapdf, or `docling`                                                                           |

Start with
[`detect_tables()`](https://cathalbyrnegit.github.io/macrox/reference/detect_tables.md)
to see what the engines find automatically:

``` r
library(macrox)

sess <- mx_session("report.pdf")
sess |> detect_tables(pages = 1:20)
```

------------------------------------------------------------------------

## bbox — word-position engine

Uses
[`pdftools::pdf_data()`](https://docs.ropensci.org/pdftools//reference/pdftools.html)
word-level bounding boxes. No Java required. Works on any PDF with
selectable text.

**Best for:** PDFs with charts sharing a page with tables, missing grid
lines, or numbers that run together without whitespace.

``` r
area <- sess |> locate_area(page = 8)
sess |> select_table("monthly", page = 8, area = area, method = "bbox")
```

[`locate_area()`](https://cathalbyrnegit.github.io/macrox/reference/locate_area.md)
opens an interactive click-drag selector on a rendered page image. The
returned numeric vector `c(top, left, bottom, right)` can be hardcoded
into the macro.

### Tuning bbox

Two parameters control how words are grouped into rows and columns:

| Parameter | Default | Effect                                                                                    |
|-----------|---------|-------------------------------------------------------------------------------------------|
| `col_gap` | auto    | Minimum horizontal gap (pts) between columns. Increase if columns are merging.            |
| `row_tol` | auto    | Vertical tolerance (pts) for grouping words into the same row. Increase for slanted text. |

``` r
sess |> select_table("financials", page = 12, method = "bbox",
  area    = c(top = 80, left = 30, bottom = 500, right = 750),
  col_gap = 20,   # widen if two columns merge into one
  row_tol = 3)    # increase if words on the same row are misaligned
```

### Multi-row headers with bbox

Government PDFs often have two- or three-row header structures. Use
`header_rows` to flatten them:

``` r
# Two-row header: group label in row 1, sub-labels in row 2
sess |> select_table("breed_sire", page = 16, area = area,
  method = "bbox", header_rows = 2)
# Column names become e.g. "Group_A.Jan", "Group_A.Feb", "Group_B.Sub1"
```

------------------------------------------------------------------------

## lattice and stream — tabulapdf engines

Wrappers around [tabulapdf](https://github.com/ropensci/tabulapdf).
Require **Java** and the `tabulapdf` package (listed in Suggests).

| Engine    | Use when                                    |
|-----------|---------------------------------------------|
| `lattice` | PDF has visible ruling lines between cells  |
| `stream`  | Columns are whitespace-aligned with no grid |

``` r
sess |> select_table("calf_monthly", page = 8, method = "lattice")
sess |> select_table("roster",       page = 9, method = "stream")

# By table index when multiple tables are on the same page
sess |> select_table("sire_beef",  page = 15, table_index = 1)
sess |> select_table("sire_dairy", page = 15, table_index = 2)
```

If Java is not available, macrox falls back to `bbox` with a warning.

------------------------------------------------------------------------

## docling — ML layout engine

Uses [Docling](https://ds4sd.github.io/docling/) (IBM Research) deep
learning models. Detects and extracts tables from scanned PDFs and
complex multi-column layouts where positional methods fail. Runs **fully
offline** after a one-time model download (~2–3 GB).

### One-time setup

``` r
setup_docling()   # installs Docling into a managed Python virtualenv
# Restart R before first use
```

### Usage

``` r
# Scan for tables using Docling's ML detection
sess |> detect_tables(pages = 1:20, method = "docling")

# Extract a specific table (by table_index on that page)
sess |> select_table_docling("financials", page = 10, table_index = 1)

# Release the in-memory document cache when done
close_docling()
```

Docling caches converted pages in memory, so extracting multiple tables
from the same page is fast. Call
[`close_docling()`](https://cathalbyrnegit.github.io/macrox/reference/close_docling.md)
when the session is complete to free memory.

### Docling vs LLM

Both handle scanned PDFs. Choose Docling when:

- You need **offline / air-gapped** operation
- You process **many pages** (Docling is faster than LLM at scale)
- Layout is complex but headers are **single-row**

Choose LLM when:

- Headers are **multi-level or spanning** and the schema matters
- The table occupies an **irregular bounding box**
- You already have an LLM provider set up

------------------------------------------------------------------------

## Fuzzy caption matching

Instead of specifying a page number, point macrox at a caption and it
finds the table for you:

``` r
sess |> select_table("mart_breed",
  label_match  = "Mart Movements by Breed",
  fuzzy_method = "jw",     # Jaro-Winkler (default), also "lv", "dl", "cosine"
  max_dist     = 0.2)      # maximum acceptable distance (0 = exact, 1 = anything)
```

Useful when page numbers shift between report editions. macrox scans
`pdf_text()` output for each page, scores each against the target
caption, and selects the best match within `max_dist`.

------------------------------------------------------------------------

## Multi-page tables

When the same table continues across several consecutive pages,
[`stack_pages()`](https://cathalbyrnegit.github.io/macrox/reference/stack_pages.md)
extracts from each page and row-binds the results.

``` r
sess |> stack_pages("breed_all", pages = 10:14,
  area   = c(top = 80, left = 30, bottom = 700, right = 550),
  method = "bbox")
```

By default, `header_match = TRUE` — rows on pages 2+ that look like the
header row (matching column names) are discarded automatically. Turn
this off if the table has no repeated header:

``` r
sess |> stack_pages("breed_all", pages = 10:14,
  method       = "bbox",
  header_match = FALSE)
```

For multi-page tables with complex or irregular headers, use the LLM
method — see the [LLM
Integration](https://cathalbyrnegit.github.io/macrox/articles/llm-integration.md)
vignette.

------------------------------------------------------------------------

## Preview and inspect

After any extraction:

``` r
sess |> preview("monthly")          # head(6) with column names
sess |> preview_all()               # all tables, column stats, totals
```

Use
[`view_in_pdf()`](https://cathalbyrnegit.github.io/macrox/reference/view_in_pdf.md)
to highlight a step’s bounding box on the PDF page:

``` r
sess |> view_in_pdf(step = 1)       # step index from show_steps()
```
