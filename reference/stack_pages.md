# Extract and stack the same table across multiple PDF pages

Extracts a table from each page in `pages` and row-binds them into a
single data frame. Supports all extraction engines including `"llm"`.

## Usage

``` r
stack_pages(
  sess,
  label,
  pages,
  area = NULL,
  method = c("bbox", "lattice", "stream", "llm"),
  chat = NULL,
  provider = getOption("macrox.llm.provider", "anthropic"),
  model = getOption("macrox.llm.model", NULL),
  base_url = NULL,
  schema = NULL,
  prompt = NULL,
  dpi = 150L,
  mode = c("structured", "page"),
  table_index = 1L,
  header_rows = 1L,
  header_match = TRUE,
  row_tol = NULL,
  col_gap = NULL
)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Character label for the combined table.

- pages:

  Integer vector of page numbers to extract and stack (minimum 2).

- area:

  Named `c(top, left, bottom, right)` in PDF points, or `NULL` for the
  full page. Applied identically to every page.

- method:

  Extraction engine: `"bbox"` (default), `"lattice"`, `"stream"`, or
  `"llm"`.

- chat:

  An existing `ellmer` Chat object (LLM method only). See
  [`select_table_llm()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_llm.md).

- provider:

  LLM provider (LLM method only, default `"anthropic"`). Ignored when
  `chat` is supplied. Reads `getOption("macrox.llm.provider")` when set.

- model:

  Model name (LLM method only). `NULL` uses the provider default.
  Ignored when `chat` is supplied. Reads `getOption("macrox.llm.model")`
  when set.

- base_url:

  Base URL for `"openai_compatible"` providers (LLM only).

- schema:

  Named character vector of column types, e.g.
  `c(Month = "character", Total = "integer")` (LLM method only). `NULL`
  asks the model to auto-detect columns — a consistent schema is
  strongly recommended for multi-page stacking.

- prompt:

  Extra instructions appended to the base prompt (LLM only).

- dpi:

  Render resolution for page images (LLM only, default 150).

- mode:

  LLM extraction mode: `"structured"` (default) or `"page"`. See
  [`select_table_llm()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_llm.md).

- table_index:

  Which table to pick when `mode = "page"` returns multiple tables (LLM
  only, default 1).

- header_rows:

  Number of header rows (non-LLM methods, default 1).

- header_match:

  Logical (non-LLM methods, default `TRUE`). When `TRUE` repeated
  headers are consumed as column names on every page.

- row_tol:

  `bbox` method only.

- col_gap:

  `bbox` method only.

## Value

`sess` invisibly (step is recorded).

## Details

For `"bbox"`, `"lattice"`, and `"stream"`: repeated header rows are
handled automatically when `header_match = TRUE` (default) — the header
is consumed as column names on each page and dropped from the data. Set
`header_match = FALSE` when pages 2+ start directly with data.

For `"llm"`: the model returns schema-defined columns directly so there
are no repeated headers to consume; `header_match` is ignored. Supply
`schema` for best accuracy and consistent column names across pages.
