# Test extraction parameters and return a structured evaluation report

The R-Core Judge for the MCP agent loop. Runs a single extraction pass
with the given parameters and returns a structured report indicating
whether the result is clean or needs adjustment. The MCP
`test_parameters` tool calls this in a loop until `status == "success"`.

## Usage

``` r
test_extraction(
  file,
  page,
  area = NULL,
  method = c("bbox", "lattice", "stream", "llm"),
  header_rows = 1L,
  label = "target",
  provider = "anthropic",
  schema = NULL,
  preview_rows = 3L,
  ...
)
```

## Arguments

- file:

  Path to a PDF file.

- page:

  Page number (integer).

- area:

  Named `c(top, left, bottom, right)` in PDF points, or `NULL` for the
  full page.

- method:

  Extraction method: `"bbox"` (default), `"lattice"`, `"stream"`, or
  `"llm"`.

- header_rows:

  Number of header rows (default 1).

- label:

  Internal table label used during extraction (default `"target"`).

- provider:

  LLM provider, only used when `method = "llm"`.

- schema:

  Named character vector of column types, only used when
  `method = "llm"`.

- preview_rows:

  Number of data rows to include in the response preview (default 3).
  Keeps the agent context window small.

- ...:

  Additional arguments forwarded to the extraction function.

## Value

A list with:

- `status`:

  `"success"`, `"needs_alignment"`, `"empty"`, or `"crash"`.

- `metrics`:

  Structural quality metrics: row/col counts, unnamed headers, empty
  columns, header-spill detection, likely-numeric character columns.

- `guidance`:

  Plain-English action string for the agent when status is not
  `"success"`.

- `preview`:

  First `preview_rows` rows as a list of named lists for JSON
  serialisation.

## Details

Returns a list that serialises cleanly to JSON via
[`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
