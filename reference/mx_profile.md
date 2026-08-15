# Profile a PDF for agent consumption

Returns a structured machine-readable description of the PDF's tables
and surrounding text context. Pass the result to an LLM to generate a
macro.

## Usage

``` r
mx_profile(path, pages = NULL, method = "lattice", context_lines = 3)
```

## Arguments

- path:

  Path to a PDF file.

- pages:

  Integer vector of pages to scan. Defaults to all pages (capped at 60).

- method:

  Extraction method: `"lattice"` (default) or `"stream"`.

- context_lines:

  Number of text lines above/below each table's header row to include as
  context (default 3).

## Value

A `macrox_profile` list.
