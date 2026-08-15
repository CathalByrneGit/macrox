# Export session data as a structured JSON payload

Combines extracted items (document metadata) and tables into a single
self-describing JSON document. Suitable for API ingestion, database
loading, and RAG pipelines.

## Usage

``` r
export_json(sess, path = NULL, pretty = TRUE, include_steps = FALSE)
```

## Arguments

- sess:

  A `macrox_session` object.

- path:

  File path to write the JSON. `NULL` (default) returns the JSON string
  invisibly without writing.

- pretty:

  Logical; pretty-print the JSON (default `TRUE`).

- include_steps:

  Include the recorded step list in the output (default `FALSE`).

## Value

The JSON string, invisibly.
