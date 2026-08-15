# Update the schema for a recorded LLM extraction step and re-extract

Finds the most recent `select_table_llm` step for `label`, replaces its
schema, and re-runs the extraction against the same page/area. Useful
when the auto-detected columns need renaming or the types need
adjustment.

## Usage

``` r
update_llm_schema(
  sess,
  label,
  schema,
  prompt = NULL,
  chat = NULL,
  re_extract = TRUE
)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Label of the table to update.

- schema:

  New named character vector:
  `c(Month = "character", Male = "integer")`.

- prompt:

  Replacement prompt, or `NULL` to keep the existing one.

- chat:

  An existing `ellmer` Chat object to use for the re-extraction. See
  [`select_table_llm()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_llm.md).
  `NULL` (default) reuses the provider/model recorded for the step.

- re_extract:

  Re-run the LLM call immediately (default `TRUE`).

## Value

`sess` invisibly.
