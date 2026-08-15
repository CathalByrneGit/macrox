# Update the prompt for a recorded select_item step and re-extract

Update the prompt for a recorded select_item step and re-extract

## Usage

``` r
update_item(
  sess,
  label,
  prompt = NULL,
  cast = NULL,
  chat = NULL,
  re_extract = TRUE
)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Label of the item to update.

- prompt:

  New prompt string.

- cast:

  New cast type, or `NULL` to keep existing.

- chat:

  An existing `ellmer` Chat object to use for the re-extraction. See
  [`select_table_llm()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_llm.md).
  `NULL` (default) reuses the provider/model recorded for the step.

- re_extract:

  Re-run the LLM call immediately (default `TRUE`).

## Value

`sess` invisibly.
