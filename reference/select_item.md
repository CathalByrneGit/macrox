# Extract a single metadata field from a PDF

Extracts a single structured value from a PDF using either an LLM or a
local GLiNER2 model. Results are stored in `sess$items[[label]]` and
recorded as a step.

## Usage

``` r
select_item(
  sess,
  label,
  prompt,
  cast = NULL,
  page = NULL,
  area = NULL,
  backend = c("llm", "gliner"),
  chat = NULL,
  provider = getOption("macrox.llm.provider", "anthropic"),
  model = getOption("macrox.llm.model", NULL),
  base_url = NULL,
  dpi = 120L,
  gliner_model = "fastino/gliner2-base-v1",
  all_matches = FALSE
)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Character label for the item (e.g. `"invoice_number"`).

- prompt:

  Description of the field to extract. For the LLM backend this is the
  full instruction; for GLiNER it is the field description passed
  alongside `label` as the entity type.

- cast:

  Optional type to cast the returned string to: `"character"` (default),
  `"numeric"`, `"integer"`, or `"date:<fmt>"` e.g. `"date:%d/%m/%Y"`.

- page:

  Page number. For `backend = "llm"`, `NULL` sends all page text and a
  non-NULL value renders an image. For `backend = "gliner"`, `NULL` uses
  all pages' text and a non-NULL value uses only that page's text (no
  image rendering).

- area:

  Named `c(top, left, bottom, right)` in PDF points. Only used when
  `backend = "llm"` and `page` is non-NULL.

- backend:

  Extraction backend: `"llm"` (default, requires ellmer) or `"gliner"`
  (local GLiNER2 model, requires reticulate + gliner2 Python package
  installed via
  [`setup_gliner()`](https://cathalbyrnegit.github.io/macrox/reference/setup_gliner.md)).

- chat:

  An existing `ellmer` Chat object. Only used when `backend = "llm"`.
  See
  [`select_table_llm()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_llm.md).

- provider:

  LLM provider (default `"anthropic"`). Only used when `backend = "llm"`
  and `chat` is `NULL`.

- model:

  Model name. Only used when `backend = "llm"`.

- base_url:

  Base URL for `"openai_compatible"` providers.

- dpi:

  Render resolution when `backend = "llm"` and `page` is set (default
  120).

- gliner_model:

  GLiNER2 model name (HuggingFace repo). Only used when
  `backend = "gliner"`. Default `"fastino/gliner2-base-v1"` (205M
  params); use `"fastino/gliner2-large-v1"` for higher accuracy.

- all_matches:

  Logical; only used when `backend = "gliner"`. If `TRUE`, return every
  occurrence of the entity found in the text as a character vector
  instead of just the first match. Default `FALSE`.

## Value

`sess` invisibly (step is recorded).

## Examples

``` r
if (FALSE) { # \dontrun{
# LLM backend (default)
sess |> select_item("invoice_number",
  prompt = "Extract the invoice ID or reference number.")

# GLiNER backend — local, no API key needed
setup_gliner()
sess |> select_item("invoice_number",
  prompt  = "Invoice ID or reference number",
  backend = "gliner")

# GLiNER on a specific page
sess |> select_item("total_amount",
  prompt  = "Grand total amount payable",
  page    = 1L,
  cast    = "numeric",
  backend = "gliner")
} # }
```
