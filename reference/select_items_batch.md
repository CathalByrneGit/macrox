# Extract multiple metadata fields in one GLiNER model pass

Runs a single GLiNER2 inference call to extract several named fields
from a PDF, which is significantly faster than calling
[`select_item()`](https://cathalbyrnegit.github.io/macrox/reference/select_item.md)
once per field. Each field is recorded as a separate `select_item` step
so macros replay with the standard `select_item` mechanism.

## Usage

``` r
select_items_batch(
  sess,
  items,
  page = NULL,
  cast = NULL,
  gliner_model = "fastino/gliner2-base-v1",
  all_matches = FALSE
)
```

## Arguments

- sess:

  A `macrox_session` object.

- items:

  Named character vector: names are field labels, values are
  natural-language prompts describing the field. E.g.
  `c(invoice_no = "Invoice ID or reference number", total = "Grand total amount payable")`.

- page:

  Page number to extract from. `NULL` (default) concatenates all pages'
  text.

- cast:

  Named character vector of cast types keyed by label. Unlisted fields
  default to `"character"`. Supported types: `"character"`, `"numeric"`,
  `"integer"`, `"date:<fmt>"` (e.g. `"date:%d/%m/%Y"`).

- gliner_model:

  GLiNER2 model identifier. Default `"fastino/gliner2-base-v1"` (205M);
  use `"fastino/gliner2-large-v1"` for higher accuracy.

- all_matches:

  If `TRUE`, return every occurrence found per label as a character
  vector. Labels listed in `cast` are still applied element-wise.
  Default `FALSE`.

## Value

`sess` invisibly (one `select_item` step recorded per field).

## Details

GLiNER2 can return multiple occurrences of the same entity type when
they are present in the text; `select_items_batch` surfaces the first
match per field (the same behaviour as
[`select_item()`](https://cathalbyrnegit.github.io/macrox/reference/select_item.md)).
