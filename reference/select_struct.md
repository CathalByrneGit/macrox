# Extract structured records from a PDF page using GLiNER2

Uses GLiNER2's `extract_json` to extract a structured entity schema
(multiple fields, optional list fields, multiple records) from a PDF
page. Results are stored as a data frame in `sess$tables[[label]]`,
identical to tabulapdf / LLM table extraction — downstream steps and
exports work unchanged.

## Usage

``` r
select_struct(
  sess,
  label,
  entity = label,
  fields,
  list_fields = character(0),
  enum_fields = character(0),
  page = 1L,
  gliner_model = "fastino/gliner2-base-v1"
)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Table label to store results under.

- entity:

  Entity type name used in the GLiNER2 schema. Defaults to `label`.

- fields:

  Named character vector: `field_name = "description"`. Every name
  becomes a column in the resulting data frame.

- list_fields:

  Character vector of field names that may contain multiple values
  (GLiNER2 `list` type). Multiple values are collapsed with `"; "`.
  Default `character(0)`.

- enum_fields:

  Named character vector of enumerated field types:
  `field_name = "[val1|val2|val3]"`. Restricts what the model extracts
  to the listed choices. Default `character(0)`.

- page:

  Integer or integer vector of pages to extract from. Multiple pages use
  a single `batch_extract_json` call. Default `1L`.

- gliner_model:

  GLiNER2 model identifier. Default `"fastino/gliner2-base-v1"`.

## Value

`sess` invisibly (step recorded).
