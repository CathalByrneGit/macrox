# LLM Integration

LLM extraction is optional — macrox works fully offline without it.
Enable it when positional methods fail: multi-level headers, irregular
bounding boxes, or scanned PDFs where Docling is not available.

Everything here requires the `ellmer` package and at least one provider
API key.

------------------------------------------------------------------------

## Provider setup

Install ellmer once:

``` r
install.packages("ellmer")
```

Set your API key as an environment variable. The easiest place is
`.Renviron` (one key per provider — add only the one you use):

``` sh
ANTHROPIC_API_KEY=sk-ant-...
OPENAI_API_KEY=sk-...
GEMINI_API_KEY=AI...
```

Edit `.Renviron` from R:

``` r
usethis::edit_r_environ()   # requires usethis; restart R after saving
```

### Setting global defaults

Add these lines to `.Rprofile` so you never have to specify `provider`
or `model` on each call:

``` r
options(
  macrox.llm.provider = "anthropic",
  macrox.llm.model    = "claude-opus-4-8"
)
```

Supported providers: `"anthropic"`, `"openai"`, `"google_gemini"`,
`"openrouter"`, `"groq"`, `"ollama"`, `"openai_compatible"`, and any
other provider for which `ellmer` exports a `chat_<provider>()`
constructor.

------------------------------------------------------------------------

## Session-level configuration

Call
[`mx_configure_llm()`](https://cathalbyrnegit.github.io/macrox/reference/mx_configure_llm.md)
once per session to avoid repeating provider/model on every extraction
call:

``` r
library(macrox)

sess <- mx_session("report.pdf")

# Option 1: provider + model (uses global options as defaults)
sess |> mx_configure_llm(provider = "anthropic", model = "claude-opus-4-8")

# Option 2: pass a fully-configured ellmer Chat object for maximum control
chat <- ellmer::chat_anthropic(
  model  = "claude-opus-4-8",
  system = "Extract data exactly as shown in the document."
)
sess |> mx_configure_llm(chat = chat)
```

Once configured, all subsequent
[`select_table_llm()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_llm.md)
and
[`select_item()`](https://cathalbyrnegit.github.io/macrox/reference/select_item.md)
calls on the session use that provider without further arguments.

### Local models via Ollama

Run models locally — no API key needed:

``` r
sess |> mx_configure_llm(
  provider = "ollama",
  model    = "llama3.2-vision:11b"   # any Ollama vision model
)
```

### OpenAI-compatible endpoints

Works with LM Studio, vLLM, and any server that speaks the OpenAI API:

``` r
sess |> mx_configure_llm(
  provider = "openai_compatible",
  model    = "mistral-nemo",
  base_url = "http://localhost:1234/v1"
)
```

------------------------------------------------------------------------

## Extracting tables with select_table_llm()

Two extraction modes:

| Mode                     | When to use                                              |
|--------------------------|----------------------------------------------------------|
| `"structured"` (default) | You know the columns; supply a `schema` for typed output |
| `"page"`                 | Unknown layout; LLM converts the full page to markdown   |

### Structured mode

``` r
area <- sess |> locate_area(page = 5)

sess |> select_table_llm("livestock",
  page   = 5,
  area   = area,
  schema = c(
    Breed   = "character",
    Male    = "integer",
    Female  = "integer",
    Total   = "integer"
  )
)
```

`schema` is a named character vector: names become column names, values
are R types (`"character"`, `"integer"`, `"numeric"`). The LLM receives
the column names as guidance; macrox converts the returned strings to
the declared types.

### Page mode

``` r
# No area or schema needed — the LLM finds and parses all tables
sess |> select_table_llm("summary",
  page = 3,
  mode = "page"
)

# Multiple tables on one page — pick by index
sess |> select_table_llm("appendix",
  page        = 3,
  mode        = "page",
  table_index = 2
)
```

### Extra instructions

Use `prompt` to add free-text guidance after the base prompt:

``` r
sess |> select_table_llm("financials",
  page   = 12,
  schema = c(Quarter = "character", Revenue = "numeric", Cost = "numeric"),
  prompt = "Ignore the 'YTD' row at the bottom. Revenue and Cost are in thousands."
)
```

### Multi-level headers

Government tables often have two header rows. `header_rows = 2` tells
the LLM to flatten them with `_` separators:

``` r
sess |> select_table_llm("breed_sire",
  page        = 16,
  area        = area,
  header_rows = 2,
  schema      = c(
    Group_A.Jan = "integer",
    Group_A.Feb = "integer",
    Group_B.Sub1 = "integer"
  )
)
```

### Updating a schema after extraction

If the auto-detected columns need adjusting, use
[`update_llm_schema()`](https://cathalbyrnegit.github.io/macrox/reference/update_llm_schema.md)
to revise without re-browsing:

``` r
# After initial extraction, refine column names and types
sess |> update_llm_schema("livestock",
  schema = c(
    Breed    = "character",
    Bulls    = "integer",
    Cows     = "integer",
    Heifers  = "integer",
    Total    = "integer"
  )
)

# Revise the prompt too
sess |> update_llm_schema("livestock",
  schema = c(Breed = "character", Total = "integer"),
  prompt = "Only extract the Breed and Total columns."
)
```

------------------------------------------------------------------------

## Extracting metadata fields with select_item()

[`select_item()`](https://cathalbyrnegit.github.io/macrox/reference/select_item.md)
extracts a single value that sits outside any table — a publication
date, an invoice number, a report title. Results accumulate in
`sess$items` alongside `sess$tables`.

``` r
# Extract from all-pages text (no page rendering needed)
sess |> select_item("pub_date",
  prompt = "Publication or report date in the header or footer."
)

# Cast the result directly
sess |> select_item("pub_date",
  prompt = "Publication date.",
  cast   = "date:%d/%m/%Y"
)

# Extract from a specific page image for better accuracy
sess |> select_item("issue_number",
  prompt = "Issue or reference number shown on the cover page.",
  page   = 1L
)

# View all extracted items
sess |> show_items()
```

Supported `cast` values: `"character"` (default), `"numeric"`,
`"integer"`, `"date:<strptime_format>"`.

### Revising an item extraction

``` r
sess |> update_item("pub_date",
  prompt = "Date shown at the top of page 1 in DD/MM/YYYY format.",
  cast   = "date:%d/%m/%Y"
)
```

------------------------------------------------------------------------

## GLiNER2 — local metadata extraction

GLiNER2 is a local NLP model (~205 MB) that extracts named entities from
text without any API key. It runs fully offline after a one-time model
download.

### Setup

``` r
setup_gliner()   # downloads the model into a managed virtualenv (~500 MB)
# Restart R before first use
```

### Single fields

``` r
sess |> select_item("invoice_number",
  prompt  = "Invoice ID or reference number",
  backend = "gliner"
)

sess |> select_item("total_amount",
  prompt  = "Grand total amount payable",
  page    = 1L,
  cast    = "numeric",
  backend = "gliner"
)

# Retrieve all occurrences (e.g. all dates mentioned)
sess |> select_item("dates_mentioned",
  prompt      = "Date or dates mentioned in the document",
  backend     = "gliner",
  all_matches = TRUE
)
```

### Batch extraction (faster for many fields)

GLiNER runs one inference pass for all fields — much faster than calling
[`select_item()`](https://cathalbyrnegit.github.io/macrox/reference/select_item.md)
in a loop:

``` r
sess |> select_items_batch(
  items = c(
    invoice_number = "Invoice ID or reference number",
    vendor_name    = "Supplier or vendor name",
    total_amount   = "Grand total amount payable",
    due_date       = "Payment due date"
  ),
  cast = c(
    total_amount = "numeric",
    due_date     = "date:%d/%m/%Y"
  )
)
```

### Structured records with select_struct()

[`select_struct()`](https://cathalbyrnegit.github.io/macrox/reference/select_struct.md)
uses GLiNER2’s `extract_json` to pull multiple records with a defined
schema — useful for line-item tables that GLiNER handles better than
tabulapdf:

``` r
sess |> select_struct("line_items",
  fields = c(
    description = "Item description or product name",
    qty         = "Quantity ordered",
    unit_price  = "Unit price per item",
    total       = "Line total amount"
  ),
  list_fields  = character(0),             # none in this case
  page         = 2L,
  gliner_model = "fastino/gliner2-base-v1" # or "-large-v1" for better accuracy
)

# Convert raw records to a data frame (includes per-field confidence scores)
sess |> struct_to_df("line_items",
  min_confidence = 0.6   # drop low-confidence records; NULL keeps all
)

# Now available as sess$tables[["line_items"]] for downstream steps
```

The resulting table has value columns plus `<field>_conf` columns for
per-field GLiNER confidence scores. Use `min_confidence` to filter out
uncertain extractions.

------------------------------------------------------------------------

## Multi-page tables with LLM

When a table spans several pages and has an irregular or multi-level
header, combine
[`stack_pages()`](https://cathalbyrnegit.github.io/macrox/reference/stack_pages.md)
with `method = "llm"`:

``` r
sess |> stack_pages("breed_all",
  pages  = 10:14,
  method = "llm",
  schema = c(
    Breed  = "character",
    Month  = "character",
    Male   = "integer",
    Female = "integer"
  )
)
```

[`stack_pages()`](https://cathalbyrnegit.github.io/macrox/reference/stack_pages.md)
extracts from each page independently and row-binds the results. The
`header_match = TRUE` default discards repeated header rows on pages 2+.

------------------------------------------------------------------------

## Exporting items and tables as JSON

[`export_json()`](https://cathalbyrnegit.github.io/macrox/reference/export_json.md)
combines items and tables into a single structured payload suitable for
APIs, databases, or RAG pipelines:

``` r
sess |> export_json("output.json")

# Include the recorded steps for auditability
sess |> export_json("output_with_steps.json", include_steps = TRUE)

# Return the JSON string without writing to disk
json <- sess |> export_json()
```
