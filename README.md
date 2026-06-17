# macrox

> Record and replay PDF table extraction workflows — like an Excel macro for PDFs.

You open a PDF, pull out the tables you need, clean them up — and the package silently records every step. Save the recording as a YAML macro. Next time the same report lands (new month, new year, same layout), one line replays everything and hands you clean data frames.

Works with any domain. Government statistics, financial reports, health data, planning documents. No domain logic is baked in — column names, types, and filters are all user-defined.

---

## Installation

```r
remotes::install_github("cathalbyrnegit/macrox")

# Recommended
install.packages(c("magick", "ellmer", "validate", "shinyAce", "shinyFiles"))
```

**Optional Python backends** (Docling for ML-based table detection, GLiNER2 for local NLP field extraction):

```r
# After installing macrox:
setup_docling()   # installs docling into a Python virtualenv (~2-3 GB models)
setup_gliner()    # installs GLiNER2 (~500 MB model)
```

---

## Usage modes

**Standalone app** — launch with `mx_app()`. Upload a PDF, draw boxes to extract tables, validate, export. No coding required.

**Script API** — build the extraction interactively, save as a YAML macro, replay automatically on future files.

**Shiny module** — embed `macrox_ui()` / `macrox_server()` in any existing Shiny app.

---

## Standalone app

```r
library(macrox)

mx_app()           # system browser (default)
mx_app("dialog")   # floating RStudio dialog
mx_app("pane")     # RStudio Viewer pane
```

| Tab | Purpose |
|---|---|
| **Extract** | Full PDF viewer with draw-to-select, bbox / lattice / stream / docling / llm method picker, live table preview. `←`/`→` keys navigate pages. |
| **Tables** | One tab per extracted table. Rename columns, Cast types, Filter rows (GUI), Validate (rule editor), Column stats. |
| **Items** | Extract single metadata fields (invoice numbers, dates, totals) via LLM or GLiNER2. |
| **Struct** | Extract structured records (multiple fields, multiple rows) via GLiNER2. |
| **Steps** | Recorded step badges; remove individual steps, clear all. Edit the YAML macro directly with syntax highlighting. |
| **Replay** | Choose a saved macro + a new PDF, replay, results load straight into Tables. |
| **Batch** | Select multiple PDFs, one macro, run all. Download results as Excel or zip of CSVs. |
| **Export** | CSV, Excel, JSON, zip of CSVs. Edit and download the YAML macro. |
| **Help** | Workflow checklist and method reference. |

---

## Script API workflow

### 1 — Open a session

```r
library(macrox)

sess <- mx_session("AIM_Stats_Report_2024.pdf")

# Or call with no argument for a file chooser dialog
sess <- mx_session()
# ✔ Session opened: AIM_Stats_Report_2024.pdf
```

---

### 2 — Scan for tables (optional)

```r
sess |> detect_tables(pages = 8:20)
# Page 10, table 1: 28 x 17  | County, Number.of.Breeding.Herds, ...
# ℹ 18 tables shown, 24 skipped (14 empty/header-only, 10 likely chart/figure).
```

`detect_tables()` filters noise automatically:

- **`min_rows = 1`** — drops header-only extractions from section title pages.
- **`max_header_chars = 40`** — drops chart axis labels masquerading as tables.

```r
sess |> detect_tables(min_rows = 0, max_header_chars = Inf)  # see everything
```

---

### 3 — Choose an extraction method

Five engines, mixed freely within a session and macro.

#### `bbox` — word-position engine (recommended for messy PDFs)

Uses `pdftools::pdf_data()` word bounding boxes. No Java. Best for PDFs with charts on the same page, missing grid lines, or numbers running together. Always pair with `area =`.

```r
area <- sess |> locate_area(page = 16)
sess |> select_table("breed_by_sire", page = 16, area = area, method = "bbox")
```

Tune if columns merge or split:

```r
sess |> select_table("monthly", page = 27, method = "bbox",
                      area = c(top=80, left=30, bottom=500, right=750),
                      col_gap = 20)
```

#### `lattice` / `stream` — tabulapdf engines

For clean PDFs with visible grid lines (`lattice`) or whitespace-aligned columns (`stream`). Requires Java.

```r
sess |> select_table("calf_monthly", page = 8, method = "lattice")
```

#### `docling` — ML layout engine (IBM Research)

Uses Docling's deep learning layout models to detect and extract tables. Handles scanned PDFs and complex layouts. Runs locally after an initial one-time model download (~2-3 GB). No API key needed.

```r
# One-time setup
setup_docling()
# Restart R, then:

# Scan for tables using Docling's ML detection
sess |> detect_tables(pages = 1:20, method = "docling")

# Extract a specific table
sess |> select_table_docling("financials", page = 10, table_index = 1)
```

#### `llm` — LLM engine via ellmer

Sends a rendered page image to a large language model. Best for multi-level spanning headers and irregular layouts that defeat positional extraction. Requires the `ellmer` package and a provider API key set in `.Renviron`.

```r
# Pass any ellmer chat object directly
chat <- ellmer::chat_anthropic()
sess |> select_table_llm("stillborn", page = 40, area = area, chat = chat)

# Or specify provider/model inline (chat object created automatically)
sess |> select_table_llm("stillborn", page = 40, area = area,
                          provider = "anthropic")

# Predefined schema — better accuracy, consistent replay
sess |> select_table_llm("breed_sire", page = 16, area = area,
  chat     = chat,
  schema   = c(Breed = "character", Male = "integer",
               Female = "integer", Total = "integer"),
  prompt   = "Flatten the two-row header with _ separators.")

# Revise schema without re-drawing the area
sess |> update_llm_schema("breed_sire",
  schema = c(Breed = "character", Male = "integer", Total = "integer"))
```

**Using `chat =`** — you can pass any [ellmer](https://ellmer.tidyverse.org/)
chat object. This gives you full control over model parameters, system prompts,
and provider configuration:

```r
chat <- ellmer::chat_openai(model = "gpt-4o")
chat <- ellmer::chat_google_gemini(model = "gemini-2.0-flash")
chat <- ellmer::chat_ollama(model = "llava")

# Same chat object works for tables and items
sess |> select_table_llm("tbl", page = 1, chat = chat)
sess |> select_item("ref", prompt = "Reference number", chat = chat)
```

**Using `provider =`** — shorthand when defaults are fine. `provider` can be the
name of any ellmer chat constructor without the `chat_` prefix — e.g.
`"anthropic"`, `"openai"`, `"google_gemini"`, `"openrouter"`, `"groq"`,
`"mistral"`, `"deepseek"`, `"ollama"`, or `"openai_compatible"`. `model`
defaults to a sensible choice for `"anthropic"`, `"openai"`, and
`"google_gemini"`; other providers require `model` to be set explicitly.

```r
sess |> select_table_llm("my_table", page = 5,
  provider = "openai_compatible",
  model    = "llama3.2-vision",
  base_url = "http://localhost:1234/v1")

sess |> select_table_llm("my_table", page = 5,
  provider = "openrouter",
  model    = "anthropic/claude-opus-4.5")
```

**API keys** — set in `.Renviron`, using whatever environment variable the
chosen `ellmer::chat_<provider>()` expects:

```
ANTHROPIC_API_KEY=sk-ant-...
OPENAI_API_KEY=sk-...
GEMINI_API_KEY=AI...
OPENROUTER_API_KEY=sk-or-...
```

---

### 4 — Column name behaviour

All engines preserve the original casing from the PDF. Non-alphanumeric characters are converted to `.` by `make.names()`, duplicates get `.1`, `.2` suffixes. The LLM engine returns names exactly as specified in the schema.

`rename_columns()` is always the next step — raw names from extraction are intermediate.

---

### 5 — Select tables

#### By page + position (tabulapdf only)
```r
sess |> select_table("sire_beef",  page = 15, table_index = 1)
sess |> select_table("sire_dairy", page = 15, table_index = 2)
```

#### By bounding box
```r
area <- sess |> locate_area(page = 13)
sess |> select_table("breed_by_sire", page = 13, area = area, method = "bbox")
```

#### By fuzzy caption match
```r
sess |> select_table("mart_breed", label_match = "Mart Movements by Breed")
```

#### Multi-row headers
```r
# bbox / tabulapdf — flatten via header_rows
sess |> select_table("stillborn", page = 40, area = area,
                      method = "bbox", header_rows = 2)

# LLM — prompt the model
sess |> select_table_llm("stillborn", page = 40, area = area,
  prompt = "Two-row header — flatten with _ separator.")
```

---

### 5b — Multi-page tables

When the same table spans several consecutive pages:

```r
sess |> stack_pages("breed_all", pages = 10:14, area = area, method = "bbox")
```

`stack_pages()` extracts the table from each page and row-binds the results. Repeated headers are consumed automatically (`header_match = TRUE` by default).

---

### 5c — Extract metadata items

`select_item()` extracts single fields — invoice numbers, dates, totals — anything that isn't a table row. Results are stored in `sess$items` and included in `export_json()`.

```r
# LLM backend (default)
sess |> select_item("invoice_number",
  prompt = "Extract the invoice ID or reference number.")

sess |> select_item("total_amount",
  prompt = "The grand total payable.",
  cast   = "numeric")

# GLiNER2 backend — local, no API key needed
sess |> select_item("invoice_number",
  prompt  = "Invoice ID or reference number",
  backend = "gliner")

# Batch extraction — multiple fields in one GLiNER pass
sess |> select_items_batch(c(
  invoice_no = "Invoice ID or reference number",
  total      = "Grand total amount payable",
  due_date   = "Payment due date"
), cast = c(total = "numeric", due_date = "date:%d/%m/%Y"))

sess |> show_items()
```

---

### 5d — Structured record extraction (GLiNER2)

`select_struct()` extracts structured records with multiple fields and multiple rows — like a table, but from unstructured text using GLiNER2's NLP.

```r
sess |> select_struct("line_items",
  entity = "LineItem",
  fields = c(description = "Product or service description",
             quantity    = "Number of units",
             unit_price  = "Price per unit"),
  page = 1L)

# Convert raw extraction to a data frame (with optional confidence filtering)
sess |> struct_to_df("line_items", min_confidence = 0.5)
```

---

### 6 — Clean each table

```r
sess |> rename_columns("calf_monthly", c(
  Month = "month", Male = "male_count",
  Female = "female_count", Total = "total_count"
))

sess |> cast_types("calf_monthly", c(
  male_count = "integer", female_count = "integer", total_count = "integer"
))

sess |> filter_rows("calf_monthly", exclude_where = "month == 'Total'")

# Forward-fill blank cells (common in merged-cell government PDFs)
sess |> fill_down("calf_monthly", cols = "region")

# Strip currency, commas, parenthetical negatives; auto-convert to numeric
sess |> clean_numbers("calf_monthly", cols = c("male_count", "female_count"))

# Auto-detect column types as a starting point
sess |> suggest_schema("calf_monthly")
```

---

### 7 — Derive and combine tables

```r
# Add a derived column
sess |> add_column("calf_monthly", "year", expr = "2024L")
sess |> add_column("calf_monthly", "total_check", expr = "male_count + female_count")

# Row-bind tables with the same schema
sess |> stack_tables("all_years", tables = c("calves_2022", "calves_2023", "calves_2024"))

# Join two tables
sess |> merge_tables("combined", left = "calf_monthly", right = "calf_by_county",
                      by = "month")
```

---

### 8 — Validate tables

Uses the `validate` package DSL. Rules are named R expressions — both row-level predicates and table-level aggregates are supported. Rules are stored in the macro and re-run automatically on every replay.

```r
sess |> validate_table("calf_monthly", rules = c(
  twelve_rows    = "nrow(.) == 12",
  no_na_month    = "!anyNA(month)",
  positive_males = "all(male_count > 0, na.rm = TRUE)",
  totals_match   = "all(abs(male_count + female_count - total_count) < 2, na.rm = TRUE)"
))
# ✔ All 4 validation rules passed for 'calf_monthly'

# Review all validation results
sess |> show_validations()

# Strict mode — abort replay on failure
sess |> validate_table("calf_monthly", rules = c(...), strict = TRUE)
```

---

### 9 — Audit the step list

```r
sess |> show_steps()
# * [1]  select_table     → calf_monthly  | page 8, table 1 [lattice]
# * [2]  rename_columns   → calf_monthly  | 4 renames
# * [3]  cast_types       → calf_monthly  | 4 casts
# * [4]  filter_rows      → calf_monthly  | exclude: month == 'Total'
# * [5]  add_column       → calf_monthly  | year = 2024L
# * [6]  validate_table   → calf_monthly  | 4 rules
# * [7]  select_table_llm → breed_sire    | page 16, area=[...] [anthropic]
# ...

sess |> remove_step(3)
```

---

### 10 — Final review

```r
sess |> preview_all()
# ✔ All tables look clean. Ready to save_macro.
```

---

### 11 — Export from script

```r
# Write all tables as separate CSVs
sess |> export_csv(dir = "output/")

# Write all tables to a single Excel workbook
sess |> export_excel("output/report_tables.xlsx")

# JSON — items (metadata) + tables (row arrays) in one payload
sess |> export_json(path = "output/report.json")
```

---

### 12 — Save the macro

```r
sess |> save_macro("dafm_aim_bovine", path = "inst/macros/")
# ✔ Macro saved: inst/macros/dafm_aim_bovine.yml
```

---

### 13 — Replay on a new file

```r
tables <- mx_replay("AIM_Stats_Report_2025.pdf", macro = "dafm_aim_bovine")

# Batch across multiple files
files   <- list.files("reports/", pattern = "\\.pdf$", full.names = TRUE)
results <- mx_replay_batch(files, macro = "dafm_aim_bovine")
```

LLM steps re-call the provider API on replay. For deterministic replay without API calls, complete LLM extraction once, then rely on `rename_columns()` / `cast_types()` for all cleaning — those steps replay with no network calls.

---

### 14 — Diff two runs

```r
diff <- diff_replay(
  "AIM_Stats_Report_2024.pdf",
  "AIM_Stats_Report_2025.pdf",
  macro = "dafm_aim_bovine"
)
print(diff)
# macrox diff
#   Reference: AIM_Stats_Report_2024.pdf
#   New:       AIM_Stats_Report_2025.pdf
#
# ✔ calf_monthly [unchanged]
# ~ breed_by_sire [changed] (rows: 17→18, 1 col added: HE)
# ✔ calf_by_county [unchanged]
```

---

## The macro YAML

All step types serialise cleanly. The full engine and schema are stored so replay is exact.

```yaml
macro:
  name: dafm_aim_bovine
  created: '2026-05-07 14:32'
  source: AIM_Stats_Report_2024.pdf
  n_steps: 11

steps:
  - step: select_table
    label: calf_monthly
    page: 8
    method: lattice

  - step: select_table_docling
    label: financials
    page: 10
    table_index: 1

  - step: select_table_llm
    label: breed_sire
    page: 16
    area: [60, 25, 620, 790]
    provider: anthropic
    model: claude-opus-4-5-20251001
    schema:
      Breed: character
      Male: integer
      Total: integer
    prompt: "Flatten two-row header."

  - step: select_item
    label: report_date
    prompt: "The publication date of this report."
    cast: "date:%B %Y"
    backend: llm
    provider: anthropic

  - step: select_struct
    label: line_items
    entity: LineItem
    fields:
      description: "Product or service description"
      quantity: "Number of units"
    page: 1

  - step: rename_columns
    table: calf_monthly
    mapping:
      Month: month
      Male: male_count
      Total: total_count

  - step: cast_types
    table: calf_monthly
    types:
      male_count: integer
      total_count: integer

  - step: filter_rows
    table: calf_monthly
    exclude_where: "month == 'Total'"

  - step: fill_down
    table: calf_monthly
    cols: [region]

  - step: clean_numbers
    table: calf_monthly
    cols: [male_count, total_count]

  - step: validate_table
    table: calf_monthly
    strict: false
    rules:
      twelve_rows: "nrow(.) == 12"
      no_na_month: "!anyNA(month)"
      positive:    "all(male_count > 0, na.rm = TRUE)"
```

---

## Shiny module

For embedding in an existing app:

```r
# UI:
bslib::accordion_panel(
  "PDF Import",
  icon = shiny::icon("file-pdf"),
  macrox::macrox_ui(ns("pdf_import"))
)

# Server:
pdf_result <- macrox::macrox_server("pdf_import")

observe({
  pdf_tables <- pdf_result$tables()
  req(length(pdf_tables) > 0)
  for (nm in names(pdf_tables)) your_upload_fn(pdf_tables[[nm]], target = nm)
})
```

Returns `list(tables = reactive(...), steps = reactive(...))`.

---

## Function reference

| Function | Recorded | Purpose |
|---|---|---|
| `mx_app(viewer)` | — | Launch standalone Shiny app |
| `mx_session(path)` | — | Open a script session; omit `path` for file chooser |
| **Detection** | | |
| `detect_tables(sess, pages, method, ...)` | No | Scan pages for tables (lattice / stream / docling) |
| `detect_tables_quietly(path, pages, ...)` | — | Silent detection for agents |
| `locate_area(sess, page)` | No | Interactive click-drag area selector |
| **Table extraction** | | |
| `select_table(sess, label, page, area, method, ...)` | **Yes** | Extract via bbox / lattice / stream |
| `select_table_llm(sess, label, page, ...)` | **Yes** | Extract via LLM |
| `select_table_docling(sess, label, page, table_index)` | **Yes** | Extract via Docling ML models |
| `stack_pages(sess, label, pages, ...)` | **Yes** | Extract + stack same table across pages |
| `update_llm_schema(sess, label, schema, ...)` | — | Revise LLM schema and re-extract |
| **Metadata extraction** | | |
| `select_item(sess, label, prompt, ...)` | **Yes** | Extract a single field (LLM or GLiNER2) |
| `select_items_batch(sess, items, ...)` | **Yes** | Extract multiple fields in one GLiNER pass |
| `select_struct(sess, label, fields, ...)` | **Yes** | Extract structured records via GLiNER2 |
| `struct_to_df(sess, label, min_confidence)` | **Yes** | Convert struct records to a data frame |
| `update_item(sess, label, prompt, ...)` | — | Revise item prompt and re-extract |
| `show_items(sess)` | No | Print all extracted items |
| **Cleaning** | | |
| `rename_columns(sess, table, mapping)` | **Yes** | Rename raw headers |
| `cast_types(sess, table, types)` | **Yes** | Parse to numeric / integer / date |
| `filter_rows(sess, table, exclude_where)` | **Yes** | Drop rows by expression |
| `fill_down(sess, table, cols)` | **Yes** | Forward-fill blank / NA cells |
| `clean_numbers(sess, table, cols, ...)` | **Yes** | Strip currency, commas, parse negatives |
| `add_column(sess, table, name, expr)` | **Yes** | Add a derived column |
| `suggest_schema(sess, label)` | No | Auto-detect column types |
| **Composition** | | |
| `stack_tables(sess, label, tables, .fill)` | **Yes** | Row-bind tables into one |
| `merge_tables(sess, label, left, right, by, ...)` | **Yes** | Join two tables |
| **Validation** | | |
| `validate_table(sess, table, rules, strict)` | **Yes** | Run data quality rules |
| `show_validations(sess)` | No | Print all confrontation results |
| **Inspection** | | |
| `view_in_pdf(sess, step, dpi)` | No | View a step's PDF location |
| `preview(sess, table, n)` | No | Inspect one table |
| `preview_all(sess, n)` | No | Inspect all tables with totals |
| `show_steps(sess)` | No | Numbered step list with flags |
| `remove_step(sess, index)` | — | Drop a step by index |
| **Macro I/O** | | |
| `save_macro(sess, name, path)` | — | Write YAML macro |
| `load_macro(name, path)` | — | Read YAML macro |
| `mx_replay(file, macro)` | — | Replay macro, returns named list of data frames |
| `mx_replay_batch(files, macro)` | — | Replay across multiple files |
| **Comparison** | | |
| `diff_replay(file1, file2, macro)` | — | Compare macro outputs across two files |
| `detail(diff, table)` | — | Cell-level change details |
| **Export** | | |
| `export_csv(sess, dir, tables)` | — | Write tables to CSV files |
| `export_excel(sess, path, tables)` | — | Write tables to an Excel workbook |
| `export_json(sess, path, ...)` | — | JSON payload (items + tables) |
| **Agent tools** | | |
| `mx_profile(path, pages, method)` | — | Machine-readable PDF profile for agents |
| `validate_macro(file, macro)` | — | Pre-flight macro validation |
| `test_extraction(file, page, ...)` | — | Single extraction with eval report |
| **Python backends** | | |
| `setup_docling(envname)` | — | Install Docling Python package |
| `close_docling()` | — | Release Docling document cache |
| `setup_gliner(model, envname)` | — | Install GLiNER2 Python package |
| `close_gliner()` | — | Unload GLiNER2 model from memory |
| **Shiny** | | |
| `macrox_ui(id, title, height)` | — | Shiny module UI |
| `macrox_server(id)` | — | Shiny module server |

---

## Choosing an extraction method

| Situation | Recommended |
|---|---|
| Clean PDF with visible grid lines | `lattice` |
| Whitespace-aligned columns, no grid | `stream` |
| Charts and tables on the same page | `bbox` + `area =` |
| Numbers running together | `bbox` + `col_gap =` |
| Scanned PDF, no selectable text | `docling` or `llm` |
| Complex layout, offline | `docling` |
| Multi-level / spanning headers | `llm` with `schema =` |
| Highly irregular layout | `llm` |
| No API key / offline | `bbox`, tabulapdf, or `docling` |
| Single metadata fields (invoice #, date) | `select_item()` with `llm` or `gliner` |
| Structured records from text | `select_struct()` with GLiNER2 |

---

## Dependencies

| Package | Role | Required |
|---|---|---|
| `pdftools` | Word positions (`bbox`), page text, rendering | Yes |
| `stringdist` | Fuzzy caption matching | Yes |
| `yaml` | Macro read/write | Yes |
| `cli` | Console output | Yes |
| `rlang` | `%||%` operator | Yes |
| `tabulapdf` | `lattice` / `stream` extraction (needs Java) | Suggested |
| `reticulate` | Python bridge for Docling and GLiNER2 backends | Suggested |
| `magick` | Bounding box highlights; LLM image cropping | Suggested |
| `ellmer` | LLM extraction | Suggested |
| `validate` | Data quality rules (`validate_table`) | Suggested |
| `jsonlite` | JSON export | Suggested |
| `shiny` + `bslib` + `DT` | Standalone app and Shiny module | Suggested |
| `shinyFiles` | Native filesystem browser in app | Suggested |
| `shinyAce` | YAML editor with syntax highlighting | Suggested |
| `writexl` | Excel export | Suggested |
| `zip` | Zip-of-CSVs export | Suggested |
| `rstudioapi` | Opens Viewer pane in RStudio | Suggested |

---

## Design decisions

**Five extraction engines** — `bbox`, tabulapdf (lattice/stream), Docling, and LLM. Mixed per-table within the same session and macro. `bbox` is the practical default; Docling handles scanned PDFs offline; LLM handles what positional methods cannot.

**Tables + items + structs** — tables (`select_table`), single fields (`select_item`), and structured records (`select_struct`) are all first-class citizens. Items and structs are recorded steps and replay automatically.

**Validation as a step** — `validate_table()` is recorded and replays with the macro. The same rules run every time, surfacing data drift automatically without extra code.

**Derive and combine as steps** — `add_column()`, `fill_down()`, `clean_numbers()`, `stack_tables()`, and `merge_tables()` are recorded steps, so a complete pipeline from raw PDF to analysis-ready data frame is captured in a single macro.

**Diff without code** — `diff_replay()` gives an immediate structural comparison between two report editions. The first sign of a format change is a `~` in the diff output, not a mysterious downstream error.

**S3 not R6** — session is a plain R environment; mutation in place; pipe API.

**No commit gate** — `preview_all()` is the checkpoint. Save when ready.

**Domain agnostic** — no report-specific logic in package code.

**Shiny in Suggests** — script API and `validate_table()` work with no Shiny installed.

**Python optional** — Docling and GLiNER2 require `reticulate` and their respective Python packages, but all other functionality works without Python.
