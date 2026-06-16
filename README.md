# pdfmacro

> Record and replay PDF table extraction workflows — like an Excel macro for PDFs.

You open a PDF, pull out the tables you need, clean them up — and the package silently records every step. Save the recording as a YAML macro. Next time the same report lands (new month, new year, same layout), one line replays everything and hands you clean data frames.

Works with any domain. Government statistics, financial reports, health data, planning documents. No domain logic is baked in — column names, types, and filters are all user-defined.

---

## Installation

```r
# Java required for the lattice/stream extraction engines
remotes::install_github("your/pdfmacro")

# Recommended
install.packages(c("magick", "ellmer", "validate", "shinyAce", "shinyFiles"))
```

---

## Usage modes

**Standalone app** — launch with `pdf_app()`. Upload a PDF, draw boxes to extract tables, validate, export. No coding required.

**Script API** — build the extraction interactively, save as a YAML macro, replay automatically on future files.

**Shiny module** — embed `pdfmacro_ui()` / `pdfmacro_server()` in any existing Shiny app.

---

## Standalone app

```r
library(pdfmacro)

pdf_app()           # system browser (default)
pdf_app("dialog")   # floating RStudio dialog
pdf_app("pane")     # RStudio Viewer pane
```

| Tab | Purpose |
|---|---|
| **Extract** | Full PDF viewer with draw-to-select, bbox / lattice / stream / llm method picker, live table preview. `←`/`→` keys navigate pages. |
| **Tables** | One tab per extracted table. Rename columns, Cast types, Filter rows (GUI), Validate (rule editor), Column stats. |
| **Steps** | Recorded step badges; remove individual steps, clear all. Edit the YAML macro directly with syntax highlighting. |
| **Replay** | Choose a saved macro + a new PDF, replay, results load straight into Tables. |
| **Batch** | Select multiple PDFs, one macro, run all. Download results as Excel or zip of CSVs. |
| **Export** | CSV, single-table Excel, all-tables workbook, zip of CSVs. Edit and download the YAML macro. |
| **Help** | Workflow checklist and method reference. |

---

## Script API workflow

### 1 — Open a session

```r
library(pdfmacro)

sess <- pdf_session("AIM_Stats_Report_2024.pdf")

# Or call with no argument for a file chooser dialog
sess <- pdf_session()
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

Three engines, mixed freely within a session and macro.

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

#### `llm` — LLM engine via ellmer

Sends a rendered page image to a large language model. Best for multi-level spanning headers and irregular layouts that defeat positional extraction. Requires the `ellmer` package and a provider API key set in `.Renviron`.

```r
# Auto-detect columns
sess |> select_table_llm("stillborn", page = 40, area = area,
                          provider = "anthropic")

# Predefined schema — better accuracy, consistent replay
sess |> select_table_llm("breed_sire", page = 16, area = area,
  provider = "anthropic",
  schema   = c(Breed = "character", Male = "integer",
               Female = "integer", Total = "integer"),
  prompt   = "Flatten the two-row header with _ separators.")

# Revise schema without re-drawing the area
sess |> update_llm_schema("breed_sire",
  schema = c(Breed = "character", Male = "integer", Total = "integer"))
```

`provider` can be the name of any [ellmer](https://ellmer.tidyverse.org/) chat
constructor, without the `chat_` prefix — e.g. `"anthropic"`, `"openai"`,
`"google_gemini"`, `"openrouter"`, `"groq"`, `"mistral"`, `"deepseek"`,
`"ollama"`, or `"openai_compatible"`. `model` defaults to a sensible choice
for `"anthropic"`, `"openai"`, and `"google_gemini"`; other providers require
`model` to be set explicitly.

For `openai_compatible` (any OpenAI-API-compatible endpoint — LM Studio, vLLM, etc.)
or hosted gateways like `openrouter`:

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
tables <- pdf_replay("AIM_Stats_Report_2025.pdf", macro = "dafm_aim_bovine")

# Batch across multiple files
files   <- list.files("reports/", pattern = "\\.pdf$", full.names = TRUE)
results <- pdf_replay_batch(files, macro = "dafm_aim_bovine")
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
# pdfmacro diff
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
  n_steps: 9

steps:
  - step: select_table
    label: calf_monthly
    page: 8
    method: lattice

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

  - step: add_column
    table: calf_monthly
    name: year
    expr: "2024L"

  - step: validate_table
    table: calf_monthly
    strict: false
    rules:
      twelve_rows: "nrow(.) == 12"
      no_na_month: "!anyNA(month)"
      positive:    "all(male_count > 0, na.rm = TRUE)"

  - step: stack_tables
    label: all_years
    tables: [calves_2022, calves_2023, calves_2024]

  - step: merge_tables
    label: combined
    left: calf_monthly
    right: calf_by_county
    by: [month]
    all: false
```

---

## Shiny module

For embedding in an existing app:

```r
# UI:
bslib::accordion_panel(
  "PDF Import",
  icon = shiny::icon("file-pdf"),
  pdfmacro::pdfmacro_ui(ns("pdf_import"))
)

# Server:
pdf_result <- pdfmacro::pdfmacro_server("pdf_import")

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
| `pdf_app(viewer)` | — | Launch standalone Shiny app |
| `pdf_session(path)` | — | Open a script session; omit `path` for file chooser |
| `detect_tables(sess, pages, method, min_rows, max_header_chars)` | No | Scan pages, filter noise |
| `locate_area(sess, page)` | No | Interactive click-drag area selector |
| `select_table(sess, label, page, area, method, header_rows, row_tol, col_gap, ...)` | **Yes** | Extract via bbox / lattice / stream |
| `select_table_llm(sess, label, page, area, provider, model, base_url, schema, prompt, ...)` | **Yes** | Extract via LLM |
| `update_llm_schema(sess, label, schema, prompt, re_extract)` | — | Revise LLM schema and re-extract |
| `rename_columns(sess, table, mapping)` | **Yes** | Rename raw headers |
| `cast_types(sess, table, types)` | **Yes** | Parse to numeric / integer / date |
| `filter_rows(sess, table, exclude_where)` | **Yes** | Drop rows by expression |
| `add_column(sess, table, name, expr)` | **Yes** | Add a derived column |
| `stack_tables(sess, label, tables, .fill)` | **Yes** | Row-bind tables into one |
| `merge_tables(sess, label, left, right, by, ...)` | **Yes** | Join two tables |
| `validate_table(sess, table, rules, strict)` | **Yes** | Run data quality rules |
| `show_validations(sess)` | No | Print all confrontation results |
| `view_in_pdf(sess, step, dpi)` | No | View a step's PDF location |
| `preview(sess, table, n)` | No | Inspect one table |
| `preview_all(sess, n)` | No | Inspect all tables with totals |
| `show_steps(sess)` | No | Numbered step list with flags |
| `remove_step(sess, index)` | — | Drop a step by index |
| `save_macro(sess, name, path)` | — | Write YAML macro |
| `load_macro(name, path)` | — | Read YAML macro |
| `pdf_replay(file, macro)` | — | Replay macro, returns named list of data frames |
| `pdf_replay_batch(files, macro)` | — | Replay across multiple files |
| `diff_replay(file1, file2, macro)` | — | Compare macro outputs across two files |
| `export_csv(sess, dir, tables)` | — | Write tables to CSV files |
| `export_excel(sess, path, tables)` | — | Write tables to an Excel workbook |
| `pdf_profile(path, pages, method)` | — | Machine-readable PDF profile for agents |
| `validate_macro(file, macro)` | — | Pre-flight macro validation |
| `detect_tables_quietly(path, pages, ...)` | — | Silent detection for agents |
| `pdfmacro_ui(id, title, height)` | — | Shiny module UI |
| `pdfmacro_server(id)` | — | Shiny module server |

---

## Choosing an extraction method

| Situation | Recommended |
|---|---|
| Clean PDF with visible grid lines | `lattice` |
| Whitespace-aligned table, no grid | `stream` |
| Charts and tables on the same page | `bbox` + `area =` |
| Numbers running together | `bbox` + `area =` |
| Multi-level / spanning headers | `llm` with `schema =` |
| Highly irregular layout | `llm` |
| No API key / offline | `bbox` or tabulapdf |

---

## Dependencies

| Package | Role | Required |
|---|---|---|
| `pdftools` | Word positions (`bbox`), page text, rendering | Yes |
| `tabulapdf` | `lattice` / `stream` extraction (needs Java) | Yes |
| `stringdist` | Fuzzy caption matching | Yes |
| `yaml` | Macro read/write | Yes |
| `cli` | Console output | Yes |
| `rlang` | `%||%` operator | Yes |
| `magick` | Bounding box highlights; LLM image cropping | Suggested |
| `ellmer` | LLM extraction | Suggested |
| `validate` | Data quality rules (`validate_table`) | Suggested |
| `shiny` + `bslib` + `DT` | Standalone app and Shiny module | Suggested |
| `shinyFiles` | Native filesystem browser in app | Suggested |
| `shinyAce` | YAML editor with syntax highlighting | Suggested |
| `writexl` | Excel export | Suggested |
| `zip` | Zip-of-CSVs export | Suggested |
| `rstudioapi` | Opens Viewer pane in RStudio | Suggested |

---

## Design decisions

**Three extraction engines** — `bbox`, tabulapdf, and `llm`. Mixed per-table within the same session. `bbox` is the practical default; `llm` handles what bbox cannot.

**Validation as a step** — `validate_table()` is recorded and replays with the macro. The same rules run every time, surfacing data drift automatically without extra code.

**Derive and combine as steps** — `add_column()`, `stack_tables()`, and `merge_tables()` are recorded steps, so a complete pipeline from raw PDF to analysis-ready data frame is captured in a single macro.

**Diff without code** — `diff_replay()` gives an immediate structural comparison between two report editions. The first sign of a format change is a `~` in the diff output, not a mysterious downstream error.

**S3 not R6** — session is a plain R environment; mutation in place; pipe API.

**No commit gate** — `preview_all()` is the checkpoint. Save when ready.

**Domain agnostic** — no report-specific logic in package code.

**Shiny in Suggests** — script API and `validate_table()` work with no Shiny installed.
