# macrox

> Record and replay PDF table extraction workflows — like an Excel macro
> for PDFs.

You open a PDF, pull out the tables you need, clean them up — and the
package silently records every step. Save the recording as a YAML macro.
Next time the same report lands, one line replays everything and hands
you clean data frames.

Works with any domain: government statistics, financial reports, health
data, planning documents. No domain logic is baked in.

------------------------------------------------------------------------

## Installation

``` r
remotes::install_github("cathalbyrnegit/macrox")

# Recommended
install.packages(c("magick", "ellmer", "validate", "shinyAce", "shinyFiles"))
```

**Optional Python backends** (Docling for ML table detection, GLiNER2
for local NLP):

``` r
setup_docling()   # ~2-3 GB models, one-time setup
setup_gliner()    # ~500 MB model, one-time setup
```

------------------------------------------------------------------------

## Quick start

``` r
library(macrox)

sess <- mx_session("report_2024.pdf")

sess |> detect_tables(pages = 1:20)

area <- sess |> locate_area(page = 8)
sess |> select_table("monthly", page = 8, area = area, method = "bbox")
sess |> rename_columns("monthly", c(Month = "month", Total = "total"))
sess |> cast_types("monthly", c(total = "integer"))
sess |> filter_rows("monthly", exclude_where = "month == 'Total'")

sess |> save_macro("my_report")

# Next year — one line
mx_replay("report_2025.pdf", macro = "my_report")
```

------------------------------------------------------------------------

## Usage modes

| Mode               | How                                                                                                                                                                                                   |
|--------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Standalone app** | [`mx_app()`](https://cathalbyrnegit.github.io/macrox/reference/mx_app.md) — full GUI, no code needed                                                                                                  |
| **Script API**     | Build interactively, save as YAML macro, replay automatically                                                                                                                                         |
| **Shiny module**   | [`macrox_ui()`](https://cathalbyrnegit.github.io/macrox/reference/macrox_ui.md) / [`macrox_server()`](https://cathalbyrnegit.github.io/macrox/reference/macrox_server.md) — embed in any existing app |

------------------------------------------------------------------------

## Five extraction engines

| Engine               | Best for                                                        |
|----------------------|-----------------------------------------------------------------|
| `bbox`               | Messy PDFs, charts on same page, no Java needed                 |
| `lattice` / `stream` | Clean PDFs with grid lines or aligned columns (needs Java)      |
| `docling`            | Scanned PDFs, offline, ML layout detection                      |
| `llm`                | Multi-level headers, irregular layouts, any provider via ellmer |

------------------------------------------------------------------------

## Learn more

| Vignette                                                                                       | Topics covered                                                                  |
|------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------|
| [Getting Started](https://cathalbyrnegit.github.io/macrox/articles/macrox.html)                | Full script workflow: extract → clean → validate → save → replay                |
| [Extraction Methods](https://cathalbyrnegit.github.io/macrox/articles/extraction-methods.html) | bbox tuning, lattice/stream, Docling, multi-page tables, method selection guide |
| [LLM Integration](https://cathalbyrnegit.github.io/macrox/articles/llm-integration.html)       | Provider setup, schemas, global config, items, GLiNER2                          |
| [Macros & Testing](https://cathalbyrnegit.github.io/macrox/articles/macros-and-testing.html)   | Parameterised macros, batch replay, diff, CI snapshot testing                   |

[**Full function reference
→**](https://cathalbyrnegit.github.io/macrox/reference/)
