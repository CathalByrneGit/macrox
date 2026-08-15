# Package index

## Session

Create and manage extraction sessions.

- [`mx_session()`](https://cathalbyrnegit.github.io/macrox/reference/mx_session.md)
  : Open a macrox session
- [`mx_profile()`](https://cathalbyrnegit.github.io/macrox/reference/mx_profile.md)
  : Profile a PDF for agent consumption
- [`show_steps()`](https://cathalbyrnegit.github.io/macrox/reference/show_steps.md)
  : Show recorded steps
- [`remove_step()`](https://cathalbyrnegit.github.io/macrox/reference/remove_step.md)
  : Remove a recorded step by index

## Detection

Scan a PDF for table locations.

- [`detect_tables()`](https://cathalbyrnegit.github.io/macrox/reference/detect_tables.md)
  : Scan a PDF for tables
- [`detect_tables_quietly()`](https://cathalbyrnegit.github.io/macrox/reference/detect_tables_quietly.md)
  : Detect tables without console output
- [`locate_area()`](https://cathalbyrnegit.github.io/macrox/reference/locate_area.md)
  : Interactively select a table area

## Table extraction

Pull tables out of PDF pages.

- [`select_table()`](https://cathalbyrnegit.github.io/macrox/reference/select_table.md)
  : Extract a table from the PDF and record the step
- [`select_table_docling()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_docling.md)
  : Extract a table using Docling
- [`select_table_llm()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_llm.md)
  : Extract a table using an LLM
- [`stack_pages()`](https://cathalbyrnegit.github.io/macrox/reference/stack_pages.md)
  : Extract and stack the same table across multiple PDF pages
- [`stack_tables()`](https://cathalbyrnegit.github.io/macrox/reference/stack_tables.md)
  : Row-bind multiple extracted tables into one
- [`merge_tables()`](https://cathalbyrnegit.github.io/macrox/reference/merge_tables.md)
  : Join two extracted tables

## Metadata extraction

Extract individual fields and structured records.

- [`select_item()`](https://cathalbyrnegit.github.io/macrox/reference/select_item.md)
  : Extract a single metadata field from a PDF
- [`select_items_batch()`](https://cathalbyrnegit.github.io/macrox/reference/select_items_batch.md)
  : Extract multiple metadata fields in one GLiNER model pass
- [`select_struct()`](https://cathalbyrnegit.github.io/macrox/reference/select_struct.md)
  : Extract structured records from a PDF page using GLiNER2
- [`struct_to_df()`](https://cathalbyrnegit.github.io/macrox/reference/struct_to_df.md)
  : Convert raw struct extraction to a data frame
- [`show_items()`](https://cathalbyrnegit.github.io/macrox/reference/show_items.md)
  : Show all extracted items
- [`update_item()`](https://cathalbyrnegit.github.io/macrox/reference/update_item.md)
  : Update the prompt for a recorded select_item step and re-extract
- [`update_llm_schema()`](https://cathalbyrnegit.github.io/macrox/reference/update_llm_schema.md)
  : Update the schema for a recorded LLM extraction step and re-extract
- [`suggest_schema()`](https://cathalbyrnegit.github.io/macrox/reference/suggest_schema.md)
  : Suggest column types for an extracted table

## LLM configuration

Configure the LLM provider at session or global level.

- [`mx_configure_llm()`](https://cathalbyrnegit.github.io/macrox/reference/mx_configure_llm.md)
  : Configure a session-level default LLM

## Cleaning

Rename, cast, filter, and reshape extracted tables.

- [`rename_columns()`](https://cathalbyrnegit.github.io/macrox/reference/rename_columns.md)
  : Rename columns in an extracted table
- [`cast_types()`](https://cathalbyrnegit.github.io/macrox/reference/cast_types.md)
  : Cast column types in an extracted table
- [`filter_rows()`](https://cathalbyrnegit.github.io/macrox/reference/filter_rows.md)
  : Remove rows matching an expression
- [`add_column()`](https://cathalbyrnegit.github.io/macrox/reference/add_column.md)
  : Add a derived column to an extracted table
- [`fill_down()`](https://cathalbyrnegit.github.io/macrox/reference/fill_down.md)
  : Forward-fill blank or NA cells in specified columns
- [`clean_numbers()`](https://cathalbyrnegit.github.io/macrox/reference/clean_numbers.md)
  : Clean and parse numeric strings in table columns

## Inspection

Preview and visualise extracted data.

- [`preview()`](https://cathalbyrnegit.github.io/macrox/reference/preview.md)
  : Preview one extracted table
- [`preview_all()`](https://cathalbyrnegit.github.io/macrox/reference/preview_all.md)
  : Preview all extracted tables with numeric totals
- [`view_in_pdf()`](https://cathalbyrnegit.github.io/macrox/reference/view_in_pdf.md)
  : View a recorded step's location in the PDF

## Macros

Save, load, and replay extraction workflows.

- [`save_macro()`](https://cathalbyrnegit.github.io/macrox/reference/save_macro.md)
  : Save the session's recorded steps as a YAML macro
- [`load_macro()`](https://cathalbyrnegit.github.io/macrox/reference/load_macro.md)
  : Load a YAML macro
- [`mx_replay()`](https://cathalbyrnegit.github.io/macrox/reference/mx_replay.md)
  : Replay a macro against a new PDF file
- [`mx_replay_batch()`](https://cathalbyrnegit.github.io/macrox/reference/mx_replay_batch.md)
  : Replay a macro across multiple PDF files
- [`validate_macro()`](https://cathalbyrnegit.github.io/macrox/reference/validate_macro.md)
  : Validate a macro against a PDF without extracting data

## Comparison

Diff macro outputs across two PDFs.

- [`diff_replay()`](https://cathalbyrnegit.github.io/macrox/reference/diff_replay.md)
  : Compare macro outputs across two PDF files
- [`detail()`](https://cathalbyrnegit.github.io/macrox/reference/detail.md)
  : Inspect cell-level changes for one table in a diff

## Validation

Validate table structure and values.

- [`validate_table()`](https://cathalbyrnegit.github.io/macrox/reference/validate_table.md)
  : Validate an extracted table against a set of rules
- [`show_validations()`](https://cathalbyrnegit.github.io/macrox/reference/show_validations.md)
  : Show validation results for all tables

## Export

Write extracted data to files.

- [`export_csv()`](https://cathalbyrnegit.github.io/macrox/reference/export_csv.md)
  : Export all extracted tables to CSV files
- [`export_excel()`](https://cathalbyrnegit.github.io/macrox/reference/export_excel.md)
  : Export all extracted tables to an Excel workbook
- [`export_json()`](https://cathalbyrnegit.github.io/macrox/reference/export_json.md)
  : Export session data as a structured JSON payload

## Testing

Snapshot-test macro outputs in CI.

- [`test_macro()`](https://cathalbyrnegit.github.io/macrox/reference/test_macro.md)
  : Snapshot-test a macro against a stored reference
- [`test_extraction()`](https://cathalbyrnegit.github.io/macrox/reference/test_extraction.md)
  : Test extraction parameters and return a structured evaluation report
- [`expect_table_snapshot()`](https://cathalbyrnegit.github.io/macrox/reference/expect_table_snapshot.md)
  : Snapshot-test an extracted table

## Shiny

Embed macrox in a Shiny application.

- [`mx_app()`](https://cathalbyrnegit.github.io/macrox/reference/mx_app.md)
  : Launch the macrox standalone app
- [`macrox_ui()`](https://cathalbyrnegit.github.io/macrox/reference/macrox_ui.md)
  : Shiny module UI for PDF table extraction
- [`macrox_server()`](https://cathalbyrnegit.github.io/macrox/reference/macrox_server.md)
  : Shiny module server for PDF table extraction

## Python backends

Set up optional ML and NLP engines.

- [`setup_docling()`](https://cathalbyrnegit.github.io/macrox/reference/setup_docling.md)
  : Install Docling into a Python environment
- [`close_docling()`](https://cathalbyrnegit.github.io/macrox/reference/close_docling.md)
  : Release the Docling document cache
- [`setup_gliner()`](https://cathalbyrnegit.github.io/macrox/reference/setup_gliner.md)
  : Install GLiNER2 in the macrox Python environment
- [`close_gliner()`](https://cathalbyrnegit.github.io/macrox/reference/close_gliner.md)
  : Unload the GLiNER2 model from memory
