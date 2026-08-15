# Shiny module UI for PDF table extraction

Provides a split-pane interface: PDF viewer on the left, live extracted
table on the right. Transforms (rename, type-cast, filter) open in modal
dialogs so both panes remain visible.

## Usage

``` r
macrox_ui(id, title = "PDF · Table Extractor", height = "600px")
```

## Arguments

- id:

  Module namespace ID.

- title:

  Card title shown in the header (default `"PDF · Table Extractor"`).

- height:

  Height of the PDF image pane (default `"600px"`).

## Value

A `bslib` card UI element.
