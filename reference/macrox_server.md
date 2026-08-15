# Shiny module server for PDF table extraction

Shiny module server for PDF table extraction

## Usage

``` r
macrox_server(id)
```

## Arguments

- id:

  Module namespace ID (must match
  [`macrox_ui()`](https://cathalbyrnegit.github.io/macrox/reference/macrox_ui.md)).

## Value

A list with two reactives: `tables` (named list of data frames) and
`steps` (list of step definitions).
