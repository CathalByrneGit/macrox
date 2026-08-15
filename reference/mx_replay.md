# Replay a macro against a new PDF file

Replay a macro against a new PDF file

## Usage

``` r
mx_replay(file, macro, macro_path = ".", params = list())
```

## Arguments

- file:

  Path to the PDF file.

- macro:

  Either a macro name / path (character) or a step list returned by
  [`load_macro()`](https://cathalbyrnegit.github.io/macrox/reference/load_macro.md).

- macro_path:

  Directory to look for the macro file (default `.`).

- params:

  Named list of parameter values for parameterised macros, e.g.
  `list(year = 2025L, region = "Cork")`. Each `$name` placeholder in
  step fields is replaced with the corresponding value.

## Value

Named list of extracted data frames.
