# Validate a macro against a PDF without extracting data

Runs a pre-flight check on every step. Agents should call this after
generating a macro and fix any errors before the human runs
[`mx_replay()`](https://cathalbyrnegit.github.io/macrox/reference/mx_replay.md).

## Usage

``` r
validate_macro(file, macro)
```

## Arguments

- file:

  Path to a PDF file.

- macro:

  Either a step list or a path to a `.yml` file.

## Value

A `macrox_validation` list with `$valid`, `$steps` (data frame), and
`$errors` (character vector).
