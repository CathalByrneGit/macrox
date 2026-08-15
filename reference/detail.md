# Inspect cell-level changes for one table in a diff

Returns a data frame of every cell that changed between the two replays,
with columns `row`, `col`, `ref`, `new`, and `delta` (numeric
difference, or `NA` for non-numeric columns).

## Usage

``` r
detail(x, ...)
```

## Arguments

- x:

  A `macrox_diff` object returned by
  [`diff_replay()`](https://cathalbyrnegit.github.io/macrox/reference/diff_replay.md).

- ...:

  Unused.

- table:

  Character name of the table to inspect.

## Value

A data frame with one row per changed cell.
