# Forward-fill blank or NA cells in specified columns

Fills each blank or `NA` cell downward from the last non-blank value.
Useful when merged header cells in government PDFs leave empty values in
rows below the merged cell boundary.

## Usage

``` r
fill_down(sess, table, cols = NULL)
```

## Arguments

- sess:

  A `macrox_session` object.

- table:

  Character label of the target table.

- cols:

  Character vector of column names to fill. `NULL` (default) fills all
  columns.

## Value

`sess` invisibly (step is recorded).
