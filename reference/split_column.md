# Split one column into several by a separator

Splits each cell in `col` on `sep` (a regex) and stores the resulting
pieces in new columns named by `into`. The original column is removed
unless `keep = TRUE`. Cells with fewer pieces than `length(into)` are
padded with `NA`; cells with more are truncated.

## Usage

``` r
split_column(sess, table, col, into, sep = "\\s+", keep = FALSE)
```

## Arguments

- sess:

  A `macrox_session` object.

- table:

  Character label of the target table.

- col:

  Name of the column to split.

- into:

  Character vector of names for the new columns.

- sep:

  Regex separator (default `"\\s+"` — one or more whitespace chars).

- keep:

  If `TRUE`, keep the original column alongside the new ones.

## Value

`sess` invisibly (step is recorded).
