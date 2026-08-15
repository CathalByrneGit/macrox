# Join two extracted tables

Wraps [`base::merge()`](https://rdrr.io/r/base/merge.html) and records
the step so it replays automatically.

## Usage

``` r
merge_tables(
  sess,
  label,
  left,
  right,
  by,
  all = FALSE,
  all.x = FALSE,
  all.y = FALSE
)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Label for the merged output table.

- left:

  Label of the left table.

- right:

  Label of the right table.

- by:

  Character vector of column names to join on.

- all:

  Logical; `FALSE` (default) = inner join, `TRUE` = full outer.

- all.x:

  Left join when `TRUE`.

- all.y:

  Right join when `TRUE`.

## Value

`sess` invisibly (step is recorded).
