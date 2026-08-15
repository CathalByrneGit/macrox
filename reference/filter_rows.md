# Remove rows matching an expression

Remove rows matching an expression

## Usage

``` r
filter_rows(sess, table, exclude_where)
```

## Arguments

- sess:

  A `macrox_session` object.

- table:

  Character label of the target table.

- exclude_where:

  Character string expression evaluated against the data frame columns.
  Rows where the expression is `TRUE` are removed.

## Value

`sess` invisibly (step is recorded).
