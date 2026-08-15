# Row-bind multiple extracted tables into one

Useful after batch replay to combine monthly or regional tables that
share the same schema.

## Usage

``` r
stack_tables(sess, label, tables, .fill = FALSE)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Label for the combined output table.

- tables:

  Character vector of existing table labels to stack.

- .fill:

  Fill missing columns with `NA` when schemas differ (default `FALSE`).

## Value

`sess` invisibly (step is recorded).
