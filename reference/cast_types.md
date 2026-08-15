# Cast column types in an extracted table

Cast column types in an extracted table

## Usage

``` r
cast_types(sess, table, types)
```

## Arguments

- sess:

  A `macrox_session` object.

- table:

  Character label of the target table.

- types:

  Named character vector: `c(col = "integer")`. Supported types:
  `"numeric"`, `"integer"`, `"character"`, `"date:<fmt>"` e.g.
  `"date:%d/%m/%Y"`.

## Value

`sess` invisibly (step is recorded).
