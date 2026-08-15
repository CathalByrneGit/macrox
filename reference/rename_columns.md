# Rename columns in an extracted table

Rename columns in an extracted table

## Usage

``` r
rename_columns(sess, table, mapping)
```

## Arguments

- sess:

  A `macrox_session` object.

- table:

  Character label of the target table.

- mapping:

  Named character vector: `c(OldName = "new_name", ...)`.

## Value

`sess` invisibly (step is recorded).
