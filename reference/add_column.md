# Add a derived column to an extracted table

Add a derived column to an extracted table

## Usage

``` r
add_column(sess, table, name, expr)
```

## Arguments

- sess:

  A `macrox_session` object.

- table:

  Character label of the target table.

- name:

  Name of the new column.

- expr:

  R expression (as a string) evaluated against the data frame, e.g.
  `"male_count + female_count"` or `"2024L"`.

## Value

`sess` invisibly (step is recorded).
