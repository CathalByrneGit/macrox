# Clean and parse numeric strings in table columns

Strips thousands separators, currency symbols, and parenthetical
negatives; maps sentinel strings to `NA`; optionally coerces columns to
numeric.

## Usage

``` r
clean_numbers(
  sess,
  table,
  cols = NULL,
  currency = c("£", "$", "€", "¥"),
  na_strings = c("-", "—", "n/a", "na", ""),
  negative_parens = TRUE,
  convert = TRUE
)
```

## Arguments

- sess:

  A `macrox_session` object.

- table:

  Character label of the target table.

- cols:

  Character vector of column names to process. `NULL` (default) targets
  all non-numeric columns.

- currency:

  Character vector of currency symbols to strip.

- na_strings:

  Strings that should become `NA` (case-insensitive after whitespace
  trimming).

- negative_parens:

  If `TRUE` (default), converts `(123)` to `-123`.

- convert:

  If `TRUE` (default), coerces a column to `numeric` when all non-NA
  values parse successfully.

## Value

`sess` invisibly (step is recorded).
