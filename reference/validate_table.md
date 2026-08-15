# Validate an extracted table against a set of rules

Uses the `validate` package DSL. Rules are named character strings
containing R expressions evaluated against the data frame. Both
row-level predicates (`total_count > 0`) and table-level aggregates
(`nrow(.) == 12`) are supported.

## Usage

``` r
validate_table(sess, table, rules, strict = FALSE)
```

## Arguments

- sess:

  A `macrox_session` object.

- table:

  Character label of the table to validate.

- rules:

  Named character vector or named list of R expression strings. Names
  become the rule identifiers shown in reports. Alternatively, a
  [`validate::validator()`](https://rdrr.io/pkg/validate/man/validator.html)
  object.

- strict:

  If `TRUE`, any rule failure calls `cli_abort()` instead of
  `cli_warn()`. Default `FALSE`.

## Value

`sess` invisibly (step is recorded).

## Details

Results are stored in `sess$validations[[table]]` for later inspection.
On replay, failures produce warnings by default; set `strict = TRUE` to
abort.

## Examples

``` r
if (FALSE) { # \dontrun{
sess |> validate_table("calf_monthly", rules = c(
  no_na_month    = "!anyNA(month)",
  positive_males = "all(male_count > 0, na.rm = TRUE)",
  twelve_rows    = "nrow(.) == 12"
))
} # }
```
