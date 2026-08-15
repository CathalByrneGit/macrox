# Suggest column types for an extracted table

Inspects values in each column and returns a named character vector of
type specs compatible with
[`cast_types()`](https://cathalbyrnegit.github.io/macrox/reference/cast_types.md).
Useful as a starting point before calling
[`cast_types()`](https://cathalbyrnegit.github.io/macrox/reference/cast_types.md)
or providing a schema to
[`select_table_llm()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_llm.md).

## Usage

``` r
suggest_schema(sess, label)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Character label of the table to inspect.

## Value

Named character vector of type specs (invisibly).
