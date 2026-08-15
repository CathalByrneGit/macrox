# Convert raw struct extraction to a data frame

Takes the raw GLiNER2 records stored by
[`select_struct()`](https://cathalbyrnegit.github.io/macrox/reference/select_struct.md)
(which include per-field confidence scores) and converts them to a data
frame stored in `sess$tables[[label]]`. Optionally filters out
low-confidence records.

## Usage

``` r
struct_to_df(sess, label, min_confidence = NULL)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Label matching a prior
  [`select_struct()`](https://cathalbyrnegit.github.io/macrox/reference/select_struct.md)
  call.

- min_confidence:

  Numeric in `[0, 1]`. Records whose mean field confidence is below this
  threshold are dropped. `NULL` (default) keeps all.

## Value

`sess` invisibly (step recorded).
