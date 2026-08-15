# Interactively select a table area

Opens a click-and-drag gadget (via
[`tabulapdf::locate_areas`](https://docs.ropensci.org/tabulapdf/reference/extract_areas.html))
to capture bounding box coordinates. Returns a named
`c(top, left, bottom, right)` vector suitable for passing directly to
[`select_table()`](https://cathalbyrnegit.github.io/macrox/reference/select_table.md).

## Usage

``` r
locate_area(sess, page = 1)
```

## Arguments

- sess:

  A `macrox_session` object.

- page:

  Page number to display (default 1).

## Value

Named numeric vector `c(top, left, bottom, right)`, invisibly.
