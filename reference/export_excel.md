# Export all extracted tables to an Excel workbook

Writes one worksheet per table. Requires the `writexl` or `openxlsx`
package.

## Usage

``` r
export_excel(sess, path = "macrox_tables.xlsx", tables = NULL)
```

## Arguments

- sess:

  A `macrox_session` object.

- path:

  Output `.xlsx` file path (default `"macrox_tables.xlsx"`).

- tables:

  Character vector of table labels to include. Default: all.

## Value

`path` invisibly.
