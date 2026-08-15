# Export all extracted tables to CSV files

Writes one `.csv` per table into `dir`. Existing files are overwritten.

## Usage

``` r
export_csv(sess, dir = ".", tables = NULL)
```

## Arguments

- sess:

  A `macrox_session` object.

- dir:

  Directory to write into (created if it doesn't exist).

- tables:

  Character vector of table labels to export. Default: all tables.

## Value

Named character vector of output file paths, invisibly.
