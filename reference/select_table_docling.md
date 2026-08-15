# Extract a table using Docling

Converts the PDF page using Docling's ML layout pipeline and returns a
clean data frame. Works fully offline after the initial one-time model
download; handles scanned PDFs and complex layouts where `bbox` fails.

## Usage

``` r
select_table_docling(sess, label, page, table_index = 1L)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Character label for the extracted table.

- page:

  Page number (integer).

- table_index:

  Which table to use when multiple are detected on the page (default 1).

## Value

`sess` invisibly (step is recorded).

## Details

Docling converts the target page the first time it is called and caches
the result in the Python session, so repeated calls for different tables
on the same page do not re-run the conversion pipeline. Call
[`close_docling()`](https://cathalbyrnegit.github.io/macrox/reference/close_docling.md)
to release the cache when done.
