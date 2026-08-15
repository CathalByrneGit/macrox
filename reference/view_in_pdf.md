# View a recorded step's location in the PDF

Renders the PDF page for a `select_table` or `select_table_llm` step and
opens it in the RStudio Viewer or system browser. If the step used a
bounding box, the selected area is highlighted in blue. Requires
`magick` for annotations (Suggests).

## Usage

``` r
view_in_pdf(sess, step = NULL, dpi = 150)
```

## Arguments

- sess:

  A `macrox_session` object.

- step:

  Integer step index. Defaults to the most recent step with a page
  location (`select_table` or `select_table_llm`).

- dpi:

  Render resolution (default 150).

## Value

The bounding box area (named numeric vector or NULL), invisibly.
