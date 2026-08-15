# Install Docling into a Python environment

Creates (or reuses) a Python environment and installs the `docling`
package. Run once and restart R before calling
[`select_table_docling()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_docling.md).

## Usage

``` r
setup_docling(envname = "r-macrox", pip_options = character(0))
```

## Arguments

- envname:

  Name of the Python environment (default `"r-macrox"`).

- pip_options:

  Extra pip flags, e.g.
  `c("--index-url", "https://pypi.internal.corp/simple")`.

## Value

Invisible `NULL`.

## Details

On the first extraction call Docling downloads its ML models (~2-3 GB)
and caches them locally. Subsequent calls are fully offline.
