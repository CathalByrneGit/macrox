# Install GLiNER2 in the macrox Python environment

Installs the `gliner2` Python package and pre-loads the requested model
so it is ready for
[`select_item()`](https://cathalbyrnegit.github.io/macrox/reference/select_item.md)
calls with `backend = "gliner"`.

## Usage

``` r
setup_gliner(
  model = "fastino/gliner2-base-v1",
  envname = "r-macrox",
  pip_options = character(0)
)
```

## Arguments

- model:

  Pre-trained model identifier (HuggingFace repo). Defaults to the 205M
  base model `"fastino/gliner2-base-v1"`. The 340M large variant is
  `"fastino/gliner2-large-v1"`.

- envname:

  Python virtual environment name (default `"r-macrox"`).

- pip_options:

  Additional pip install options (character vector).

## Value

`NULL` invisibly.
