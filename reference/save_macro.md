# Save the session's recorded steps as a YAML macro

Save the session's recorded steps as a YAML macro

## Usage

``` r
save_macro(sess, name, path = ".", overwrite = FALSE, params = NULL)
```

## Arguments

- sess:

  A `macrox_session` object.

- name:

  Macro name (used as the filename stem).

- path:

  Directory to write the `.yml` file (default `.`).

- overwrite:

  Logical; overwrite an existing file? (default FALSE)

- params:

  Optional parameter declarations for parameterised macros. Accepts a
  named character vector `c(year = "integer", month = "character")` or a
  bare character vector of names `c("year", "month")`. Declared
  parameters can be referenced as `$name` in any step field (e.g.
  `expr = "$year"`) and supplied at replay time via
  `mx_replay(..., params = list(year = 2025L))`.

## Value

The output file path, invisibly.
