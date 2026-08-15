# Load a YAML macro

Load a YAML macro

## Usage

``` r
load_macro(name, path = ".")
```

## Arguments

- name:

  Macro name (stem) or full path to a `.yml` file.

- path:

  Directory to look in when `name` has no extension (default `.`).

## Value

The list of step definitions. If the macro declares parameters, a
`params` attribute is attached — inspect with `attr(steps, "params")`.
