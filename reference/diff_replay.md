# Compare macro outputs across two PDF files

Replays the same macro against two files and returns a structured diff
showing added, removed, and changed tables. For changed tables the diff
includes per-column numeric delta statistics. Use
[`detail()`](https://cathalbyrnegit.github.io/macrox/reference/detail.md)
on the result to inspect which cells changed.

## Usage

``` r
diff_replay(
  file1,
  file2,
  macro,
  macro_path = ".",
  params = list(),
  params2 = NULL
)
```

## Arguments

- file1:

  Path to the reference PDF.

- file2:

  Path to the new PDF.

- macro:

  Macro name, path to a `.yml`, or a step list.

- macro_path:

  Directory for macro lookup (default `.`).

- params:

  Named list of parameter values applied to both replays (e.g.
  `list(year = 2024L)`). Use `params2` to supply different values for
  the second file.

- params2:

  Named list of parameter values for `file2` only. Defaults to `params`
  when `NULL`.

## Value

A `macrox_diff` object. Print gives a summary;
[`detail()`](https://cathalbyrnegit.github.io/macrox/reference/detail.md)
drills into a single table.
