# Replay a macro across multiple PDF files

Replay a macro across multiple PDF files

## Usage

``` r
mx_replay_batch(
  files,
  macro,
  macro_path = ".",
  params = list(),
  .progress = TRUE,
  .parallel = FALSE
)
```

## Arguments

- files:

  Character vector of PDF file paths.

- macro:

  Macro name, path, or step list (see
  [`mx_replay()`](https://cathalbyrnegit.github.io/macrox/reference/mx_replay.md)).

- macro_path:

  Directory for macro lookup (default `.`).

- params:

  Named list of parameter values passed to each
  [`mx_replay()`](https://cathalbyrnegit.github.io/macrox/reference/mx_replay.md)
  call. Useful when all files share the same parameters. To vary
  parameters per file, call
  [`mx_replay()`](https://cathalbyrnegit.github.io/macrox/reference/mx_replay.md)
  directly in a loop.

- .progress:

  Show file-level progress messages (default TRUE).

- .parallel:

  If `TRUE`, run files in parallel using purrr + mirai. Requires
  `purrr >= 1.1.0` and `mirai`. Set up workers first with
  `mirai::daemons(n)`. Defaults to `FALSE` (sequential).

## Value

Named list (file basenames) of table lists. Files that fail are NULL.
