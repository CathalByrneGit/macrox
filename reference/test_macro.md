# Snapshot-test a macro against a stored reference

Replays a macro (or accepts a pre-replayed table list) and compares the
output against a stored YAML snapshot. On the first call the snapshot is
written; subsequent calls compare against it. Designed for use inside
[`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
blocks but also works standalone.

## Usage

``` r
test_macro(
  file = NULL,
  macro = NULL,
  tables = NULL,
  macro_path = ".",
  params = list(),
  snapshot_dir = file.path("tests", "testthat", "macro-snapshots"),
  name = NULL,
  update = identical(Sys.getenv("TESTTHAT_SNAPSHOT_UPDATE"), "true"),
  n_head = 5L
)
```

## Arguments

- file:

  Path to the PDF file. Ignored when `tables` is supplied.

- macro:

  Macro name, path, or step list. Ignored when `tables` is supplied.

- tables:

  A pre-replayed named list of data frames (the result of
  [`mx_replay()`](https://cathalbyrnegit.github.io/macrox/reference/mx_replay.md)).
  When supplied, `file` and `macro` are ignored.

- macro_path:

  Directory for macro lookup (default `.`).

- params:

  Named list of macro parameter values passed to
  [`mx_replay()`](https://cathalbyrnegit.github.io/macrox/reference/mx_replay.md).

- snapshot_dir:

  Directory in which to read/write snapshot files (default
  `"tests/testthat/macro-snapshots"`).

- name:

  Snapshot file stem. Defaults to the macro name when `macro` is a
  string, otherwise `"macro_snapshot"`.

- update:

  If `TRUE`, overwrite the stored snapshot with the current output. Also
  triggered by the environment variable `TESTTHAT_SNAPSHOT_UPDATE=true`
  (set automatically by
  [`testthat::snapshot_accept()`](https://testthat.r-lib.org/reference/snapshot_accept.html)).

- n_head:

  Number of header rows captured in the snapshot for spot-checking
  (default 5).

## Value

The replayed table list, invisibly.

## Details

**Workflow:**

1.  Call `test_macro(file, macro = "my_macro")` locally with the
    reference PDF. The snapshot is written to `snapshot_dir`.

2.  Commit the snapshot file alongside your package.

3.  In CI, either replay with the same PDF or supply `tables` directly.
    `test_macro()` compares against the committed snapshot and fails on
    drift.
