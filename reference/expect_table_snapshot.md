# Snapshot-test an extracted table

Wraps
[`testthat::expect_snapshot()`](https://testthat.r-lib.org/reference/expect_snapshot.html)
to lock in the printed representation of a table stored in a session. On
first run the snapshot is written to `tests/testthat/_snaps/`;
subsequent runs compare against that baseline.

## Usage

``` r
expect_table_snapshot(sess, label = NULL, variant = NULL)
```

## Arguments

- sess:

  A `macrox_session` object, or a plain data frame.

- label:

  Character label of the table to snapshot. Ignored when `sess` is
  already a data frame.

- variant:

  Optional variant string passed to
  [`testthat::expect_snapshot()`](https://testthat.r-lib.org/reference/expect_snapshot.html),
  allowing multiple snapshots per test.

## Value

The result of `expect_snapshot()`, invisibly.

## Details

Must be called inside a `testthat` test (i.e., within `test_that()`).
