# --------------------------------------------------------------------------- #
#  test_macro() — snapshot testing for macros                                 #
# --------------------------------------------------------------------------- #

#' Snapshot-test a macro against a stored reference
#'
#' Replays a macro (or accepts a pre-replayed table list) and compares the
#' output against a stored YAML snapshot. On the first call the snapshot is
#' written; subsequent calls compare against it. Designed for use inside
#' `testthat::test_that()` blocks but also works standalone.
#'
#' **Workflow:**
#' 1. Call `test_macro(file, macro = "my_macro")` locally with the reference PDF.
#'    The snapshot is written to `snapshot_dir`.
#' 2. Commit the snapshot file alongside your package.
#' 3. In CI, either replay with the same PDF or supply `tables` directly.
#'    `test_macro()` compares against the committed snapshot and fails on drift.
#'
#' @param file Path to the PDF file. Ignored when `tables` is supplied.
#' @param macro Macro name, path, or step list. Ignored when `tables` is
#'   supplied.
#' @param tables A pre-replayed named list of data frames (the result of
#'   [mx_replay()]). When supplied, `file` and `macro` are ignored.
#' @param macro_path Directory for macro lookup (default `.`).
#' @param params Named list of macro parameter values passed to [mx_replay()].
#' @param snapshot_dir Directory in which to read/write snapshot files (default
#'   `"tests/testthat/macro-snapshots"`).
#' @param name Snapshot file stem. Defaults to the macro name when `macro` is a
#'   string, otherwise `"macro_snapshot"`.
#' @param update If `TRUE`, overwrite the stored snapshot with the current
#'   output. Also triggered by the environment variable
#'   `TESTTHAT_SNAPSHOT_UPDATE=true` (set automatically by
#'   `testthat::snapshot_accept()`).
#' @param n_head Number of header rows captured in the snapshot for spot-checking
#'   (default 5).
#' @return The replayed table list, invisibly.
#' @export
test_macro <- function(file         = NULL,
                       macro        = NULL,
                       tables       = NULL,
                       macro_path   = ".",
                       params       = list(),
                       snapshot_dir = file.path("tests", "testthat",
                                                "macro-snapshots"),
                       name         = NULL,
                       update       = identical(
                         Sys.getenv("TESTTHAT_SNAPSHOT_UPDATE"), "true"
                       ),
                       n_head       = 5L) {

  if (is.null(tables) && (is.null(file) || is.null(macro))) {
    cli::cli_abort(c(
      "Provide either {.arg tables} (a named list of data frames) or both",
      "{.arg file} and {.arg macro}."
    ))
  }

  # Replay if tables not pre-supplied
  if (is.null(tables)) {
    tables <- mx_replay(file, macro, macro_path, params = params)
  }

  # Derive snapshot name
  snap_name <- name %||% {
    if (is.character(macro)) sub("\\.yml$", "", basename(macro))
    else "macro_snapshot"
  }

  snap_file   <- file.path(snapshot_dir, paste0(snap_name, ".snap.yml"))
  current_snap <- .build_macro_snapshot(tables, n_head)

  if (update || !file.exists(snap_file)) {
    dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
    yaml::write_yaml(current_snap, snap_file)
    if (file.exists(snap_file)) {
      cli::cli_inform(c(
        "v" = "Snapshot recorded: {.path {snap_file}}",
        "i" = "Commit this file to track macro output over time."
      ))
    }
    return(invisible(tables))
  }

  stored_snap <- yaml::read_yaml(snap_file)
  .compare_macro_snapshots(current_snap, stored_snap, snap_name, snap_file)

  cli::cli_inform(c("v" = "Snapshot matches: {.val {snap_name}}"))
  invisible(tables)
}


# --------------------------------------------------------------------------- #
#  Internals                                                                   #
# --------------------------------------------------------------------------- #

.build_macro_snapshot <- function(tables, n_head = 5L) {
  snap <- lapply(tables, function(df) {
    h <- utils::head(df, n_head)
    # Store head rows as lists of character strings for YAML-safe comparison
    head_rows <- lapply(seq_len(nrow(h)), function(i) {
      row <- as.list(h[i, , drop = FALSE])
      lapply(row, function(v) if (is.na(v)) NA_character_ else as.character(v))
    })
    list(
      nrow = nrow(df),
      ncol = ncol(df),
      cols = as.list(names(df)),
      head = head_rows
    )
  })
  list(tables = snap)
}

# Normalize a snapshot head list so numeric YAML coercions don't break equality.
.norm_snap_head <- function(head_rows) {
  lapply(head_rows, function(row) {
    lapply(row, function(v) {
      if (is.null(v) || (length(v) == 1L && is.na(v))) NA_character_
      else as.character(v)
    })
  })
}

.compare_macro_snapshots <- function(current, stored, snap_name, snap_file) {
  cur_tables <- names(current$tables)
  sto_tables <- names(stored$tables)

  added   <- setdiff(cur_tables, sto_tables)
  removed <- setdiff(sto_tables, cur_tables)

  failures <- character(0)

  if (length(added) > 0L)
    failures <- c(failures,
      paste0("New tables not in snapshot: ", paste(added, collapse = ", ")))
  if (length(removed) > 0L)
    failures <- c(failures,
      paste0("Tables removed since snapshot: ", paste(removed, collapse = ", ")))

  for (nm in intersect(cur_tables, sto_tables)) {
    cur <- current$tables[[nm]]
    sto <- stored$tables[[nm]]

    cur_cols <- unlist(cur$cols)
    sto_cols <- unlist(sto$cols)

    if (!identical(cur_cols, sto_cols)) {
      failures <- c(failures, paste0(
        "Table '", nm, "': column mismatch.\n",
        "  Expected: ", paste(sto_cols, collapse = ", "), "\n",
        "  Got:      ", paste(cur_cols, collapse = ", ")
      ))
    }

    cur_nrow <- unlist(cur$nrow)
    sto_nrow <- unlist(sto$nrow)
    if (!identical(as.integer(cur_nrow), as.integer(sto_nrow))) {
      failures <- c(failures, paste0(
        "Table '", nm, "': row count mismatch. ",
        "Expected ", sto_nrow, ", got ", cur_nrow, "."
      ))
    }

    if (!identical(.norm_snap_head(cur$head), .norm_snap_head(sto$head))) {
      failures <- c(failures, paste0(
        "Table '", nm, "': first rows differ from snapshot."
      ))
    }
  }

  if (length(failures) > 0L) {
    cli::cli_abort(c(
      "!" = "Snapshot mismatch for {.val {snap_name}}:",
      setNames(failures, rep("x", length(failures))),
      "i" = "Run {.code test_macro(..., update = TRUE)} to accept new output.",
      "i" = "Snapshot file: {.path {snap_file}}"
    ))
  }
}
