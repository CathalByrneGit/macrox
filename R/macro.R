#' Save the session's recorded steps as a YAML macro
#'
#' @param sess A `pdfmacro_session` object.
#' @param name Macro name (used as the filename stem).
#' @param path Directory to write the `.yml` file (default `.`).
#' @param overwrite Logical; overwrite an existing file? (default FALSE)
#' @return The output file path, invisibly.
#' @export
save_macro <- function(sess, name, path = ".", overwrite = FALSE) {
  if (length(sess$steps) == 0) {
    cli::cli_abort("No steps recorded. Nothing to save.")
  }

  out_file <- file.path(path, paste0(name, ".yml"))

  if (file.exists(out_file) && !overwrite) {
    cli::cli_abort(c(
      "File already exists: {.path {out_file}}",
      "i" = "Use {.code overwrite = TRUE} to replace it."
    ))
  }

  # Strip internal dot-fields before serialising
  clean_steps <- lapply(sess$steps, function(s) {
    s[!grepl("^\\.", names(s))]
  })

  macro <- list(
    macro = list(
      name     = name,
      created  = format(Sys.time(), "%Y-%m-%d %H:%M"),
      source   = basename(sess$path),
      n_steps  = length(clean_steps)
    ),
    steps = clean_steps
  )

  yaml::write_yaml(macro, out_file)
  cli::cli_inform(c(
    "v" = "Macro saved: {.path {out_file}}",
    "i" = "{length(clean_steps)} step{?s} recorded."
  ))
  invisible(out_file)
}


#' Load a YAML macro
#'
#' @param name Macro name (stem) or full path to a `.yml` file.
#' @param path Directory to look in when `name` has no extension (default `.`).
#' @return The list of step definitions.
#' @export
load_macro <- function(name, path = ".") {
  if (grepl("\\.yml$", name)) {
    yml_file <- name
  } else {
    yml_file <- file.path(path, paste0(name, ".yml"))
  }

  if (!file.exists(yml_file)) {
    cli::cli_abort("Macro file not found: {.path {yml_file}}")
  }

  macro <- yaml::read_yaml(yml_file)

  hdr <- macro$macro
  cli::cli_inform(c(
    "v" = "Loaded macro: {.val {hdr$name}}",
    "i" = "{hdr$n_steps} step{?s} | created {hdr$created} | source: {hdr$source}"
  ))

  macro$steps
}


#' Replay a macro against a new PDF file
#'
#' @param file Path to the PDF file.
#' @param macro Either a macro name / path (character) or a step list returned
#'   by [load_macro()].
#' @param macro_path Directory to look for the macro file (default `.`).
#' @return Named list of extracted data frames.
#' @export
pdf_replay <- function(file, macro, macro_path = ".") {
  if (is.character(macro)) {
    steps <- load_macro(macro, path = macro_path)
  } else {
    steps <- macro
  }

  sess              <- pdf_session(file)
  sess$.replaying   <- TRUE

  for (i in seq_along(steps)) {
    step <- steps[[i]]
    cli::cli_inform("Replaying step [{i}/{length(steps)}]: {.val {step$step}}")
    tryCatch(
      .dispatch_step(sess, step),
      error = function(e) cli::cli_abort(
        "Replay failed at step [{i}] ({step$step} / {step$label %||% step$table %||% '?'}): {conditionMessage(e)}"
      )
    )
  }

  sess$.replaying <- NULL
  cli::cli_inform(c("v" = "Replay complete. {length(sess$tables)} table{?s} extracted."))
  sess$tables
}


#' Replay a macro across multiple PDF files
#'
#' @param files Character vector of PDF file paths.
#' @param macro Macro name, path, or step list (see [pdf_replay()]).
#' @param macro_path Directory for macro lookup (default `.`).
#' @param .progress Show file-level progress messages (default TRUE).
#' @return Named list (file basenames) of table lists. Files that fail are NULL.
#' @export
pdf_replay_batch <- function(files, macro, macro_path = ".", .progress = TRUE) {
  if (is.character(macro)) {
    steps <- load_macro(macro, path = macro_path)
  } else {
    steps <- macro
  }

  results  <- vector("list", length(files))
  names(results) <- basename(files)
  n_ok  <- 0L
  n_fail <- 0L

  for (i in seq_along(files)) {
    f <- files[[i]]
    if (.progress) cli::cli_inform("Processing [{i}/{length(files)}]: {.file {basename(f)}}")
    result <- tryCatch(
      {
        n_ok <<- n_ok + 1L
        pdf_replay(f, steps)
      },
      error = function(e) {
        n_ok  <<- n_ok - 1L
        n_fail <<- n_fail + 1L
        cli::cli_warn("Failed: {.file {basename(f)}} — {conditionMessage(e)}")
        NULL
      }
    )
    results[[i]] <- result
  }

  cli::cli_inform(c("v" = "Batch complete: {n_ok} succeeded, {n_fail} failed."))
  results
}


# --------------------------------------------------------------------------- #
#  .dispatch_step() — internal                                                 #
# --------------------------------------------------------------------------- #

.dispatch_step <- function(sess, step) {
  switch(step$step,

    select_table = select_table(
      sess,
      label        = step$label,
      page         = step$page,
      table_index  = step$table_index %||% 1L,
      area         = step$area,
      label_match  = step$label_match,
      method       = step$method %||% "lattice",
      header_rows  = step$header_rows %||% 1L,
      fuzzy_method = step$fuzzy_method %||% "jw",
      max_dist     = step$max_dist %||% 0.2,
      row_tol      = step$row_tol,
      col_gap      = step$col_gap
    ),

    rename_columns = rename_columns(
      sess,
      table   = step$table,
      mapping = unlist(step$mapping)
    ),

    cast_types = cast_types(
      sess,
      table = step$table,
      types = unlist(step$types)
    ),

    filter_rows = filter_rows(
      sess,
      table         = step$table,
      exclude_where = step$exclude_where
    ),

    select_table_llm = select_table_llm(
      sess,
      label       = step$label,
      page        = step$page,
      area        = step$area,
      provider    = step$provider    %||% "anthropic",
      model       = step$model,
      base_url    = step$base_url,
      schema      = if (!is.null(step$schema)) unlist(step$schema) else NULL,
      prompt      = step$prompt,
      dpi         = step$dpi         %||% 150L,
      header_rows = step$header_rows %||% 1L
    ),

    add_column = add_column(
      sess,
      table = step$table,
      name  = step$name,
      expr  = step$expr
    ),

    stack_tables = stack_tables(
      sess,
      label  = step$label,
      tables = unlist(step$tables),
      .fill  = isTRUE(step$.fill)
    ),

    merge_tables = merge_tables(
      sess,
      label = step$label,
      left  = step$left,
      right = step$right,
      by    = unlist(step$by),
      all   = isTRUE(step$all),
      all.x = isTRUE(step$all.x),
      all.y = isTRUE(step$all.y)
    ),

    validate_table = validate_table(
      sess,
      table  = step$table,
      rules  = unlist(step$rules),
      strict = isTRUE(step$strict)
    ),

    select_item = select_item(
      sess,
      label    = step$label,
      prompt   = step$prompt,
      cast     = step$cast,
      page     = step$page,
      area     = step$area,
      provider = step$provider %||% "anthropic",
      model    = step$model,
      base_url = step$base_url,
      dpi      = step$dpi %||% 120L
    ),

    select_table_paddle = select_table_paddle(
      sess,
      label       = step$label,
      page        = step$page,
      area        = step$area,
      dpi         = step$dpi         %||% 150L,
      header_rows = step$header_rows %||% 1L,
      table_index = step$table_index %||% 1L,
      device      = step$device      %||% "cpu",
      backend     = step$backend     %||% "auto"
    ),

    cli::cli_abort("Unknown step type: {.val {step$step}}")
  )
}
