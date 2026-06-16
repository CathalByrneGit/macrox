#' @importFrom cli cli_inform cli_warn cli_abort cli_h2 cli_h3 cli_rule cli_bullets
#' @importFrom rlang `%||%`
#' @importFrom utils head
NULL

# --------------------------------------------------------------------------- #
#  Null-coalescing operator                                                     #
# --------------------------------------------------------------------------- #

`%||%` <- function(a, b) if (!is.null(a)) a else b

# --------------------------------------------------------------------------- #
#  .matrix_to_df() — internal                                                 #
#  Converts a character matrix returned by tabulapdf (output="matrix") into   #
#  a plain data.frame. Row 1 of the matrix is used as column names; the       #
#  remaining rows become the data. Falls back gracefully for non-matrix input. #
# --------------------------------------------------------------------------- #

.matrix_to_df <- function(x) {
  if (is.data.frame(x)) return(x)
  if (!is.matrix(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  if (nrow(x) == 0) {
    return(as.data.frame(matrix(character(0), ncol = ncol(x)),
                         stringsAsFactors = FALSE))
  }
  col_names <- as.character(x[1, ])
  col_names <- make.names(col_names, unique = TRUE)
  if (nrow(x) == 1) {
    df <- as.data.frame(
      matrix(character(0), nrow = 0, ncol = ncol(x)),
      stringsAsFactors = FALSE
    )
  } else {
    df <- as.data.frame(x[-1, , drop = FALSE], stringsAsFactors = FALSE)
  }
  names(df) <- col_names
  rownames(df) <- NULL
  df
}



# --------------------------------------------------------------------------- #
#  pdf_session()                                                                #
# --------------------------------------------------------------------------- #

#' Open a pdfmacro session
#'
#' Creates a session object that holds the PDF path, extracted tables, and a
#' running log of every recorded step.
#'
#' @param path Path to a PDF file.
#' @return An invisible `pdfmacro_session` environment.
#' @export
pdf_session <- function(path = NULL) {
  
  if(is.null(path)){
    
    cli::cli_inform('No file given: Please choose a file')
    
    if(rstudioapi::isAvailable()){
      
      path <- rstudioapi::selectFile(filter = '(*.pdf)')
      
    }else{
      
      path <- file.choose()
      
    }
    
  }
  
  
  if(is.null(path)) cli::cli_abort('No file chosen')
  
  path <- normalizePath(path, mustWork = FALSE)
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
    
  }

  sess <- new.env(parent = emptyenv())
  sess$path       <- path
  sess$tables     <- list()
  sess$items      <- list()
  sess$steps      <- list()
  sess$text       <- NULL   # lazy page-text cache
  sess$detect     <- NULL   # detect_tables() output cache

  class(sess) <- "pdfmacro_session"
  cli::cli_inform(c("v" = "Session opened: {.file {basename(path)}}"))
  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  print method                                                                 #
# --------------------------------------------------------------------------- #

#' @export
print.pdfmacro_session <- function(x, ...) {
  n_tables <- length(x$tables)
  n_steps  <- length(x$steps)
  labels   <- if (n_tables > 0) paste(names(x$tables), collapse = "' '") else "(none)"

  cat("pdfmacro session\n")
  cat("File:  ", basename(x$path), "\n")
  cat("Tables:", n_tables, "extracted")
  if (n_tables > 0) cat("  '", labels, "'", sep = "")
  cat("\n")
  cat("Steps: ", n_steps, "recorded\n")

  flagged <- sum(vapply(x$steps, function(s) isTRUE(s$.flagged), logical(1)))
  if (flagged > 0) {
    cli::cli_warn(c(
      "!" = "{flagged} flagged step{?s} detected.",
      "i" = "Run {.fn show_steps} to review."
    ))
  }
  invisible(x)
}


# --------------------------------------------------------------------------- #
#  record_step() — internal                                                     #
# --------------------------------------------------------------------------- #

record_step <- function(sess, step_list) {
  # Replay guard — don't re-record when replaying
  if (isTRUE(sess$.replaying)) return(invisible(NULL))

  dup <- .check_duplicate(sess$steps, step_list)

  if (dup$type == "exact") {
    cli::cli_warn(c(
      "!" = "Duplicate step skipped: {.val {step_list$step}} on {.val {step_list$table %||% step_list$label}}",
      "i" = "Identical to step [{dup$index}]. Use {.fn remove_step} to replace it."
    ))
    return(invisible(NULL))
  }

  if (dup$type == "overwrite_table") {
    cli::cli_warn(c(
      "!" = "Table {.val {step_list$label}} already extracted at step [{dup$index}].",
      "i" = "On replay this will overwrite the earlier extraction."
    ))
    step_list$.flagged <- TRUE
    step_list$.flag    <- paste0("Overwrites extraction at step [", dup$index, "]")
  }

  if (dup$type == "repeat_transform") {
    cli::cli_warn(c(
      "!" = "{.val {step_list$step}} already applied to {.val {step_list$table}} at step [{dup$index}].",
      "i" = "Consider removing one with {.fn remove_step}."
    ))
    step_list$.flagged <- TRUE
    step_list$.flag    <- paste0("Repeats step [", dup$index, "]")
  }

  sess$steps <- c(sess$steps, list(step_list))
  invisible(NULL)
}


# --------------------------------------------------------------------------- #
#  .check_duplicate() — internal                                               #
# --------------------------------------------------------------------------- #

.strip_internal <- function(step) {
  step[!grepl("^\\.", names(step))]
}

.check_duplicate <- function(existing, new_step) {
  new_clean <- .strip_internal(new_step)

  for (i in seq_along(existing)) {
    ex      <- existing[[i]]
    ex_clean <- .strip_internal(ex)

    # Exact match
    if (identical(ex_clean, new_clean)) {
      return(list(type = "exact", index = i))
    }

    # Same label being re-extracted (select_table with same label)
    if (!is.null(new_step$step) && new_step$step == "select_table" &&
        !is.null(ex$step)       && ex$step       == "select_table" &&
        !is.null(new_step$label) && identical(new_step$label, ex$label)) {
      return(list(type = "overwrite_table", index = i))
    }

    # Same transform repeated on same table
    transform_steps <- c("rename_columns", "cast_types", "filter_rows")
    if (!is.null(new_step$step) && new_step$step %in% transform_steps &&
        !is.null(ex$step)       && ex$step       == new_step$step &&
        !is.null(new_step$table) && identical(new_step$table, ex$table)) {
      return(list(type = "repeat_transform", index = i))
    }
  }

  list(type = "none")
}


# --------------------------------------------------------------------------- #
#  show_steps()                                                                 #
# --------------------------------------------------------------------------- #

#' Show recorded steps
#'
#' Prints a numbered list of all steps in the session, with flagged steps
#' highlighted.
#'
#' @param sess A `pdfmacro_session` object.
#' @return `sess` invisibly.
#' @export
show_steps <- function(sess) {
  n <- length(sess$steps)
  if (n == 0) {
    cli::cli_inform("No steps recorded yet.")
    return(invisible(sess))
  }

  cli::cli_inform("Recorded steps ({n} total)")

  for (i in seq_along(sess$steps)) {
    s      <- sess$steps[[i]]
    detail <- .step_detail(s)
    flag   <- isTRUE(s$.flagged)
    bullet <- if (flag) "!" else "*"
    cli::cli_bullets(setNames(
      paste0("[", i, "]  ", formatC(s$step, width = 16, flag = "-"),
             " \u2192 ",
             formatC(s$label %||% s$table %||% "?", width = 16, flag = "-"),
             " | ", detail),
      bullet
    ))
    if (flag && !is.null(s$.flag)) {
      cli::cli_bullets(c(" " = paste0("     \u26a0 FLAG: ", s$.flag)))
    }
  }
  invisible(sess)
}

.step_detail <- function(s) {
  switch(s$step,
    select_table   = {
      loc <- if (!is.null(s$area)) {
        paste0("area=[", paste(round(s$area), collapse = ","), "]")
      } else if (!is.null(s$label_match)) {
        paste0("fuzzy: '", s$label_match, "'")
      } else {
        paste0("page ", s$page, ", table ", s$table_index %||% 1)
      }
      paste0(loc, " [", s$method %||% "lattice", "]")
    },
    rename_columns = paste0(length(s$mapping), " renames"),
    cast_types     = paste0(length(s$types), " casts"),
    filter_rows    = paste0("exclude: ", s$exclude_where),
    paste0("(", s$step, ")")
  )
}


# --------------------------------------------------------------------------- #
#  remove_step()                                                                #
# --------------------------------------------------------------------------- #

#' Remove a recorded step by index
#'
#' @param sess A `pdfmacro_session` object.
#' @param index Integer index of the step to remove (see [show_steps()]).
#' @return `sess` invisibly.
#' @export
remove_step <- function(sess, index) {
  n <- length(sess$steps)
  if (n == 0) cli::cli_abort("No steps to remove.")
  if (!is.numeric(index) || index < 1 || index > n) {
    cli::cli_abort("Index must be between 1 and {n}. Got: {index}")
  }
  removed <- sess$steps[[index]]
  sess$steps <- sess$steps[-index]
  if (removed$step == "select_table" && !is.null(removed$label)) {
    sess$tables[[removed$label]] <- NULL
  }
  cli::cli_inform(c("v" = "Removed step [{index}]: {.val {removed$step}} on {.val {removed$label %||% removed$table %||% '?'}}"))
  invisible(sess)
}


# --------------------------------------------------------------------------- #
#  get_table() / set_table() — internal                                        #
# --------------------------------------------------------------------------- #

get_table <- function(sess, label) {
  if (!(label %in% names(sess$tables))) {
    cli::cli_abort("Table {.val {label}} not found. Available: {.val {names(sess$tables)}}")
  }
  sess$tables[[label]]
}

set_table <- function(sess, label, df) {
  sess$tables[[label]] <- df
  invisible(sess)
}
