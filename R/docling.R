# --------------------------------------------------------------------------- #
#  Docling extraction engine                                                   #
#                                                                              #
#  Docling (IBM Research, MIT licence) converts PDFs — including scanned ones  #
#  — to structured output using ML layout models. Tables are detected and      #
#  returned as data frames without an API key or network connection after the  #
#  initial one-time model download (~2-3 GB).                                  #
#                                                                              #
#  Setup: run setup_docling() once, restart R, then use select_table_docling().#
# --------------------------------------------------------------------------- #

.pdfmacro_docling_env <- new.env(parent = emptyenv())


# --------------------------------------------------------------------------- #
#  Public API                                                                  #
# --------------------------------------------------------------------------- #

#' Install Docling into a Python environment
#'
#' Creates (or reuses) a Python environment and installs the `docling` package.
#' Run once and restart R before calling [select_table_docling()].
#'
#' On the first extraction call Docling downloads its ML models (~2-3 GB) and
#' caches them locally. Subsequent calls are fully offline.
#'
#' @param envname Name of the Python environment (default `"r-pdfmacro"`).
#' @param pip_options Extra pip flags, e.g.
#'   `c("--index-url", "https://pypi.internal.corp/simple")`.
#' @return Invisible `NULL`.
#' @export
setup_docling <- function(envname = "r-pdfmacro", pip_options = character(0)) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg reticulate} is required.",
      "i" = "Install with: {.code install.packages('reticulate')}"
    ))
  }

  cli::cli_inform(c(
    "i" = "Installing docling into {.val {envname}}...",
    "i" = "Models (~2-3 GB) download automatically on first extraction call."
  ))

  reticulate::py_install(
    packages    = "docling",
    envname     = envname,
    pip         = TRUE,
    pip_options = pip_options
  )

  cli::cli_inform(c(
    "v" = "Done. Restart R before first use."
  ))

  invisible(NULL)
}


#' Extract a table using Docling
#'
#' Converts the PDF page using Docling's ML layout pipeline and returns a
#' clean data frame. Works fully offline after the initial one-time model
#' download; handles scanned PDFs and complex layouts where `bbox` fails.
#'
#' Docling converts the target page the first time it is called and caches
#' the result in the Python session, so repeated calls for different tables
#' on the same page do not re-run the conversion pipeline. Call
#' [close_docling()] to release the cache when done.
#'
#' @param sess A `pdfmacro_session` object.
#' @param label Character label for the extracted table.
#' @param page Page number (integer).
#' @param table_index Which table to use when multiple are detected on the
#'   page (default 1).
#' @return `sess` invisibly (step is recorded).
#' @export
select_table_docling <- function(sess, label, page, table_index = 1L) {
  .require_docling()
  .load_docling_helpers()

  cli::cli_inform(c(
    "i" = "Running Docling on page {page} (first call loads models — allow 30-60 s)..."
  ))

  result <- tryCatch(
    reticulate::py_to_r(
      reticulate::py$docling_extract_tables(sess$path, as.integer(page))
    ),
    error = function(e) cli::cli_abort("Docling failed: {conditionMessage(e)}")
  )

  n <- as.integer(result$n_tables)
  if (n == 0L) {
    cli::cli_abort(c(
      "Docling found no tables on page {page}.",
      "i" = "Check that the page contains a detectable table and that Docling's models are fully downloaded."
    ))
  }

  idx <- min(as.integer(table_index), n)
  if (n > 1L) {
    cli::cli_inform(c(
      "i" = "{n} tables detected on page {page}; using table {idx}. Set {.arg table_index} to pick another."
    ))
  }

  tbl     <- result$tables[[idx]]
  headers <- unlist(tbl$headers)
  data    <- tbl$data

  mat <- do.call(rbind, lapply(data, function(row) {
    v          <- as.character(unlist(row))
    length(v)  <- length(headers)
    v[is.na(v)] <- ""
    v
  }))
  if (!is.matrix(mat)) mat <- matrix(mat, nrow = 1L, ncol = length(headers))

  df           <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df)    <- make.names(headers, unique = TRUE)
  rownames(df) <- NULL

  if (nrow(df) == 0L) cli::cli_abort("Docling returned an empty table.")

  set_table(sess, label, df)
  record_step(sess, list(
    step        = "select_table_docling",
    label       = label,
    page        = as.integer(page),
    table_index = as.integer(idx)
  ))

  cli::cli_inform(c(
    "v" = "Table {.val {label}} extracted via Docling: {nrow(df)} × {ncol(df)}"
  ))
  invisible(sess)
}


#' Release the Docling document cache
#'
#' Frees the in-memory cache of converted PDF documents. Call this when
#' you are done extracting tables to reduce memory usage.
#'
#' @return Invisible `NULL`.
#' @export
close_docling <- function() {
  if (isTRUE(get0("docling_helpers_loaded", envir = .pdfmacro_docling_env))) {
    tryCatch(
      reticulate::py_run_string("docling_clear_cache()"),
      error = function(e) NULL
    )
    cli::cli_inform(c("v" = "Docling cache cleared."))
  } else {
    cli::cli_inform("Docling helpers are not currently loaded.")
  }
  invisible(NULL)
}


# --------------------------------------------------------------------------- #
#  Internal helpers                                                            #
# --------------------------------------------------------------------------- #

.require_docling <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg reticulate} is required for Docling extraction.",
      "i" = "Install with: {.code install.packages('reticulate')}"
    ))
  }
}

.load_docling_helpers <- function(envname = "r-pdfmacro") {
  if (!isTRUE(get0("docling_helpers_loaded", envir = .pdfmacro_docling_env))) {
    if (!reticulate::py_available(initialize = FALSE)) {
      reticulate::use_virtualenv(envname, required = FALSE)
    } else if (!reticulate::py_module_available("docling")) {
      cli::cli_abort(c(
        "Python is already initialised without the {.val {envname}} virtualenv,",
        "so {.pkg docling} is not importable.",
        "i" = "Restart R and load pdfmacro before any other reticulate calls."
      ))
    }
    py_file <- system.file("python/docling_helpers.py", package = "pdfmacro")
    if (!nzchar(py_file)) {
      cli::cli_abort("Could not find inst/python/docling_helpers.py in the pdfmacro package.")
    }
    reticulate::source_python(py_file)
    assign("docling_helpers_loaded", TRUE, envir = .pdfmacro_docling_env)
  }
}
