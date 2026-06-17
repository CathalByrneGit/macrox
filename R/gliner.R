# --------------------------------------------------------------------------- #
#  GLiNER2 backend for select_item()                                          #
#                                                                              #
#  GLiNER2 is a 205–340M parameter local NLP model that extracts structured   #
#  fields from text without any API call.  It runs on CPU.                    #
# --------------------------------------------------------------------------- #

.macrox_gliner_env <- new.env(parent = emptyenv())


#' Install GLiNER2 in the macrox Python environment
#'
#' Installs the `gliner2` Python package and pre-loads the requested model so
#' it is ready for [select_item()] calls with `backend = "gliner"`.
#'
#' @param model Pre-trained model identifier (HuggingFace repo).  Defaults to
#'   the 205M base model `"fastino/gliner2-base-v1"`.  The 340M large variant
#'   is `"fastino/gliner2-large-v1"`.
#' @param envname Python virtual environment name (default `"r-macrox"`).
#' @param pip_options Additional pip install options (character vector).
#' @return `NULL` invisibly.
#' @export
setup_gliner <- function(model       = "fastino/gliner2-base-v1",
                          envname     = "r-macrox",
                          pip_options = character(0)) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg reticulate} is required.",
      "i" = "Install with: {.code install.packages('reticulate')}"
    ))
  }
  cli::cli_inform(c("i" = "Installing GLiNER2 — this downloads ~500MB on first run..."))
  reticulate::py_install(
    "gliner2[local]",
    envname     = envname,
    pip         = TRUE,
    pip_options = pip_options
  )
  cli::cli_inform(c("i" = "Loading model {.val {model}}..."))
  .ensure_gliner(model)
  cli::cli_inform(c("v" = "GLiNER2 ready ({.val {model}})."))
  invisible(NULL)
}


#' Unload the GLiNER2 model from memory
#'
#' @return `NULL` invisibly.
#' @export
close_gliner <- function() {
  if (!is.null(.macrox_gliner_env$py)) {
    tryCatch(
      .macrox_gliner_env$py$gliner_clear(),
      error = function(e) invisible(NULL)
    )
  }
  .macrox_gliner_env$py    <- NULL
  .macrox_gliner_env$model <- NULL
  cli::cli_inform(c("v" = "GLiNER2 model unloaded."))
  invisible(NULL)
}


# Internal: source Python helpers and ensure the right model is loaded.
.ensure_gliner <- function(model = "fastino/gliner2-base-v1") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg reticulate} is required for the GLiNER backend.",
      "i" = "Install with: {.code install.packages('reticulate')}"
    ))
  }

  if (is.null(.macrox_gliner_env$py)) {
    py_file <- system.file("python", "gliner_helpers.py", package = "macrox")
    if (!nzchar(py_file)) {
      cli::cli_abort("GLiNER helper not found — try reinstalling macrox.")
    }
    reticulate::source_python(py_file)
    .macrox_gliner_env$py <- reticulate::import_main()
  }

  current <- tryCatch(
    .macrox_gliner_env$py$gliner_loaded_model(),
    error = function(e) NULL
  )

  if (!identical(current, model)) {
    cli::cli_inform(c("i" = "Loading GLiNER2 model {.val {model}}..."))
    .macrox_gliner_env$py$gliner_setup(model)
    .macrox_gliner_env$model <- model
  }

  invisible(.macrox_gliner_env$py)
}
