# --------------------------------------------------------------------------- #
#  Package hooks                                                               #
# --------------------------------------------------------------------------- #

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("reticulate", quietly = TRUE) &&
      !reticulate::py_available(initialize = FALSE) &&
      reticulate::virtualenv_exists("r-pdfmacro")) {
    reticulate::use_virtualenv("r-pdfmacro", required = FALSE)
  }
}
