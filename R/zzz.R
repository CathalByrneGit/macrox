# --------------------------------------------------------------------------- #
#  Package hooks                                                               #
# --------------------------------------------------------------------------- #

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("reticulate", quietly = TRUE) &&
      !reticulate::py_available(initialize = FALSE) &&
      reticulate::virtualenv_exists("r-macrox")) {
    reticulate::use_virtualenv("r-macrox", required = FALSE)
  }
}
