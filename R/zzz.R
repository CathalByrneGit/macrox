# --------------------------------------------------------------------------- #
#  Package hooks                                                               #
# --------------------------------------------------------------------------- #

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("reticulate", quietly = TRUE) &&
      !reticulate::py_available(initialize = FALSE) &&
      reticulate::virtualenv_exists("r-macrox")) {
    reticulate::use_virtualenv("r-macrox", required = FALSE)
  }

  # Register the blockr blocks whenever blockr.core is loaded, without loading
  # it ourselves.
  register <- function(...) {
    tryCatch(register_macrox_blocks(), error = function(e) NULL)
  }
  if ("blockr.core" %in% loadedNamespaces()) {
    register()
  } else {
    setHook(packageEvent("blockr.core", "onLoad"), register)
  }
}
