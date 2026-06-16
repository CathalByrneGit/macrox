# --------------------------------------------------------------------------- #
#  Package hooks                                                               #
# --------------------------------------------------------------------------- #

.onLoad <- function(libname, pkgname) {
  # Declare Python dependencies via py_require() (reticulate >= 1.41).
  # Reticulate + uv provisions these automatically into a cached ephemeral
  # environment the first time Python is initialised — no virtualenv
  # management or .Renviron configuration needed.
  #
  # Only the lightweight core is declared here. paddlepaddle (~200 MB) is
  # declared when the user explicitly calls setup_paddle(backend = "paddle"),
  # keeping the default footprint small.
  if (requireNamespace("reticulate", quietly = TRUE) &&
      utils::packageVersion("reticulate") >= "1.41") {
    reticulate::py_require(c("paddleocr", "paddlex[ocr]", "onnxruntime"))
  }
}
