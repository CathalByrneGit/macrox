# --------------------------------------------------------------------------- #
#  PaddleOCR PP-StructureV3 extraction engine                                 #
#                                                                              #
#  Follows reticulate package best-practices:                                 #
#  - Python helpers bundled in inst/python/paddle_helpers.py                  #
#  - loaded lazily on first use via source_python()                            #
#  - paddleocr module imported with delay_load = TRUE so the R package        #
#    loads cleanly even when Python / PaddleOCR is not installed              #
#  - .onLoad() calls configure_environment() so user env settings are         #
#    respected (RETICULATE_PYTHON, use_virtualenv(), etc.)                    #
# --------------------------------------------------------------------------- #

# Module-level reference — populated lazily, never at package load time.
# import(..., delay_load = TRUE) defers the actual Python import until the
# object is first used; the package loads fine with no Python installed.
paddleocr_mod <- NULL   # set on first .init_paddle()

.onLoad_paddle <- function() {
  paddleocr_mod <<- reticulate::import("paddleocr", delay_load = TRUE)
}

# Package-level cache for the loaded helpers flag and pipeline
.pdfmacro_env <- new.env(parent = emptyenv())


# --------------------------------------------------------------------------- #
#  Public API                                                                  #
# --------------------------------------------------------------------------- #

#' Install PaddleOCR into a Python environment
#'
#' Creates (or reuses) a conda/virtualenv environment and installs
#' PaddleOCR and a suitable inference backend.
#' Run once before using `select_table_paddle()`, then restart R.
#'
#' ## Choosing a backend
#'
#' PaddleOCR 3.x supports multiple inference backends. The default is
#' `backend = "onnxruntime"` which is strongly recommended for restricted
#' environments:
#' * Only ~20 MB (vs ~200 MB for PaddlePaddle).
#' * Ships on standard PyPI — internal mirrors almost always carry it.
#' * No `paddlepaddle` package needed at all.
#' * Models are auto-converted to ONNX on first use (internet required once).
#'
#' Use `backend = "paddle"` if you need GPU inference or are installing from
#' offline wheels that include paddlepaddle.
#'
#' @param envname Name of the Python environment (default `"r-pdfmacro"`).
#' @param backend Inference backend: `"onnxruntime"` (default, CPU-only,
#'   lightweight) or `"paddle"` (supports GPU via `cuda_version`).
#' @param cuda_version CUDA version string for GPU builds, e.g. `"12.6"`.
#'   Only used when `backend = "paddle"`. Supported: `"11.8"`, `"12.0"`,
#'   `"12.3"`, `"12.4"`, `"12.6"`.
#' @param index_url Custom PyPI index URL for restricted networks, e.g.
#'   `"https://packagemanager.posit.co/pypi/latest/simple"`.
#' @param proxy HTTP/HTTPS proxy URL, e.g. `"http://proxy.corp:8080"`.
#' @param pip_options Extra pip flags, e.g.
#'   `c("--trusted-host", "pypi.internal.corp")`.
#' @param wheels_dir Path to a local directory of pre-downloaded `.whl` files
#'   for fully offline install. Use [download_paddle_wheels()] to prepare it.
#' @return Invisible `NULL`.
#'
#' @examples
#' \dontrun{
#' # Recommended for restricted networks — only needs onnxruntime from PyPI
#' setup_paddle()
#'
#' # Internal mirror (onnxruntime is almost always available there)
#' setup_paddle(index_url = "https://packagemanager.posit.co/pypi/latest/simple")
#'
#' # GPU via PaddlePaddle backend — CUDA 12.6
#' setup_paddle(backend = "paddle", cuda_version = "12.6")
#'
#' # Offline: download wheels first (on an internet machine), then:
#' download_paddle_wheels("~/paddleocr_wheels")
#' setup_paddle(wheels_dir = "/path/on/server/paddleocr_wheels")
#' }
#' @export
setup_paddle <- function(envname      = "r-pdfmacro",
                          backend      = c("onnxruntime", "paddle"),
                          cuda_version = NULL,
                          index_url    = NULL,
                          proxy        = NULL,
                          pip_options  = character(0),
                          wheels_dir   = NULL) {
  backend <- match.arg(backend)

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg reticulate} is required.",
      "i" = "Install with: {.code install.packages('reticulate')}"
    ))
  }

  # ── Offline install from a local wheels directory ──────────────────────────
  if (!is.null(wheels_dir)) {
    wheels_dir <- normalizePath(wheels_dir, mustWork = TRUE)
    cli::cli_inform(c(
      "i" = "Offline install from {.path {wheels_dir}}..."
    ))
    reticulate::py_install(
      packages    = c("paddleocr", "paddlex[ocr]"),
      envname     = envname,
      pip         = TRUE,
      pip_options = c("--no-index", "--find-links", wheels_dir, pip_options)
    )
    cli::cli_inform(c("v" = "PaddleOCR installed from local wheels. Restart R."))
    return(invisible(NULL))
  }

  # ── Build base pip options ─────────────────────────────────────────────────
  extra <- pip_options
  if (!is.null(proxy))     extra <- c(extra, "--proxy", proxy)
  if (!is.null(index_url)) extra <- c(extra, "--index-url", index_url)

  # ── Backend-specific install ───────────────────────────────────────────────
  if (backend == "onnxruntime") {
    # Lightweight CPU backend — no paddlepaddle needed.
    # onnxruntime is on standard PyPI so internal mirrors usually carry it.
    cli::cli_inform(c(
      "i" = "Installing paddleocr + onnxruntime (CPU backend) into {.val {envname}}...",
      "i" = "This is the recommended option for restricted networks (~20 MB backend)."
    ))
    reticulate::py_install(
      packages    = c("paddleocr", "paddlex[ocr]", "onnxruntime"),
      envname     = envname,
      pip         = TRUE,
      pip_options = extra
    )
    cli::cli_inform(c(
      "v" = "Done. Restart R before first use.",
      "i" = "Models (~200 MB) download automatically on first extraction call."
    ))

  } else {
    # PaddlePaddle backend — supports GPU but is a heavier install
    if (!is.null(cuda_version)) {
      cv       <- gsub("\\.", "", as.character(cuda_version))
      cuda_idx <- paste0(
        "https://www.paddlepaddle.org.cn/packages/stable/cu", cv, "/"
      )
      cli::cli_inform(c(
        "i" = "Installing paddlepaddle-gpu (CUDA {cuda_version}) + paddleocr",
        "i" = "CUDA index: {.url {cuda_idx}}"
      ))
      reticulate::py_install(
        packages    = c("paddlepaddle-gpu", "paddleocr", "paddlex[ocr]"),
        envname     = envname,
        pip         = TRUE,
        pip_options = c(extra, "--extra-index-url", cuda_idx)
      )
    } else {
      cli::cli_inform(c(
        "i" = "Installing paddlepaddle (CPU) + paddleocr into {.val {envname}}..."
      ))
      if (utils::packageVersion("reticulate") >= "1.41")
        reticulate::py_require("paddlepaddle")
      reticulate::py_install(
        packages    = c("paddlepaddle", "paddleocr", "paddlex[ocr]"),
        envname     = envname,
        pip         = TRUE,
        pip_options = extra
      )
    }
    cli::cli_inform(c(
      "v" = "Done. Restart R before first use.",
      if (!is.null(cuda_version))
        c("i" = "Pass {.code device = 'gpu'} to {.fn select_table_paddle} to activate GPU.")
      else character(0)
    ))
  }

  invisible(NULL)
}





#' Extract a table using PP-StructureV3 (PaddleOCR, offline)
#'
#' Renders the PDF page (optionally cropped to `area`), passes the image to
#' PP-StructureV3, and returns a clean data frame. Works fully offline — no
#' API keys required. Ideal for scanned PDFs and complex layouts where `bbox`
#' fails.
#'
#' @param sess A `pdfmacro_session` object.
#' @param label Character label for the extracted table.
#' @param page Page number (integer).
#' @param area Named `c(top, left, bottom, right)` in PDF points. When
#'   supplied the image is cropped before being sent — faster and more accurate.
#'   Strongly recommended for pages that mix charts and tables: cropping to the
#'   table region removes chart noise and improves cell recognition.
#' @param dpi Render resolution (default 200). Higher values improve OCR
#'   accuracy on pages with dense numbers; 300 is a safe maximum.
#' @param header_rows Number of header rows (default 1).
#' @param table_index Which table to use when multiple are detected (default 1).
#' @param device `"cpu"` (default) or `"gpu"`. GPU requires a GPU build of
#'   PaddlePaddle installed via [setup_paddle()].
#' @param backend Inference backend: `"auto"` (default — uses whatever is
#'   installed), `"onnxruntime"`, or `"paddle"`. Override when both backends
#'   are installed and you want to force a specific one.
#' @return `sess` invisibly (step is recorded).
#' @export
select_table_paddle <- function(sess, label,
                                 page,
                                 area        = NULL,
                                 dpi         = 200L,
                                 header_rows = 1L,
                                 table_index = 1L,
                                 device      = c("cpu", "gpu"),
                                 backend     = c("auto", "onnxruntime", "paddle"),
                                 debug       = FALSE) {
  device  <- match.arg(device)
  backend <- match.arg(backend)
  use_gpu <- device == "gpu"

  # Ensure paddlepaddle is available when the paddle backend is needed.
  # py_require() works when reticulate manages Python via uv (recommended).
  # py_install() is the fallback for self-managed environments (virtualenvs
  # the user created manually, or RETICULATE_PYTHON pointing to a system Python).
  if (backend %in% c("paddle", "auto") &&
      requireNamespace("reticulate", quietly = TRUE)) {
    if (utils::packageVersion("reticulate") >= "1.41") {
      reticulate::py_require("paddlepaddle")
    }
    # Also install directly in case py_require has no effect on this environment
    if (!reticulate::py_module_available("paddle")) {
      cli::cli_inform(c(
        "i" = "Installing paddlepaddle + paddlex[ocr] into the active Python environment...",
        "i" = "This is a one-time ~200 MB download."
      ))
      install_failed <- tryCatch({
        reticulate::py_install(c("paddlepaddle", "paddlex[ocr]"), pip = TRUE)
        FALSE
      }, error = function(e) TRUE, warning = function(w) TRUE)

      if (install_failed && !reticulate::py_module_available("paddle")) {
        cli::cli_warn(c(
          "!" = "Could not install paddlepaddle into this Python environment automatically.",
          "i" = "{.code py_require(\"paddlepaddle\")} has been requested for this session, but reticulate's ephemeral environment may need a restart to apply it.",
          "i" = "Restart R and re-run this call. If the error persists, install manually with {.fn setup_paddle}."
        ))
      }
    } else if (!reticulate::py_module_available("einops")) {
      # paddlepaddle present but paddlex[ocr] extras missing — einops is
      # one of the OCR-extra-only deps so its absence reliably detects this.
      cli::cli_inform(c("i" = "Installing missing paddlex[ocr] extras..."))
      if (utils::packageVersion("reticulate") >= "1.41") {
        reticulate::py_require("paddlex[ocr]")
      } else {
        reticulate::py_install("paddlex[ocr]", pip = TRUE)
      }
    }
  }

  .require_paddle()

  # Render page → temp PNG
  cli::cli_inform(c("i" = "Rendering page {page} for PP-StructureV3..."))
  img_path <- .render_page_for_llm(sess$path, page, area, dpi)
  on.exit(unlink(img_path), add = TRUE)

  # Load helper if not yet done
  .load_paddle_helpers()

  # Call the bundled Python helper
  cli::cli_inform(c("i" = "Running PP-StructureV3 (first call loads models ~10 s)..."))
  result <- tryCatch(
    reticulate::py_to_r(
      reticulate::py$pdfmacro_paddle_extract(img_path, use_gpu, FALSE, backend, debug)
    ),
    error = function(e) cli::cli_abort(
      "PP-StructureV3 failed: {conditionMessage(e)}"
    )
  )

  backend_used <- result$tier
  html_list    <- result$tables

  if (length(html_list) == 0L) {
    cli::cli_abort(c(
      "PP-StructureV3 found no tables in the image.",
      "i" = "Try cropping closer to the table using {.arg area =}."
    ))
  }

  if (debug) {
    debug_file <- tempfile(pattern = "pdfmacro_paddle_html_", fileext = ".txt")
    writeLines(
      c(
        sprintf("backend: %s", backend_used),
        sprintf("tables found: %d", length(html_list)),
        "",
        vapply(seq_along(html_list), function(i) {
          paste0("=== table ", i, " ===\n", html_list[[i]])
        }, character(1L))
      ),
      debug_file
    )
    cli::cli_inform(c(
      "i" = "Raw HTML saved to: {.path {debug_file}}",
      "i" = "View with: {.code readLines({shQuote(debug_file)})}"
    ))
    message("[pdfmacro debug] Raw HTML (table ", min(as.integer(table_index), length(html_list)), "):")
    message(html_list[[min(as.integer(table_index), length(html_list))]])
  }

  idx <- min(as.integer(table_index), length(html_list))
  df  <- .paddle_html_to_df(html_list[[idx]], header_rows = header_rows)

  if (nrow(df) == 0L) cli::cli_abort("PP-StructureV3 returned an empty table.")

  set_table(sess, label, df)

  record_step(sess, list(
    step        = "select_table_paddle",
    label       = label,
    page        = page,
    area        = area,
    dpi         = as.integer(dpi),
    header_rows = as.integer(header_rows),
    table_index = as.integer(table_index),
    device      = device,
    backend     = backend,
    backend_used = backend_used,
    method      = "paddle"
  ))

  cli::cli_inform(c(
    "v" = "Table {.val {label}} extracted via PP-StructureV3 ({backend_used} backend): {nrow(df)} x {ncol(df)}"
  ))
  invisible(sess)
}


#' Release the cached PaddleOCR helpers from the reticulate session
#'
#' @return Invisible `NULL`.
#' @export
close_paddle <- function() {
  if (isTRUE(get0("paddle_helpers_loaded", envir = .pdfmacro_env))) {
    tryCatch(
      reticulate::py_run_string("del pdfmacro_paddle_extract"),
      error = function(e) NULL
    )
    rm("paddle_helpers_loaded", envir = .pdfmacro_env)
    cli::cli_inform(c("v" = "PaddleOCR helpers released."))
  } else {
    cli::cli_inform("PaddleOCR helpers are not currently loaded.")
  }
  invisible(NULL)
}


# --------------------------------------------------------------------------- #
#  Internal helpers                                                            #
# --------------------------------------------------------------------------- #

.require_paddle <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg reticulate} is required for PaddleOCR extraction.",
      "i" = "Install with: {.code install.packages('reticulate')}"
    ))
  }
}

# Load the bundled Python helpers via source_python() — idempotent.
.load_paddle_helpers <- function() {
  if (!isTRUE(get0("paddle_helpers_loaded", envir = .pdfmacro_env))) {
    py_file <- system.file("python/paddle_helpers.py", package = "pdfmacro")
    if (!nzchar(py_file)) {
      cli::cli_abort("Could not find inst/python/paddle_helpers.py in the pdfmacro package.")
    }
    reticulate::source_python(py_file)
    assign("paddle_helpers_loaded", TRUE, envir = .pdfmacro_env)
  }
}

# Parse PP-StructureV3 pred_html → data frame via xml2
# Handles colspan and rowspan correctly using a slot-tracker approach:
# slots[c] holds the rowspan counter for column c; when > 0 the column is
# still occupied by a cell from an earlier row and must be skipped.
.paddle_html_to_df <- function(html, header_rows = 1L) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg xml2} is required to parse PP-StructureV3 output.",
      "i" = "Install with: {.code install.packages('xml2')}"
    ))
  }

  .iattr <- function(node, attr) {
    v <- suppressWarnings(as.integer(xml2::xml_attr(node, attr)))
    if (is.na(v) || v < 1L) 1L else v
  }

  doc  <- xml2::read_html(html)
  rows <- xml2::xml_find_all(doc, ".//tr")
  if (length(rows) == 0L) {
    cli::cli_abort("No table rows found in PP-StructureV3 HTML output.")
  }
  nrows <- length(rows)

  # Pass 1: determine number of columns
  max_cols <- 0L
  slots    <- integer(0)
  for (ri in seq_len(nrows)) {
    if (length(slots)) slots <- pmax(slots - 1L, 0L)
    cells <- xml2::xml_find_all(rows[[ri]], ".//td|.//th")
    ci    <- 1L
    for (cell in cells) {
      while (length(slots) >= ci && slots[ci] > 0L) ci <- ci + 1L
      cs      <- .iattr(cell, "colspan")
      rs      <- .iattr(cell, "rowspan")
      end_col <- ci + cs - 1L
      if (end_col > length(slots)) length(slots) <- end_col
      for (cc in seq(ci, end_col)) slots[cc] <- rs
      ci <- end_col + 1L
    }
    max_cols <- max(max_cols, length(slots))
  }
  if (max_cols == 0L) cli::cli_abort("Could not determine table dimensions.")

  # Pass 2: fill grid respecting both colspan and rowspan
  grid  <- matrix("", nrow = nrows, ncol = max_cols)
  slots <- integer(0)
  for (ri in seq_len(nrows)) {
    if (length(slots)) slots <- pmax(slots - 1L, 0L)
    cells <- xml2::xml_find_all(rows[[ri]], ".//td|.//th")
    ci    <- 1L
    for (cell in cells) {
      while (length(slots) >= ci && slots[ci] > 0L) ci <- ci + 1L
      if (ci > max_cols) break
      val     <- trimws(xml2::xml_text(cell))
      cs      <- .iattr(cell, "colspan")
      rs      <- .iattr(cell, "rowspan")
      end_col <- min(ci + cs - 1L, max_cols)
      end_row <- min(ri + rs - 1L, nrows)
      grid[ri:end_row, ci:end_col] <- val
      if (end_col > length(slots)) length(slots) <- end_col
      for (cc in seq(ci, end_col)) slots[cc] <- rs
      ci <- end_col + 1L
    }
  }

  hr <- max(1L, min(as.integer(header_rows), nrows - 1L))
  if (hr == 1L) {
    hdrs <- make.names(grid[1L, ], unique = TRUE)
    df   <- as.data.frame(grid[-1L, , drop = FALSE], stringsAsFactors = FALSE)
  } else {
    hdrs <- apply(grid[seq_len(hr), , drop = FALSE], 2L, function(col) {
      paste(unique(col[nchar(trimws(col)) > 0L]), collapse = "_")
    })
    hdrs <- make.names(hdrs, unique = TRUE)
    df   <- as.data.frame(grid[-seq_len(hr), , drop = FALSE],
                           stringsAsFactors = FALSE)
  }
  names(df)    <- hdrs
  rownames(df) <- NULL
  df
}


# --------------------------------------------------------------------------- #
#  Offline wheel download helper                                               #
# --------------------------------------------------------------------------- #

#' Generate a pip download command for offline PaddleOCR installation
#'
#' Prints (and optionally runs) the `pip download` command needed to fetch all
#' PaddleOCR wheels for your server's platform and Python version. Run this on
#' an **internet-connected** machine, copy the resulting folder to your server,
#' then call `setup_paddle(wheels_dir = "/path/to/folder")`.
#'
#' @param dest Directory to download wheels into (created if absent).
#' @param python_version Python version on the **target server**, e.g.
#'   `"3.9"` (default). Match this to what Posit Workbench uses.
#' @param platform Platform tag for the target server, e.g.
#'   `"manylinux_2_17_x86_64"` (default — Linux x86_64).
#'   Use `"win_amd64"` for Windows or `"macosx_12_0_x86_64"` for Intel Mac.
#' @param cuda_version CUDA version string for GPU wheels, e.g. `"12.6"`.
#'   `NULL` (default) downloads CPU wheels only.
#' @param run Logical; actually run the command via `system()` (default `FALSE`).
#'   When `FALSE`, just prints the command for you to run manually.
#' @return The pip command string, invisibly.
#' @export
download_paddle_wheels <- function(dest,
                                    python_version = "3.9",
                                    platform       = "manylinux_2_17_x86_64",
                                    cuda_version   = NULL,
                                    run            = FALSE) {
  pv_tag <- gsub("\\.", "", python_version)  # "3.9" -> "39"

  # For onnxruntime backend, only paddleocr + onnxruntime are needed
  packages <- c("paddleocr", "paddlex[ocr]", "onnxruntime")

  extra_idx <- if (!is.null(cuda_version)) {
    cv <- gsub("\\.", "", as.character(cuda_version))
    paste0(
      "--extra-index-url https://www.paddlepaddle.org.cn/packages/stable/cu",
      cv, "/"
    )
  } else ""

  cmd <- paste(
    "pip download",
    paste(packages, collapse = " "),
    "-d", shQuote(dest),
    "--platform", platform,
    "--python-version", pv_tag,
    "--only-binary=:all:",
    extra_idx
  )

  cli::cli_inform(c(
    "i" = "Run the following on an internet-connected machine:",
    " " = ""
  ))
  cat(cmd, "\n\n")
  cli::cli_inform(c(
    "i" = "Then copy {.path {dest}} to your server and run:",
    " " = ""
  ))
  cat(paste0("pdfmacro::setup_paddle(wheels_dir = \"", dest, "\")"), "\n")

  if (run) {
    cli::cli_inform(c("i" = "Running pip download..."))
    ret <- system(cmd)
    if (ret == 0L) {
      cli::cli_inform(c("v" = "Wheels downloaded to {.path {dest}}"))
    } else {
      cli::cli_warn("pip download exited with status {ret}.")
    }
  }

  invisible(cmd)
}
