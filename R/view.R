#' View a recorded step's location in the PDF
#'
#' Renders the PDF page for a `select_table` or `select_table_llm` step and
#' opens it in the RStudio Viewer or system browser. If the step used a
#' bounding box, the selected area is highlighted in blue. Requires `magick`
#' for annotations (Suggests).
#'
#' @param sess A `macrox_session` object.
#' @param step Integer step index. Defaults to the most recent step with a
#'   page location (`select_table` or `select_table_llm`).
#' @param dpi Render resolution (default 150).
#' @return The bounding box area (named numeric vector or NULL), invisibly.
#' @export
view_in_pdf <- function(sess, step = NULL, dpi = 150) {
  steps <- sess$steps
  page_steps <- c("select_table", "select_table_llm")

  if (is.null(step)) {
    # Find last step with a page location
    idx <- rev(which(vapply(steps, function(s) s$step %in% page_steps, logical(1))))
    if (length(idx) == 0) {
      cli::cli_abort("No {.val select_table} or {.val select_table_llm} steps recorded yet.")
    }
    step <- idx[[1]]
  }

  if (step < 1 || step > length(steps)) {
    cli::cli_abort("Step index {step} out of range (1\u2013{length(steps)}).")
  }

  s <- steps[[step]]
  if (!s$step %in% page_steps) {
    cli::cli_abort(
      "Step [{step}] is {.val {s$step}}, not {.val select_table} or {.val select_table_llm}. Only these steps have a page location."
    )
  }

  page     <- s$page
  area     <- s$area
  page_raw <- tryCatch(
    pdftools::pdf_render_page(sess$path, page = page, dpi = dpi, numeric = FALSE),
    error = function(e) cli::cli_abort("Failed to render page {page}: {conditionMessage(e)}")
  )

  img <- .annotate_page(page_raw, area, sess$path, page, dpi)

  tmp_html <- .save_viewer_html(img, step, s)

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::viewer(tmp_html)
  } else {
    utils::browseURL(tmp_html)
  }

  area_str <- if (!is.null(area)) {
    paste0("area=[", paste(round(area), collapse = ","), "]")
  } else {
    "no bounding box"
  }
  cli::cli_inform(c("i" = "Step [{step}]: {.val {s$label}} | page {page} | {area_str}"))

  invisible(area)
}


#' Interactively select a table area
#'
#' Opens a click-and-drag gadget (via `tabulapdf::locate_areas`) to capture
#' bounding box coordinates. Returns a named `c(top, left, bottom, right)`
#' vector suitable for passing directly to [select_table()].
#'
#' @param sess A `macrox_session` object.
#' @param page Page number to display (default 1).
#' @return Named numeric vector `c(top, left, bottom, right)`, invisibly.
#' @export
locate_area <- function(sess, page = 1) {
  cli::cli_inform(c(
    "i" = "Opening interactive area selector for page {page}\u2026",
    "i" = "Click and drag over the table, then click Done."
  ))

  result <- tryCatch(
    tabulapdf::locate_areas(sess$path, pages = page),
    error = function(e) cli::cli_abort("locate_areas failed: {conditionMessage(e)}")
  )

  area <- unlist(result[[1]])
  names(area) <- c("top", "left", "bottom", "right")

  cli::cli_inform(c(
    "v" = "Area captured: top={round(area['top'])}, left={round(area['left'])}, bottom={round(area['bottom'])}, right={round(area['right'])}",
    "i" = "Use with: {.code select_table(sess, label = '...', page = {page}, area = c({round(area['top'])}, {round(area['left'])}, {round(area['bottom'])}, {round(area['right'])}))}"
  ))

  invisible(area)
}


# --------------------------------------------------------------------------- #
#  .annotate_page() — internal                                                 #
# --------------------------------------------------------------------------- #

.annotate_page <- function(page_raw, area, pdf_path, page, dpi) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    cli::cli_warn("Install {.pkg magick} for bounding box highlights.")
    # Return raw bytes as-is
    return(page_raw)
  }

  img <- magick::image_read(page_raw)

  if (!is.null(area)) {
    # Get page dimensions in PDF points
    pg_size <- tryCatch(
      pdftools::pdf_pagesize(pdf_path)[page, ],
      error = function(e) NULL
    )

    if (!is.null(pg_size)) {
      info      <- magick::image_info(img)
      img_w     <- info$width
      img_h     <- info$height
      pts_w     <- pg_size$width
      pts_h     <- pg_size$height

      sx <- img_w / pts_w
      sy <- img_h / pts_h

      # area = c(top, left, bottom, right) in pts, origin top-left
      px_top    <- area[["top"]]    * sy
      px_left   <- area[["left"]]   * sx
      px_bottom <- area[["bottom"]] * sy
      px_right  <- area[["right"]]  * sx

      img <- magick::image_draw(img)
      graphics::rect(px_left, px_top, px_right, px_bottom,
                     border = "#0ea5e9", col = "#0ea5e920", lwd = 2)
      grDevices::dev.off()
    }
  }

  img
}


# --------------------------------------------------------------------------- #
#  .save_viewer_html() — internal                                              #
# --------------------------------------------------------------------------- #

.save_viewer_html <- function(img, step_idx, step_def) {
  # Save both files into the SAME temp directory and reference the image
  # with a relative src="page.png". RStudio Viewer sandboxes absolute
  # filesystem paths in <img src>, so an absolute path produces a blank image.
  tmp_dir  <- tempfile()
  dir.create(tmp_dir, showWarnings = FALSE)
  tmp_png  <- file.path(tmp_dir, "page.png")
  tmp_html <- file.path(tmp_dir, "view.html")

  if (inherits(img, "magick-image")) {
    magick::image_write(img, path = tmp_png, format = "png")
  } else {
    writeBin(img, tmp_png)
  }

  area <- step_def$area
  area_str <- if (!is.null(area)) {
    paste0("top=", round(area[["top"]]),
           " left=", round(area[["left"]]),
           " bottom=", round(area[["bottom"]]),
           " right=", round(area[["right"]]))
  } else {
    "no bounding box"
  }

  method_str <- step_def$method %||% "lattice"
  label_str  <- step_def$label  %||% "?"
  page_str   <- step_def$page   %||% "?"

  html <- paste0(
    '<!DOCTYPE html><html><head>',
    '<meta charset="UTF-8">',
    '<style>',
    'body{margin:0;padding:0;background:#1e1e2e;font-family:monospace;color:#cdd6f4;}',
    '.header{background:#313244;padding:8px 12px;font-size:13px;border-bottom:1px solid #45475a;}',
    '.header span{margin-right:16px;}',
    'img{max-width:100%;display:block;}',
    '</style></head><body>',
    '<div class="header">',
    '<span><b>Step [', step_idx, ']</b></span>',
    '<span>Label: <b>', label_str, '</b></span>',
    '<span>Page: <b>', page_str, '</b></span>',
    '<span>Method: <b>', method_str, '</b></span>',
    '<span>Area: <b>', area_str, '</b></span>',
    '</div>',
    '<img src="page.png" />',
    '</body></html>'
  )

  writeLines(html, tmp_html)
  tmp_html
}
