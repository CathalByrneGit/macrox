# --------------------------------------------------------------------------- #
#  .extract_bbox() — internal                                                  #
#                                                                              #
#  Extracts a table from a PDF page using pdftools::pdf_data() word-level     #
#  bounding boxes. Works by:                                                   #
#    1. Clipping words to the supplied area (if any)                           #
#    2. Clustering words into rows by y-coordinate proximity                   #
#    3. Detecting column boundaries from gaps between word groups per row      #
#    4. Assembling a character matrix and converting to a data frame           #
#                                                                              #
#  This approach handles PDFs that defeat tabulapdf — charts on the same      #
#  page, merged/missing grid lines, numbers without separators — because it   #
#  works from character positions rather than CSV parsing.                     #
# --------------------------------------------------------------------------- #

.extract_bbox <- function(path, page,
                           area        = NULL,
                           header_rows = 1L,
                           row_tol     = NULL,
                           col_gap     = NULL) {

  # ── Get word-level data ────────────────────────────────────────────────────
  page_data <- pdftools::pdf_data(path)
  if (page > length(page_data)) {
    cli::cli_abort("Page {page} exceeds document length ({length(page_data)}).")
  }
  words <- page_data[[page]]

  if (nrow(words) == 0) return(data.frame())

  # ── Clip to area (top, left, bottom, right in PDF points) ─────────────────
  if (!is.null(area)) {
    # Keep any word whose centre falls inside the area
    cx <- words$x + words$width  / 2
    cy <- words$y + words$height / 2
    words <- words[
      cx >= area[["left"]]   &
      cx <= area[["right"]]  &
      cy >= area[["top"]]    &
      cy <= area[["bottom"]], ,
      drop = FALSE
    ]
  }

  if (nrow(words) == 0) return(data.frame())

  # ── Sort by y then x ───────────────────────────────────────────────────────
  words <- words[order(words$y, words$x), ]

  # ── Row clustering ─────────────────────────────────────────────────────────
  # New row when vertical gap between consecutive words exceeds row_tol.
  # Default: 60% of the median character height, which handles slight vertical
  # jitter within a line while cleanly separating actual rows.
  if (is.null(row_tol)) {
    row_tol <- median(words$height, na.rm = TRUE) * 0.6
  }
  words$row_id <- cumsum(c(TRUE, diff(words$y) > row_tol))

  # ── Column detection ───────────────────────────────────────────────────────
  # Within each row, identify word groups separated by gaps > col_gap.
  # The left edge of each group is recorded as a "column anchor".
  # All anchors are then clustered globally to produce stable column spans.
  if (is.null(col_gap)) {
    col_gap <- median(words$width, na.rm = TRUE) * 1.2
  }

  anchors <- numeric(0)
  for (rid in unique(words$row_id)) {
    rw <- words[words$row_id == rid, ]
    rw <- rw[order(rw$x), ]
    if (nrow(rw) == 1L) {
      anchors <- c(anchors, rw$x[1L])
      next
    }
    right_edges  <- rw$x + rw$width
    inter_gaps   <- rw$x[-1L] - right_edges[-nrow(rw)]
    is_new_group <- c(TRUE, inter_gaps > col_gap)
    anchors <- c(anchors, rw$x[is_new_group])
  }

  # Cluster anchors: anchors within col_gap/2 of each other belong to the
  # same column.
  anchors_s <- sort(anchors)
  anchor_gaps <- c(col_gap + 1, diff(anchors_s))
  col_starts  <- anchors_s[anchor_gaps > col_gap / 2]
  n_cols      <- length(col_starts)

  if (n_cols == 0L) return(data.frame())

  # ── Assign each word to a column ───────────────────────────────────────────
  words$col_id <- pmax(1L, pmin(
    as.integer(findInterval(words$x, col_starts)),
    n_cols
  ))

  # ── Build character matrix ─────────────────────────────────────────────────
  n_rows <- max(words$row_id)
  mat    <- matrix("", nrow = n_rows, ncol = n_cols)

  for (i in seq_len(nrow(words))) {
    r <- words$row_id[i]
    c <- words$col_id[i]
    curr    <- mat[r, c]
    mat[r, c] <- if (nchar(curr) == 0L) words$text[i] else paste(curr, words$text[i])
  }

  # ── Convert matrix → data frame ────────────────────────────────────────────
  .bbox_mat_to_df(mat, header_rows)
}


# --------------------------------------------------------------------------- #
#  .bbox_mat_to_df() — internal                                               #
#  Turns the raw character matrix into a named data frame.                    #
#  header_rows top rows are collapsed into column names.                      #
# --------------------------------------------------------------------------- #

.bbox_mat_to_df <- function(mat, header_rows = 1L) {
  if (nrow(mat) == 0L || ncol(mat) == 0L) return(data.frame())

  hr <- min(as.integer(header_rows), nrow(mat) - 1L)
  hr <- max(hr, 0L)

  if (hr == 0L) {
    df        <- as.data.frame(mat, stringsAsFactors = FALSE)
    names(df) <- make.names(paste0("col_", seq_len(ncol(mat))), unique = TRUE)
    rownames(df) <- NULL
    return(df)
  }

  # Collapse header rows: forward-fill empty cells then paste levels with "_"
  hdr <- mat[seq_len(hr), , drop = FALSE]
  for (i in seq_len(nrow(hdr))) hdr[i, ] <- .forward_fill(hdr[i, ])

  col_names <- vapply(seq_len(ncol(hdr)), function(j) {
    parts <- trimws(hdr[, j])
    parts <- parts[nchar(parts) > 0L]
    if (length(parts) == 0L) "" else paste(parts, collapse = " ")
  }, character(1L))

  col_names <- make.names(col_names, unique = TRUE)

  data_rows <- seq(hr + 1L, nrow(mat))
  df        <- as.data.frame(mat[data_rows, , drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- col_names
  rownames(df) <- NULL
  df
}
