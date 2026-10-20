# --------------------------------------------------------------------------- #
#  blockr blocks (experimental)                                               #
#                                                                             #
#  Optional front end: macrox steps as blocks for the blockr framework. The   #
#  blocks only call exported, session-free functions (pdf_table(),            #
#  split_column_df(), clean_numbers_df()), so the code blockr exports runs    #
#  with macrox alone. blockr.core is a suggested package; nothing here runs   #
#  unless it is installed.                                                    #
# --------------------------------------------------------------------------- #

.require_blockr <- function(what) {
  if (!requireNamespace("blockr.core", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.fn {what}} needs the {.pkg blockr.core} package, which is not installed.",
      "i" = "Install with {.code install.packages('blockr.core')}."
    ), call = NULL)
  }
  invisible(TRUE)
}

#' PDF table block
#'
#' A [blockr](https://bristolmyerssquibb.github.io/blockr.core/) data block
#' that reads one table from a PDF page with [pdf_table()]. Its output feeds the
#' other blocks on a board.
#'
#' These blocks are experimental. They follow the `blockr.core` block API, which
#' is still changing, and require the suggested `blockr.core` package.
#'
#' @param path Path to a PDF file.
#' @param page Page number.
#' @param header_rows Number of header rows.
#' @param ... Forwarded to `blockr.core::new_data_block()`.
#' @return A `pdf_table_block` object.
#' @examples
#' \dontrun{
#' blockr.core::serve(new_pdf_table_block("report.pdf", page = 9, header_rows = 4))
#' }
#' @export
new_pdf_table_block <- function(path = "", page = 1L, header_rows = 1L, ...) {
  .require_blockr("new_pdf_table_block")

  blockr.core::new_data_block(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        path_r <- shiny::reactiveVal(path)
        page_r <- shiny::reactiveVal(page)
        hdr_r  <- shiny::reactiveVal(header_rows)

        shiny::observeEvent(input$path, path_r(input$path))
        shiny::observeEvent(input$page, page_r(input$page))
        shiny::observeEvent(input$header_rows, hdr_r(input$header_rows))

        list(
          expr = shiny::reactive({
            shiny::req(nzchar(path_r()), !is.na(page_r()), !is.na(hdr_r()))
            bquote(
              macrox::pdf_table(.(p), page = .(pg), header_rows = .(hr)),
              list(p = path_r(), pg = as.integer(page_r()),
                   hr = as.integer(hdr_r()))
            )
          }),
          state = list(path = path_r, page = page_r, header_rows = hdr_r)
        )
      })
    },
    function(id) {
      shiny::tagList(
        shiny::textInput(shiny::NS(id, "path"), "PDF file", value = path),
        shiny::numericInput(shiny::NS(id, "page"), "Page", value = page, min = 1L),
        shiny::numericInput(shiny::NS(id, "header_rows"), "Header rows",
                            value = header_rows, min = 1L)
      )
    },
    class = "pdf_table_block",
    ...
  )
}

#' Split column block
#'
#' A blockr transform block that splits one column into several on a regular
#' expression, with [split_column_df()]. Typical use: a PDF table where three
#' numbers ran together in one cell.
#'
#' @param col Name of the column to split.
#' @param into New column names, comma-separated (at least two).
#' @param sep Regular expression to split on. The default splits on whitespace.
#' @param keep Keep the original column alongside the new ones.
#' @param ... Forwarded to `blockr.core::new_transform_block()`.
#' @return A `split_column_block` object.
#' @inherit new_pdf_table_block details
#' @export
new_split_column_block <- function(col = character(), into = "a, b",
                                   sep = "\\s+", keep = FALSE, ...) {
  .require_blockr("new_split_column_block")

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        col_r  <- shiny::reactiveVal(col)
        into_r <- shiny::reactiveVal(into)
        sep_r  <- shiny::reactiveVal(sep)
        keep_r <- shiny::reactiveVal(keep)

        shiny::observeEvent(input$col, col_r(input$col))
        shiny::observeEvent(input$into, into_r(input$into))
        shiny::observeEvent(input$sep, sep_r(input$sep))
        shiny::observeEvent(input$keep, keep_r(input$keep))

        shiny::observeEvent(
          names(data()),
          shiny::updateSelectInput(session, "col", choices = names(data()),
                                   selected = col_r())
        )

        list(
          expr = shiny::reactive({
            nm <- trimws(strsplit(into_r(), ",")[[1L]])
            nm <- nm[nzchar(nm)]
            shiny::req(length(col_r()) == 1L, nzchar(col_r()), length(nm) >= 2L)
            blockr.core::bbquote(
              macrox::split_column_df(.(data), col = .(col), into = .(into),
                                      sep = .(sep), keep = .(keep)),
              list(col = col_r(), into = nm, sep = sep_r(),
                   keep = isTRUE(keep_r()))
            )
          }),
          state = list(col = col_r, into = into_r, sep = sep_r, keep = keep_r)
        )
      })
    },
    function(id) {
      shiny::tagList(
        shiny::selectInput(shiny::NS(id, "col"), "Column to split",
                           choices = col, selected = col),
        shiny::textInput(shiny::NS(id, "into"),
                         "New column names (comma-separated)", value = into),
        shiny::textInput(shiny::NS(id, "sep"), "Separator (regex)", value = sep),
        shiny::checkboxInput(shiny::NS(id, "keep"), "Keep original column",
                             value = keep)
      )
    },
    dat_valid = function(data) stopifnot(is.data.frame(data)),
    expr_type = "bquoted",
    class = "split_column_block",
    ...
  )
}

#' Clean numbers block
#'
#' A blockr transform block that strips thousands separators, currency symbols
#' and parenthetical negatives from text columns, maps sentinel strings such as
#' `"n/a"` to `NA`, and converts columns that then parse cleanly to numeric,
#' with [clean_numbers_df()].
#'
#' @param cols Columns to clean. Empty (the default) cleans every non-numeric
#'   column.
#' @param convert Convert fully parseable columns to numeric.
#' @param ... Forwarded to `blockr.core::new_transform_block()`.
#' @return A `clean_numbers_block` object.
#' @inherit new_pdf_table_block details
#' @export
new_clean_numbers_block <- function(cols = character(), convert = TRUE, ...) {
  .require_blockr("new_clean_numbers_block")

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        cols_r <- shiny::reactiveVal(cols)
        conv_r <- shiny::reactiveVal(convert)

        shiny::observeEvent(input$cols, cols_r(input$cols), ignoreNULL = FALSE)
        shiny::observeEvent(input$convert, conv_r(input$convert))

        shiny::observeEvent(
          names(data()),
          shiny::updateSelectInput(session, "cols", choices = names(data()),
                                   selected = cols_r())
        )

        list(
          expr = shiny::reactive({
            cols <- cols_r()
            if (length(cols) > 0L) {
              blockr.core::bbquote(
                macrox::clean_numbers_df(.(data), cols = .(cols),
                                         convert = .(convert)),
                list(cols = cols, convert = isTRUE(conv_r()))
              )
            } else {
              blockr.core::bbquote(
                macrox::clean_numbers_df(.(data), convert = .(convert)),
                list(convert = isTRUE(conv_r()))
              )
            }
          }),
          state = list(cols = cols_r, convert = conv_r)
        )
      })
    },
    function(id) {
      shiny::tagList(
        shiny::selectInput(shiny::NS(id, "cols"),
                           "Columns (empty = all text columns)",
                           choices = cols, selected = cols, multiple = TRUE),
        shiny::checkboxInput(shiny::NS(id, "convert"),
                             "Convert to numeric when possible", value = convert)
      )
    },
    dat_valid = function(data) stopifnot(is.data.frame(data)),
    expr_type = "bquoted",
    class = "clean_numbers_block",
    allow_empty_state = c("cols"),
    ...
  )
}

#' Register the macrox blocks with blockr
#'
#' Adds the PDF table, split column and clean numbers blocks to blockr's block
#' registry so they appear in its block picker. [mx_blockr_app()] calls this for
#' you.
#'
#' @return Called for its side effect; returns `NULL` invisibly.
#' @export
register_macrox_blocks <- function() {
  .require_blockr("register_macrox_blocks")

  blockr.core::register_blocks(
    "new_pdf_table_block",
    name        = "PDF table",
    description = "Read a table from a PDF page",
    category    = "input",
    icon        = "file-pdf",
    package     = "macrox",
    overwrite   = TRUE
  )
  blockr.core::register_blocks(
    "new_split_column_block",
    name        = "Split column",
    description = "Split one column into several on a separator",
    category    = "transform",
    icon        = "columns-gap",
    package     = "macrox",
    overwrite   = TRUE
  )
  blockr.core::register_blocks(
    "new_clean_numbers_block",
    name        = "Clean numbers",
    description = "Strip separators and currency, map n/a to NA, convert to numeric",
    category    = "transform",
    icon        = "eraser",
    package     = "macrox",
    overwrite   = TRUE
  )
  invisible(NULL)
}

#' Launch a blockr board for PDF tables
#'
#' Starts a small [blockr](https://bristolmyerssquibb.github.io/blockr.core/)
#' app wired as PDF table, then split column, then clean numbers. This is an
#' experimental alternative to [mx_app()].
#'
#' @inheritParams new_pdf_table_block
#' @return Called for its side effect (it runs an app).
#' @examples
#' \dontrun{
#' mx_blockr_app("report.pdf", page = 9, header_rows = 4)
#' }
#' @export
mx_blockr_app <- function(path = "", page = 1L, header_rows = 1L) {
  .require_blockr("mx_blockr_app")
  register_macrox_blocks()

  board <- blockr.core::new_board(
    blocks = c(
      pdf   = new_pdf_table_block(path, page, header_rows),
      split = new_split_column_block(),
      clean = new_clean_numbers_block()
    ),
    links = list(
      from  = c("pdf", "split"),
      to    = c("split", "clean"),
      input = c("data", "data")
    )
  )
  blockr.core::serve(board)
}
