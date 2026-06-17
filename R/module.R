#' Shiny module UI for PDF table extraction
#'
#' Provides a split-pane interface: PDF viewer on the left, live extracted
#' table on the right. Transforms (rename, type-cast, filter) open in modal
#' dialogs so both panes remain visible.
#'
#' @param id Module namespace ID.
#' @param title Card title shown in the header (default `"PDF · Table Extractor"`).
#' @param height Height of the PDF image pane (default `"600px"`).
#' @return A `bslib` card UI element.
#' @export
macrox_ui <- function(id, title = "PDF \u00b7 Table Extractor", height = "600px") {
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE) ||
      !requireNamespace("DT",    quietly = TRUE)) {
    stop("Packages shiny, bslib, and DT are required for the Shiny module.")
  }

  ns <- shiny::NS(id)

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      shiny::icon("file-pdf"), " ", title,
      shiny::div(
        class = "ms-auto d-flex gap-2",
        shiny::fileInput(ns("load_macro_file"), NULL, accept = ".yml",
                         placeholder = "Load macro (.yml)",
                         width = "220px"),
        shiny::actionButton(ns("save_macro_btn"), "Save Macro",
                            icon = shiny::icon("save"), class = "btn-outline-primary")
      )
    ),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,

        shiny::fileInput(ns("pdf_upload"), "Upload PDF", accept = ".pdf"),
        shiny::hr(),

        shiny::textInput(ns("tbl_label"), "Table label", placeholder = "e.g. calf_monthly"),

        bslib::navset_pill(
          id = ns("select_method"),

          bslib::nav_panel("Index",
            shiny::numericInput(ns("page_idx"),        "Page",         value = 1, min = 1),
            shiny::numericInput(ns("table_index"),     "Table index",  value = 1, min = 1),
            shiny::numericInput(ns("header_rows_idx"), "Header rows",  value = 1, min = 1)
          ),

          bslib::nav_panel("Box",
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput(ns("area_top"),    "Top",    value = 0, min = 0)),
              shiny::column(6, shiny::numericInput(ns("area_left"),   "Left",   value = 0, min = 0))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput(ns("area_bottom"), "Bottom", value = 500, min = 0)),
              shiny::column(6, shiny::numericInput(ns("area_right"),  "Right",  value = 800, min = 0))
            ),
            shiny::numericInput(ns("page_box"),        "Page",         value = 1, min = 1),
            shiny::numericInput(ns("header_rows_box"), "Header rows",  value = 1, min = 1),
            # bbox tuning — only shown when bbox method is selected
            shiny::conditionalPanel(
              paste0("input['", ns("method"), "'] == 'bbox'"),
              shiny::hr(),
              shiny::tags$small(class = "text-muted fw-semibold", "bbox tuning (optional)"),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("row_tol"), "Row tol (pts)",
                                                     value = NA, min = 0, step = 1)),
                shiny::column(6, shiny::numericInput(ns("col_gap"), "Col gap (pts)",
                                                     value = NA, min = 0, step = 1))
              ),
              shiny::tags$small(class = "text-muted",
                "Leave blank to auto-detect. Increase col gap if columns merge; decrease if they split.")
            )
          ),

          bslib::nav_panel("Fuzzy",
            shiny::textInput(ns("label_match"), "Caption text",
                             placeholder = "e.g. Mart Movements by Breed"),
            shiny::numericInput(ns("max_dist"), "Max distance", value = 0.2,
                                min = 0, max = 1, step = 0.05)
          )
        ),

        shiny::selectInput(ns("method"), "Extraction method",
                           choices = c("lattice", "stream", "bbox"),
                           selected = "bbox"),

        shiny::actionButton(ns("extract_btn"), "Extract",
                            icon  = shiny::icon("table"),
                            class = "btn-primary w-100 mt-2"),

        shiny::hr(),
        shiny::h6("Transform active table"),
        shiny::uiOutput(ns("active_table_ui")),

        shiny::div(
          class = "d-flex gap-1 mt-1",
          shiny::actionButton(ns("open_rename"), "Rename",
                              class = "btn-outline-secondary btn-sm"),
          shiny::actionButton(ns("open_cast"),   "Types",
                              class = "btn-outline-secondary btn-sm"),
          shiny::actionButton(ns("open_filter"), "Filter",
                              class = "btn-outline-danger btn-sm")
        ),

        shiny::hr(),
        shiny::h6("Steps recorded"),
        shiny::uiOutput(ns("steps_badges")),

        shiny::div(
          class = "d-flex gap-1 mt-1",
          shiny::actionButton(ns("show_steps_btn"),  "Review",
                              class = "btn-outline-secondary btn-sm"),
          shiny::actionButton(ns("clear_steps_btn"), "Clear",
                              class = "btn-outline-danger btn-sm")
        )
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),

        # LEFT — PDF viewer
        bslib::card(
          bslib::card_header(
            shiny::icon("file-pdf"), " PDF Preview",
            shiny::div(
              class = "ms-auto d-flex align-items-center gap-1",
              shiny::actionButton(ns("prev_page"), "<", class = "btn-sm btn-outline-secondary"),
              shiny::numericInput(ns("viewer_page"), NULL, value = 1, min = 1, width = "70px"),
              shiny::actionButton(ns("next_page"), ">", class = "btn-sm btn-outline-secondary")
            )
          ),
          bslib::card_body(
            padding = 0,
            shiny::imageOutput(
              ns("pdf_img"),
              height = height,
              brush = shiny::brushOpts(
                id       = ns("pdf_brush"),
                fill     = "#0ea5e930",
                stroke   = "#0ea5e9",
                resetOnNew = TRUE
              )
            ),
            shiny::uiOutput(ns("brush_capture_ui"))
          )
        ),

        # RIGHT — live table
        bslib::card(
          bslib::card_header(
            shiny::icon("table"), " ",
            shiny::uiOutput(ns("table_pane_header"), inline = TRUE),
            shiny::div(
              class = "ms-auto",
              shiny::downloadButton(ns("download_csv"), "CSV", class = "btn-sm")
            )
          ),
          bslib::card_body(
            padding = 0,
            DT::DTOutput(ns("live_table_dt"))
          )
        )
      ),

      # Collapsible macro panel
      shiny::conditionalPanel(
        paste0("input['", ns("show_steps_btn"), "'] % 2 == 1"),
        bslib::card(
          bslib::card_header("Macro YAML"),
          shiny::verbatimTextOutput(ns("macro_yaml")),
          shiny::fluidRow(
            shiny::column(4, shiny::textInput(ns("macro_name"), "Macro name", value = "my_macro")),
            shiny::column(4, shiny::textInput(ns("macro_dir"),  "Directory",  value = ".")),
            shiny::column(4,
              shiny::br(),
              shiny::actionButton(ns("save_macro_confirm"), "Save to disk",
                                  class = "btn-primary")
            )
          )
        )
      )
    )
  )
}


#' Shiny module server for PDF table extraction
#'
#' @param id Module namespace ID (must match [macrox_ui()]).
#' @return A list with two reactives: `tables` (named list of data frames) and
#'   `steps` (list of step definitions).
#' @export
macrox_server <- function(id) {
  if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny required")

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      pdf_path     = NULL,
      tables       = list(),
      steps        = list(),
      page_text    = NULL,
      active_label = NULL,
      active_page  = 1L,
      active_area  = NULL,
      n_pages      = 1L,
      brush_area   = NULL
    )

    # ------------------------------------------------------------------ #
    #  PDF upload                                                           #
    # ------------------------------------------------------------------ #

    shiny::observeEvent(input$pdf_upload, {
      req(input$pdf_upload)
      rv$pdf_path   <- input$pdf_upload$datapath
      info          <- pdftools::pdf_info(rv$pdf_path)
      rv$n_pages    <- info$pages
      rv$active_page <- 1L
      shiny::updateNumericInput(session, "viewer_page", max = rv$n_pages, value = 1)
      shiny::updateNumericInput(session, "page_idx",    max = rv$n_pages)
      shiny::updateNumericInput(session, "page_box",    max = rv$n_pages)
      shiny::showNotification(paste0("Loaded: ", input$pdf_upload$name,
                                     "  (", rv$n_pages, " pages)"), type = "message")
    })

    # ------------------------------------------------------------------ #
    #  Page navigation                                                      #
    # ------------------------------------------------------------------ #

    shiny::observeEvent(input$viewer_page, {
      req(rv$pdf_path)
      pg <- as.integer(input$viewer_page)
      pg <- max(1L, min(pg, rv$n_pages))
      if (!identical(pg, rv$active_page)) {
        rv$active_page <- pg
        rv$active_area <- NULL  # clear highlight when navigating
      }
    })

    shiny::observeEvent(input$prev_page, {
      pg <- max(1L, rv$active_page - 1L)
      rv$active_page <- pg
      rv$active_area <- NULL
      shiny::updateNumericInput(session, "viewer_page", value = pg)
    })

    shiny::observeEvent(input$next_page, {
      pg <- min(rv$n_pages, rv$active_page + 1L)
      rv$active_page <- pg
      rv$active_area <- NULL
      shiny::updateNumericInput(session, "viewer_page", value = pg)
    })

    # ------------------------------------------------------------------ #
    #  Brush / area selection                                               #
    # ------------------------------------------------------------------ #

    rv_brush_area <- shiny::reactive({
      b <- input$pdf_brush
      req(b)
      pts_per_px <- 72 / 150
      c(
        top    = b$coords_img$ymin * pts_per_px,
        left   = b$coords_img$xmin * pts_per_px,
        bottom = b$coords_img$ymax * pts_per_px,
        right  = b$coords_img$xmax * pts_per_px
      )
    })

    output$brush_capture_ui <- shiny::renderUI({
      area <- tryCatch(rv_brush_area(), error = function(e) NULL)
      req(!is.null(area))
      shiny::div(
        class = "p-2 bg-light border-top",
        shiny::tags$code(sprintf(
          "top=%.0f  left=%.0f  bottom=%.0f  right=%.0f",
          area["top"], area["left"], area["bottom"], area["right"]
        )),
        shiny::actionButton(ns("use_brush"), "Use selection",
                            class = "btn-sm btn-primary ms-2"),
        shiny::actionButton(ns("clear_brush"), "Clear",
                            class = "btn-sm btn-outline-secondary ms-1")
      )
    })

    shiny::observeEvent(input$use_brush, {
      area <- tryCatch(rv_brush_area(), error = function(e) NULL)
      req(!is.null(area))
      shiny::updateNumericInput(session, "area_top",    value = unname(round(area["top"])))
      shiny::updateNumericInput(session, "area_left",   value = unname(round(area["left"])))
      shiny::updateNumericInput(session, "area_bottom", value = unname(round(area["bottom"])))
      shiny::updateNumericInput(session, "area_right",  value = unname(round(area["right"])))
      shiny::updateNumericInput(session, "page_box",    value = unname(rv$active_page))
      bslib::nav_select(ns("select_method"), "Box", session = session)
      shiny::showNotification("Box tab pre-filled. Add a label and click Extract.", type = "message")
    })

    # ------------------------------------------------------------------ #
    #  PDF rendering                                                        #
    # ------------------------------------------------------------------ #

    output$pdf_img <- shiny::renderImage({
      req(rv$pdf_path)
      page_raw <- tryCatch(
        pdftools::pdf_render_page(rv$pdf_path, page = rv$active_page,
                                   dpi = 150, numeric = FALSE),
        error = function(e) NULL
      )
      req(!is.null(page_raw))

      img <- .annotate_page(page_raw, rv$active_area, rv$pdf_path, rv$active_page, 150)

      tmp <- tempfile(fileext = ".png")
      if (requireNamespace("magick", quietly = TRUE) && inherits(img, "magick-image")) {
        magick::image_write(img, path = tmp, format = "png")
      } else {
        writeBin(as.raw(img), tmp)
      }

      list(src = tmp, contentType = "image/png", width = "100%", deleteFile = TRUE)
    }, deleteFile = TRUE)

    # ------------------------------------------------------------------ #
    #  Extract button                                                       #
    # ------------------------------------------------------------------ #

    shiny::observeEvent(input$extract_btn, {
      req(rv$pdf_path, nchar(trimws(input$tbl_label)) > 0)
      label  <- trimws(input$tbl_label)
      method <- input$method
      tab    <- input$select_method

      result <- tryCatch({
        if (tab == "Index") {
          page        <- as.integer(input$page_idx)
          table_index <- as.integer(input$table_index)
          header_rows <- as.integer(input$header_rows_idx)
          area        <- NULL
          label_match <- NULL
          if (method == "bbox") {
            shiny::showNotification(
              "bbox with no area will scan the full page — charts may interfere. Draw a box on the PDF and use the Box tab for best results.",
              type     = "warning",
              duration = 8
            )
          }
        } else if (tab == "Box") {
          page        <- as.integer(input$page_box)
          table_index <- 1L
          header_rows <- as.integer(input$header_rows_box)
          area        <- c(top    = input$area_top,
                           left   = input$area_left,
                           bottom = input$area_bottom,
                           right  = input$area_right)
          label_match <- NULL
        } else {
          # Fuzzy
          lm <- trimws(input$label_match)
          req(nchar(lm) > 0)
          if (is.null(rv$page_text)) {
            rv$page_text <- pdftools::pdf_text(rv$pdf_path)
          }
          found <- .fuzzy_find_page(
            list(text = rv$page_text),
            lm, "jw", input$max_dist
          )
          page        <- found$page
          table_index <- 1L
          header_rows <- 1L
          area        <- NULL
          label_match <- lm
        }

        if (method == "bbox") {
          row_tol_val <- if (is.na(input$row_tol) || input$row_tol == 0) NULL else input$row_tol
          col_gap_val <- if (is.na(input$col_gap) || input$col_gap == 0) NULL else input$col_gap
          df <- .extract_bbox(rv$pdf_path, page,
                              area        = area,
                              header_rows = header_rows,
                              row_tol     = row_tol_val,
                              col_gap     = col_gap_val)
          if (nrow(df) == 0) stop("bbox returned no data. Try setting an area.")
        } else {
          extract_args <- list(
            file   = rv$pdf_path,
            pages  = page,
            method = method,
            output = "matrix",
            guess  = is.null(area)
          )
          if (!is.null(area)) extract_args$area <- list(area)

          raw_list <- do.call(tabulapdf::extract_tables, extract_args)
          if (length(raw_list) == 0) stop("No tables found on page ", page)
          if (table_index > length(raw_list)) {
            stop("table_index ", table_index, " but only ", length(raw_list),
                 " table(s) on page ", page)
          }

          df <- .matrix_to_df(raw_list[[table_index]])
          if (header_rows > 1) df <- .flatten_headers(df, header_rows)
        }

        new_step <- list(
          step        = "select_table",
          label       = label,
          page        = page,
          table_index = table_index,
          area        = area,
          label_match = label_match,
          method      = method,
          header_rows = header_rows
        )

        rv$tables[[label]] <- df
        rv$steps           <- .module_record(rv$steps, new_step, session)
        rv$active_label    <- label
        rv$active_page     <- page
        rv$active_area     <- area

        shiny::updateNumericInput(session, "viewer_page", value = page)
        "ok"
      }, error = function(e) e$message)

      if (!identical(result, "ok")) {
        shiny::showNotification(paste("Extraction failed:", result), type = "error")
      }
    })

    # ------------------------------------------------------------------ #
    #  Active table selector UI                                             #
    # ------------------------------------------------------------------ #

    output$active_table_ui <- shiny::renderUI({
      lbls <- names(rv$tables)
      if (length(lbls) == 0) return(shiny::p("No tables yet.", class = "text-muted small"))
      shiny::selectInput(ns("active_label_sel"), "Active table",
                         choices = lbls, selected = rv$active_label)
    })

    shiny::observeEvent(input$active_label_sel, {
      rv$active_label <- input$active_label_sel
    })

    # ------------------------------------------------------------------ #
    #  Live table display                                                   #
    # ------------------------------------------------------------------ #

    output$table_pane_header <- shiny::renderUI({
      lbl <- rv$active_label
      req(lbl, lbl %in% names(rv$tables))
      df <- rv$tables[[lbl]]
      shiny::tags$span(shiny::tags$b(lbl),
                       shiny::tags$small(paste0("  [", nrow(df), " \u00d7 ", ncol(df), "]")))
    })

    output$live_table_dt <- DT::renderDT({
      lbl <- rv$active_label
      req(lbl, lbl %in% names(rv$tables))
      DT::datatable(rv$tables[[lbl]],
                    options = list(scrollX = TRUE, pageLength = 10),
                    rownames = FALSE)
    })

    output$download_csv <- shiny::downloadHandler(
      filename = function() paste0(rv$active_label %||% "table", ".csv"),
      content  = function(file) {
        lbl <- rv$active_label
        req(lbl, lbl %in% names(rv$tables))
        utils::write.csv(rv$tables[[lbl]], file, row.names = FALSE)
      }
    )

    # ------------------------------------------------------------------ #
    #  Steps badges                                                         #
    # ------------------------------------------------------------------ #

    output$steps_badges <- shiny::renderUI({
      n <- length(rv$steps)
      if (n == 0) return(shiny::p("No steps recorded.", class = "text-muted small"))
      badges <- lapply(seq_len(n), function(i) {
        s    <- rv$steps[[i]]
        flag <- isTRUE(s$.flagged)
        cls  <- if (flag) "badge bg-warning text-dark me-1" else "badge bg-secondary me-1"
        shiny::tags$span(class = cls,
                         paste0("[", i, "] ", s$step, " / ", s$label %||% s$table %||% "?"))
      })
      shiny::div(badges)
    })

    shiny::observeEvent(input$clear_steps_btn, {
      rv$steps <- list()
      shiny::showNotification("Steps cleared.", type = "message")
    })

    # ------------------------------------------------------------------ #
    #  Rename modal                                                         #
    # ------------------------------------------------------------------ #

    shiny::observeEvent(input$open_rename, {
      lbl <- rv$active_label
      req(lbl, lbl %in% names(rv$tables))
      df   <- rv$tables[[lbl]]
      cols <- names(df)
      rows <- lapply(seq_along(cols), function(i) {
        shiny::fluidRow(
          shiny::column(4, shiny::tags$code(cols[[i]])),
          shiny::column(8, shiny::textInput(ns(paste0("ren_", i)), NULL,
                                            value = cols[[i]], placeholder = "new name"))
        )
      })
      shiny::showModal(shiny::modalDialog(
        title = paste0("Rename columns: ", lbl),
        shiny::div(rows),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("apply_rename"), "Apply", class = "btn-primary")
        ),
        size = "l"
      ))
    })

    shiny::observeEvent(input$apply_rename, {
      lbl <- rv$active_label
      req(lbl, lbl %in% names(rv$tables))
      df   <- rv$tables[[lbl]]
      cols <- names(df)
      mapping <- vapply(seq_along(cols), function(i) {
        val <- input[[paste0("ren_", i)]]
        if (is.null(val) || nchar(trimws(val)) == 0) cols[[i]] else trimws(val)
      }, character(1))
      names(mapping) <- cols
      changed <- mapping[mapping != cols]
      if (length(changed) > 0) {
        new_step <- list(step = "rename_columns", table = lbl,
                         mapping = as.list(changed))
        idx <- match(names(changed), names(df))
        names(df)[idx] <- unname(changed)
        rv$tables[[lbl]] <- df
        rv$steps <- .module_record(rv$steps, new_step, session)
      }
      shiny::removeModal()
    })

    # ------------------------------------------------------------------ #
    #  Cast types modal                                                     #
    # ------------------------------------------------------------------ #

    shiny::observeEvent(input$open_cast, {
      lbl <- rv$active_label
      req(lbl, lbl %in% names(rv$tables))
      df   <- rv$tables[[lbl]]
      cols <- names(df)
      type_choices <- c("character", "numeric", "integer",
                        "date:%d/%m/%Y", "date:%Y-%m-%d", "date:%m/%d/%Y")
      rows <- lapply(seq_along(cols), function(i) {
        guessed <- .guess_type(df[[cols[[i]]]])
        shiny::fluidRow(
          shiny::column(4, shiny::tags$code(cols[[i]])),
          shiny::column(8, shiny::selectInput(ns(paste0("cast_", i)), NULL,
                                               choices = type_choices,
                                               selected = guessed))
        )
      })
      shiny::showModal(shiny::modalDialog(
        title = paste0("Cast types: ", lbl),
        shiny::div(rows),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("apply_cast"), "Apply", class = "btn-primary")
        ),
        size = "l"
      ))
    })

    shiny::observeEvent(input$apply_cast, {
      lbl <- rv$active_label
      req(lbl, lbl %in% names(rv$tables))
      df   <- rv$tables[[lbl]]
      cols <- names(df)
      types <- vapply(seq_along(cols), function(i) {
        input[[paste0("cast_", i)]] %||% "character"
      }, character(1))
      names(types) <- cols

      for (col in cols) {
        df[[col]] <- tryCatch(.cast_col(df[[col]], types[[col]]),
                              error = function(e) df[[col]])
      }
      new_step <- list(step = "cast_types", table = lbl, types = as.list(types))
      rv$tables[[lbl]] <- df
      rv$steps <- .module_record(rv$steps, new_step, session)
      shiny::removeModal()
    })

    # ------------------------------------------------------------------ #
    #  Filter modal                                                         #
    # ------------------------------------------------------------------ #

    shiny::observeEvent(input$open_filter, {
      lbl <- rv$active_label
      req(lbl, lbl %in% names(rv$tables))
      df <- rv$tables[[lbl]]
      shiny::showModal(shiny::modalDialog(
        title = paste0("Filter rows: ", lbl),
        shiny::p("Rows where the expression is TRUE will be removed."),
        shiny::textInput(ns("filter_expr"), "Exclude where:",
                         placeholder = "month == 'Total'"),
        shiny::tags$small("Examples: ",
                          shiny::tags$code("total == 0"),
                          " | ",
                          shiny::tags$code("is.na(value)"),
                          " | ",
                          shiny::tags$code("grepl('Total', month)")),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("apply_filter"), "Apply", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$apply_filter, {
      lbl  <- rv$active_label
      expr <- trimws(input$filter_expr)
      req(lbl, lbl %in% names(rv$tables), nchar(expr) > 0)
      df   <- rv$tables[[lbl]]
      mask <- tryCatch(
        eval(parse(text = expr), envir = df),
        error = function(e) {
          shiny::showNotification(paste("Filter error:", e$message), type = "error")
          return(NULL)
        }
      )
      req(!is.null(mask))
      df2 <- df[!mask, , drop = FALSE]; rownames(df2) <- NULL
      new_step <- list(step = "filter_rows", table = lbl, exclude_where = expr)
      rv$tables[[lbl]] <- df2
      rv$steps <- .module_record(rv$steps, new_step, session)
      shiny::removeModal()
    })

    # ------------------------------------------------------------------ #
    #  Macro save / load                                                    #
    # ------------------------------------------------------------------ #

    output$macro_yaml <- shiny::renderText({
      req(length(rv$steps) > 0)
      clean_steps <- lapply(rv$steps, function(s) s[!grepl("^\\.", names(s))])
      yaml::as.yaml(list(steps = clean_steps))
    })

    shiny::observeEvent(input$save_macro_confirm, {
      req(length(rv$steps) > 0)
      name <- trimws(input$macro_name)
      dir  <- trimws(input$macro_dir)
      req(nchar(name) > 0)
      tryCatch({
        clean_steps <- lapply(rv$steps, function(s) s[!grepl("^\\.", names(s))])
        macro <- list(
          macro = list(name = name, created = format(Sys.time(), "%Y-%m-%d %H:%M"),
                       source = basename(rv$pdf_path %||% "unknown"),
                       n_steps = length(clean_steps)),
          steps = clean_steps
        )
        out <- file.path(dir, paste0(name, ".yml"))
        yaml::write_yaml(macro, out)
        shiny::showNotification(paste0("Saved: ", out), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Save failed:", e$message), type = "error")
      })
    })

    shiny::observeEvent(input$load_macro_file, {
      req(input$load_macro_file)
      tryCatch({
        m      <- yaml::read_yaml(input$load_macro_file$datapath)
        steps  <- m$steps
        rv$steps <- steps
        shiny::showNotification(
          paste0("Macro loaded: ", m$macro$name, " (", length(steps), " steps)"),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(paste("Load failed:", e$message), type = "error")
      })
    })

    # ------------------------------------------------------------------ #
    #  Return value                                                         #
    # ------------------------------------------------------------------ #

    list(
      tables = shiny::reactive(rv$tables),
      steps  = shiny::reactive(rv$steps)
    )
  })
}


# --------------------------------------------------------------------------- #
#  Module-internal helpers                                                     #
# --------------------------------------------------------------------------- #

.module_record <- function(steps, new_step, session = NULL) {
  new_clean <- new_step[!grepl("^\\.", names(new_step))]

  for (i in seq_along(steps)) {
    ex       <- steps[[i]]
    ex_clean <- ex[!grepl("^\\.", names(ex))]

    if (identical(ex_clean, new_clean)) {
      if (!is.null(session)) {
        shiny::showNotification(
          paste0("Duplicate step skipped: '", new_step$step, "' on '",
                 new_step$table %||% new_step$label %||% "?", "'"),
          type = "warning"
        )
      }
      return(steps)
    }

    if (!is.null(new_step$step) && new_step$step == "select_table" &&
        !is.null(ex$step)       && ex$step       == "select_table" &&
        !is.null(new_step$label) && identical(new_step$label, ex$label)) {
      new_step$.flagged <- TRUE
      new_step$.flag    <- paste0("Overwrites extraction at step [", i, "]")
      if (!is.null(session)) {
        shiny::showNotification(
          paste0("Table '", new_step$label, "' already extracted. Re-extraction flagged."),
          type = "warning"
        )
      }
    }

    transform_steps <- c("rename_columns", "cast_types", "filter_rows")
    if (!is.null(new_step$step) && new_step$step %in% transform_steps &&
        !is.null(ex$step)       && ex$step       == new_step$step &&
        !is.null(new_step$table) && identical(new_step$table, ex$table)) {
      new_step$.flagged <- TRUE
      new_step$.flag    <- paste0("Repeats step [", i, "]")
      if (!is.null(session)) {
        shiny::showNotification(
          paste0("'", new_step$step, "' already applied to '", new_step$table, "'. Flagged."),
          type = "warning"
        )
      }
    }
  }

  c(steps, list(new_step))
}

.guess_type <- function(x) {
  x_char <- as.character(x)
  x_clean <- gsub("[,\\s ]", "", x_char)
  if (all(is.na(suppressWarnings(as.numeric(x_clean[!is.na(x_clean)]))))) {
    return("character")
  }
  if (all(suppressWarnings(as.numeric(x_clean)) ==
          suppressWarnings(as.integer(x_clean)), na.rm = TRUE)) {
    return("integer")
  }
  "numeric"
}
