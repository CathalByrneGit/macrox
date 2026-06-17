# --------------------------------------------------------------------------- #
#  pdf_app() — launch the standalone pdfmacro Shiny application               #
# --------------------------------------------------------------------------- #

#' Launch the pdfmacro standalone app
#'
#' Opens a full Shiny application for interactive PDF table extraction,
#' cleaning, and export. The app is self-contained — no existing session
#' or macro is required.
#'
#' @param viewer Where to display the app:
#'   * `"browser"` (default) — system web browser
#'   * `"dialog"` — floating RStudio dialog (1 200 × 900)
#'   * `"pane"` — RStudio Viewer pane
#' @return Called for its side-effect. Returns invisibly when the app is closed.
#' @export
pdf_app <- function(viewer = c("browser", "dialog", "pane")) {
  viewer <- match.arg(viewer)

  if (!requireNamespace("shiny",   quietly = TRUE)) stop("shiny required")
  if (!requireNamespace("bslib",   quietly = TRUE)) stop("bslib required")
  if (!requireNamespace("DT",      quietly = TRUE)) stop("DT required")

  ui     <- .pdf_app_ui()
  server <- .pdf_app_server

  viewer_func <- switch(viewer,
    dialog  = shiny::dialogViewer("pdfmacro", width = 1400, height = 940),
    browser = shiny::browserViewer(),
    pane    = shiny::paneViewer()
  )

  shiny::runGadget(
    shiny::shinyApp(ui, server),
    viewer      = viewer_func,
    stopOnCancel = TRUE
  )
}


# --------------------------------------------------------------------------- #
#  Theme helper (used by server to swap light / dark at runtime)               #
# --------------------------------------------------------------------------- #

.make_theme <- function(dark = FALSE) {
  css_path <- system.file("app/www/pdfmacro.css", package = "pdfmacro")
  rules    <- paste(readLines(css_path), collapse = "\n")
  if (dark) {
    bslib::bs_theme(version = 5, bg = "#1c1208", fg = "#f0e8d8", primary = "#f0a830")
  } else {
    bslib::bs_theme(version = 5, bg = "#faf6f0", fg = "#1a1208", primary = "#c47c00")
  } |> bslib::bs_add_rules(rules)
}

# --------------------------------------------------------------------------- #
#  UI                                                                          #
# --------------------------------------------------------------------------- #

.pdf_app_ui <- function() {
  css_path <- system.file("app/www/pdfmacro.css", package = "pdfmacro")

  theme <- bslib::bs_theme(
    version = 5,
    bg      = "#faf6f0",
    fg      = "#1a1208",
    primary = "#c47c00"
  ) |>
    bslib::bs_add_rules(paste(readLines(css_path), collapse = "\n"))

  # ── Sidebar ────────────────────────────────────────────────────────────────
  sidebar <- bslib::sidebar(
    width = 270,
    title = shiny::div(
      style = "font-size:9px; letter-spacing:0.18em; text-transform:uppercase; color:var(--amber);",
      shiny::icon("file-pdf"), "\u00a0pdfmacro"
    ),

    shiny::div(class = "section-label", "PDF File"),
    shinyFiles::shinyFilesButton(
      "pdf_select",
      label    = "Choose PDF…",
      title    = "Select a PDF file",
      multiple = FALSE,
      icon     = shiny::icon("folder-open"),
      class    = "btn-outline-secondary w-100",
      style    = "text-align:left;"
    ),
    shiny::uiOutput("file_status"),

    shiny::div(class = "section-label", "Extraction"),
    shiny::actionButton("open_extract", "Extract Tables",
                        icon  = shiny::icon("table"),
                        class = "btn-warning w-100"),
    shiny::tags$small(class = "text-muted",
      "Opens the extraction panel. Draw a box on the PDF, set a label, click Extract."),

    shiny::hr(),

    shiny::div(class = "section-label", "Macro"),
    shiny::fileInput("load_macro_file", NULL, accept = ".yml",
                     placeholder = "Load macro (.yml)", width = "100%"),
    shiny::uiOutput("macro_status"),

    shiny::hr(),

    shiny::div(class = "section-label", "Page Preview"),
    shiny::div(
      class = "d-flex align-items-center gap-1 mb-1",
      shiny::actionButton("prev_pg", "<", class = "btn-sm btn-outline-secondary"),
      shiny::numericInput("viewer_page", NULL, value = 1, min = 1, width = "65px"),
      shiny::actionButton("next_pg", ">",  class = "btn-sm btn-outline-secondary"),
      shiny::tags$small(class = "text-muted ms-1", shiny::uiOutput("page_counter", inline = TRUE))
    ),
    shiny::div(class = "pdf-viewer-wrap", shiny::imageOutput("pdf_thumb", height = "260px")),
    shiny::actionButton("open_browse", "Browse all pages",
                        icon  = shiny::icon("magnifying-glass"),
                        class = "btn-outline-secondary btn-sm w-100 mt-1"),

    shiny::div(
      class = "sidebar-foot",
      shiny::span("pdfmacro \u00b7 pdf_data() + tabulapdf"),
      shiny::actionButton("theme_toggle", "\u263D dark",
                          style = "background:none;border:1px solid var(--border);font-size:9px;padding:1px 6px;color:var(--ink-mid);")
    )
  )

  # ── Main tabs ─────────────────────────────────────────────────────────────
  main <- bslib::navset_card_underline(
    id = "main_tabs",

    # ── Extract ──
    bslib::nav_panel(
      title = "Extract",
      icon  = shiny::icon("table"),
      shiny::uiOutput("extract_panel")
    ),

    # ── Tables ──
    bslib::nav_panel(
      title = "Tables",
      icon  = shiny::icon("grip"),
      shiny::uiOutput("tables_panel")
    ),

    # ── Items ──
    bslib::nav_panel(
      title = "Items",
      icon  = shiny::icon("tag"),
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header(shiny::icon("tag"), " Extracted items"),
          bslib::card_body(shiny::uiOutput("items_list_ui")),
          bslib::card_footer(
            shiny::div(
              class = "d-flex gap-2",
              shiny::actionButton("open_extract_item", "Extract item",
                                  icon  = shiny::icon("plus"),
                                  class = "btn-warning btn-sm"),
              shiny::actionButton("open_batch_items", "Batch (GLiNER)",
                                  icon  = shiny::icon("layer-group"),
                                  class = "btn-outline-warning btn-sm")
            )
          )
        ),
        bslib::card(
          bslib::card_header(shiny::icon("code"), " JSON preview"),
          bslib::card_body(
            shiny::verbatimTextOutput("items_json_preview")
          ),
          bslib::card_footer(
            shiny::downloadButton("dl_json", "Download JSON",
                                  class = "btn-warning btn-sm",
                                  icon  = shiny::icon("code"))
          )
        )
      )
    ),

    # ── Steps ──
    bslib::nav_panel(
      title = "Steps",
      icon  = shiny::icon("list-check"),
      bslib::card(
        bslib::card_header(shiny::icon("list-check"), " Recorded steps"),
        shiny::uiOutput("steps_panel"),
        bslib::card_footer(
          shiny::div(
            class = "d-flex gap-2",
            shiny::uiOutput("remove_step_ui"),
            shiny::actionButton("clear_all_steps", "Clear all",
                                class = "btn-outline-danger btn-sm",
                                icon  = shiny::icon("trash"))
          )
        )
      )
    ),

    # ── Replay ──
    bslib::nav_panel(
      title = "Replay",
      icon  = shiny::icon("rotate"),
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header(shiny::icon("rotate"), " Replay a macro"),
          bslib::card_body(
            shiny::p(class = "text-muted small",
              "Select a macro file and a PDF to replay it against. ",
              "All extracted tables will appear in the Tables tab."),
            shinyFiles::shinyFilesButton(
              "replay_macro_file", label = "Choose macro (.yml)…",
              title = "Select a YAML macro",
              multiple = FALSE, filetype = list(YAML = c("yml", "yaml")),
              icon = shiny::icon("code"),
              class = "btn-outline-secondary w-100 mb-2"
            ),
            shiny::uiOutput("replay_macro_status"),
            shiny::hr(),
            shinyFiles::shinyFilesButton(
              "replay_pdf_file", label = "Choose PDF…",
              title = "Select a PDF to replay against",
              multiple = FALSE, filetype = list(PDF = "pdf"),
              icon = shiny::icon("file-pdf"),
              class = "btn-outline-secondary w-100 mb-2"
            ),
            shiny::uiOutput("replay_pdf_status")
          ),
          bslib::card_footer(
            shiny::actionButton("do_replay", "Run Replay",
                                icon  = shiny::icon("play"),
                                class = "btn-warning w-100")
          )
        ),
        bslib::card(
          bslib::card_header(shiny::icon("list"), " Replay log"),
          bslib::card_body(
            shiny::verbatimTextOutput("replay_log")
          )
        )
      )
    ),

    # ── Batch ──
    bslib::nav_panel(
      title = "Batch",
      icon  = shiny::icon("layer-group"),
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header(shiny::icon("layer-group"), " Batch replay"),
          bslib::card_body(
            shiny::p(class = "text-muted small",
              "Replay one macro against multiple PDFs. Results download as a zip of CSVs or an Excel workbook."),
            shinyFiles::shinyFilesButton(
              "batch_macro_file", label = "Choose macro (.yml)…",
              title = "Select a YAML macro", multiple = FALSE,
              filetype = list(YAML = c("yml", "yaml")),
              icon = shiny::icon("code"),
              class = "btn-outline-secondary w-100 mb-2"
            ),
            shiny::uiOutput("batch_macro_status"),
            shiny::hr(),
            shinyFiles::shinyFilesButton(
              "batch_pdf_files", label = "Choose PDFs…",
              title = "Select PDF files", multiple = TRUE,
              filetype = list(PDF = "pdf"),
              icon = shiny::icon("folder-open"),
              class = "btn-outline-secondary w-100 mb-2"
            ),
            shiny::uiOutput("batch_pdf_status")
          ),
          bslib::card_footer(
            shiny::div(
              class = "d-flex gap-2",
              shiny::actionButton("do_batch", "Run Batch",
                                  icon  = shiny::icon("play"),
                                  class = "btn-warning"),
              shiny::downloadButton("dl_batch_xlsx", "Excel",
                                    class = "btn-outline-success btn-sm"),
              shiny::downloadButton("dl_batch_zip",  "ZIP of CSVs",
                                    class = "btn-outline-success btn-sm")
            )
          )
        ),
        bslib::card(
          bslib::card_header(shiny::icon("table-list"), " Batch results"),
          bslib::card_body(
            shiny::uiOutput("batch_results_ui")
          )
        )
      )
    ),

    # ── Export ──
    bslib::nav_panel(
      title = "Export",
      icon  = shiny::icon("download"),
      bslib::layout_column_wrap(
        width = 1/2,

        bslib::card(
          bslib::card_header(shiny::icon("file-csv"), " CSV / Excel"),
          shiny::p("Download each extracted table as a separate file, or all tables in one Excel workbook."),
          shiny::uiOutput("export_table_sel"),
          bslib::card_footer(
            shiny::div(
              class = "export-grid",
              shiny::downloadButton("dl_csv",   "Download CSV",
                                    class = "btn-success btn-sm",
                                    icon  = shiny::icon("file-csv")),
              shiny::downloadButton("dl_xlsx",  "Download Excel",
                                    class = "btn-success btn-sm",
                                    icon  = shiny::icon("file-excel")),
              shiny::downloadButton("dl_all_xlsx", "All tables (.xlsx)",
                                    class = "btn-outline-success btn-sm",
                                    icon  = shiny::icon("layer-group")),
              shiny::downloadButton("dl_all_csv_zip", "All tables (.zip of CSVs)",
                                    class = "btn-outline-success btn-sm",
                                    icon  = shiny::icon("file-zipper"))
            )
          )
        ),

        bslib::card(
          bslib::card_header(shiny::icon("code"), " Macro YAML",
            shiny::div(class = "ms-auto",
              shiny::actionButton("yaml_apply_edits", "Apply edits",
                                  class = "btn-sm btn-outline-warning",
                                  icon  = shiny::icon("rotate"))
            )
          ),
          shiny::p(class = "text-muted small px-3 pt-2 mb-1",
            "Edit the YAML directly, then click Apply edits to update the step list."),
          shiny::div(
            class = "d-flex gap-2 px-3 mb-2",
            shiny::textInput("macro_name_export", "Name",
                             value = "my_macro", width = "60%"),
            shiny::textInput("macro_dir_export",  "Directory",
                             value = ".",         width = "40%")
          ),
          shinyAce::aceEditor(
            outputId = "macro_yaml_editor",
            mode     = "yaml",
            theme    = "chrome",
            height   = "300px",
            fontSize = 12,
            showLineNumbers = TRUE,
            debounce = 1000,
            value    = "# steps will appear here once you extract a table"
          ),
          bslib::card_footer(
            shiny::div(
              class = "d-flex gap-2",
              shiny::downloadButton("dl_macro_yml", "Download YAML",
                                    class = "btn-warning btn-sm",
                                    icon  = shiny::icon("code")),
              shiny::actionButton("save_macro_disk", "Save to disk",
                                  class = "btn-outline-warning btn-sm",
                                  icon  = shiny::icon("floppy-disk"))
            )
          )
        )
      )
    ),

    # ── Help ──
    bslib::nav_panel(
      title = "Help",
      icon  = shiny::icon("circle-question"),
      bslib::layout_column_wrap(
        width = 1/2,

        bslib::card(
          bslib::card_header(shiny::icon("map"), " Workflow"),
          shiny::tags$ol(
            shiny::tags$li("Upload a PDF using the sidebar."),
            shiny::tags$li("Go to the ", shiny::tags$b("Extract"), " tab."),
            shiny::tags$li("Draw a box on the PDF thumbnail to define the table area."),
            shiny::tags$li("Set a label, choose ", shiny::tags$code("bbox"), " (or lattice/stream), click ", shiny::tags$b("Extract"), "."),
            shiny::tags$li("Switch to the ", shiny::tags$b("Tables"), " tab to rename columns, cast types, and filter rows."),
            shiny::tags$li("Review steps in the ", shiny::tags$b("Steps"), " tab; remove any mistakes."),
            shiny::tags$li("Go to ", shiny::tags$b("Export"), " to download CSVs, Excel, or the YAML macro.")
          )
        ),

        bslib::card(
          bslib::card_header(shiny::icon("sliders"), " Extraction methods"),
          shiny::tags$dl(
            shiny::tags$dt(shiny::tags$code("bbox")),
            shiny::tags$dd("Word-position engine. Best for PDFs with charts on the same page, missing grid lines, or numbers running together. Always pair with a drawn area."),
            shiny::tags$dt(shiny::tags$code("lattice")),
            shiny::tags$dd("tabulapdf. Best for clean PDFs with visible grid lines. Requires Java."),
            shiny::tags$dt(shiny::tags$code("stream")),
            shiny::tags$dd("tabulapdf whitespace mode. Best for tables aligned by spaces rather than lines."),
            shiny::tags$dt(shiny::tags$code("llm")),
            shiny::tags$dd("Sends a rendered page image to an LLM. Best for complex or scanned layouts where other methods fail. Requires an API key."),
            shiny::tags$dt(shiny::tags$code("docling")),
            shiny::tags$dd("IBM Docling ML layout model. Works fully offline after one-time setup. Best for scanned PDFs and complex layouts. Run ", shiny::tags$code("setup_docling()"), " once before first use.")
          )
        )
      )
    )
  )

  # PDF.js from CDN — loaded once in the page head, used by the browse modal.
  # pmInit(url, startPage) initialises the renderer; pmPrev()/pmNext() navigate.
  # pmRender(n) calls Shiny.setInputValue('browse_current_page', n) automatically.
  pdfjs_head <- shiny::tags$head(
    shiny::tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/pdf.js/3.11.174/pdf.min.js",
      crossorigin = "anonymous"
    ),
    shiny::tags$script(shiny::HTML(paste(
      "window._pmDoc = null; window._pmPage = 1;",
      "window.pmInit = function(url, sp) {",
      "  if (typeof pdfjsLib === 'undefined') {",
      "    var c = document.getElementById('pm_pdf_canvas');",
      "    if (c) c.insertAdjacentHTML('afterend',",
      "      '<p style=\"padding:16px;color:#c00;\">PDF.js unavailable (CDN blocked?). Enter page number manually.</p>');",
      "    return;",
      "  }",
      "  pdfjsLib.GlobalWorkerOptions.workerSrc =",
      "    'https://cdnjs.cloudflare.com/ajax/libs/pdf.js/3.11.174/pdf.worker.min.js';",
      "  window._pmDoc = null;",
      "  pdfjsLib.getDocument(url).promise.then(function(pdf) {",
      "    window._pmDoc = pdf;",
      "    pmRender(sp);",
      "  }).catch(function(e) { console.warn('pdfmacro:', e); });",
      "};",
      "window.pmRender = function(n) {",
      "  if (!window._pmDoc) return;",
      "  window._pmDoc.getPage(n).then(function(page) {",
      "    var canvas = document.getElementById('pm_pdf_canvas');",
      "    if (!canvas) return;",
      "    var w = canvas.parentElement.clientWidth || 900;",
      "    var vp0 = page.getViewport({scale:1});",
      "    var vp  = page.getViewport({scale: Math.min((w/vp0.width)*0.97, 2.5)});",
      "    canvas.width  = vp.width;",
      "    canvas.height = vp.height;",
      "    page.render({canvasContext: canvas.getContext('2d'), viewport: vp});",
      "    window._pmPage = n;",
      "    var el = document.getElementById('pm_page_num');",
      "    if (el) el.textContent = n;",
      "    if (window.Shiny) Shiny.setInputValue('browse_current_page', n, {priority:'event'});",
      "  });",
      "};",
      "window.pmPrev = function() { if (window._pmPage > 1) pmRender(window._pmPage - 1); };",
      "window.pmNext = function() {",
      "  if (window._pmDoc && window._pmPage < window._pmDoc.numPages)",
      "    pmRender(window._pmPage + 1);",
      "};",
      sep="\n"
    )))
  )

  kb_js <- shiny::HTML(
    "document.addEventListener('keydown', function(e) {",
    "  var tag = document.activeElement.tagName;",
    "  if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;",
    "  if (e.key === 'ArrowRight') { var b = document.getElementById('ep_next'); if(b) b.click(); }",
    "  if (e.key === 'ArrowLeft')  { var b = document.getElementById('ep_prev'); if(b) b.click(); }",
    "});"
  )

  # Client-side extraction timer — starts on button click (before server receives
  # event), stopped from R via shinyjs::hide() + interval clear in do_extract.
  extract_timer_js <- shiny::HTML("
    var _etInterval = null;
    function startExtractTimer() {
      var t0 = Date.now();
      shinyjs.show('extract_spinner');
      var td = document.getElementById('extract_timer_display');
      if (td) td.textContent = '0';
      clearInterval(_etInterval);
      _etInterval = setInterval(function() {
        var el = document.getElementById('extract_timer_display');
        if (el) el.textContent = Math.floor((Date.now() - t0) / 1000);
      }, 250);
    }
    function stopExtractTimer() {
      clearInterval(_etInterval);
      _etInterval = null;
      shinyjs.hide('extract_spinner');
    }
    document.addEventListener('click', function(e) {
      if (e.target.closest && e.target.closest('#do_extract')) {
        var lbl = document.getElementById('ext_label');
        if (lbl && lbl.value && lbl.value.trim().length > 0) startExtractTimer();
      }
    });
  ")

  item_spinner_js <- shiny::HTML("
    document.addEventListener('click', function(e) {
      if (e.target.closest && e.target.closest('#do_extract_item')) {
        var lbl = document.getElementById('new_item_label');
        var prm = document.getElementById('new_item_prompt');
        if (lbl && lbl.value.trim() && prm && prm.value.trim()) {
          document.getElementById('item_spinner_text').textContent = 'Extracting item…';
          shinyjs.show('item_spinner');
        }
      }
      if (e.target.closest && e.target.closest('#do_batch_items')) {
        document.getElementById('item_spinner_text').textContent = 'Extracting batch…';
        shinyjs.show('item_spinner');
      }
    });
  ")
  # Fixed-position extraction spinner — outside any renderUI (never recreated).
  # shinyjs::hidden() enforces display:none!important via CSS class; show/hide
  # are driven by shinyjs.show/hide in JS so the class approach is consistent.
  extract_spinner_el <- shinyjs::hidden(
    shiny::div(
      id    = "extract_spinner",
      style = paste0(
        "position:fixed;bottom:1rem;right:1rem;z-index:9999;",
        "background:rgba(33,37,41,.92);color:#fff;",
        "padding:.55rem 1rem;border-radius:.45rem;",
        "box-shadow:0 2px 12px rgba(0,0,0,.45);"
      ),
      class = "d-flex align-items-center gap-2",
      shiny::div(class = "spinner-border spinner-border-sm text-warning", role = "status",
                 shiny::tags$span(class = "visually-hidden", "Extracting...")),
      shiny::tags$span(
        "Extracting… ",
        shiny::tags$span(id = "extract_timer_display", "0"),
        "s"
      )
    )
  )

  item_spinner_el <- shinyjs::hidden(
    shiny::div(
      id    = "item_spinner",
      style = paste0(
        "position:fixed;bottom:1rem;left:1rem;z-index:9999;",
        "background:rgba(33,37,41,.92);color:#fff;",
        "padding:.55rem 1rem;border-radius:.45rem;",
        "box-shadow:0 2px 12px rgba(0,0,0,.45);"
      ),
      class = "d-flex align-items-center gap-2",
      shiny::div(class = "spinner-border spinner-border-sm text-warning", role = "status",
                 shiny::tags$span(class = "visually-hidden", "Processing...")),
      shiny::tags$span(id = "item_spinner_text", "Extracting item…")
    )
  )

  bslib::page_sidebar(
    title   = "pdfmacro",
    sidebar = sidebar,
    main,
    theme   = theme,
    shiny::tags$head(shiny::tags$script(kb_js)),
    shiny::tags$head(shiny::tags$script(extract_timer_js)),
    shiny::tags$head(shiny::tags$script(item_spinner_js)),
    pdfjs_head,
    shinyjs::useShinyjs(),
    extract_spinner_el,
    item_spinner_el
  )
}


# --------------------------------------------------------------------------- #
#  Server                                                                      #
# --------------------------------------------------------------------------- #

.pdf_app_server <- function(input, output, session) {

  rv <- shiny::reactiveValues(
    pdf_path      = NULL,
    pdf_serve_url = NULL,   # URL served via addResourcePath for iframe viewer
    n_pages       = 1L,
    tables        = list(),
    items         = list(),
    structs       = list(),
    steps         = list(),
    active_label  = NULL,
    viewer_page   = 1L,
    active_area   = NULL,
    area_active    = FALSE,  # TRUE only after user clicks 'Use selection'
    page_text     = NULL,
    validations   = list(), # stores validate_table() results keyed by label
    scan_results  = NULL    # detect_tables_quietly() output for the Index Scan button
  )

  # ── Dark mode ─────────────────────────────────────────────────────────────
  rv_dark <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$theme_toggle, {
    dark <- !rv_dark()
    rv_dark(dark)
    session$setCurrentTheme(.make_theme(dark))
    shiny::updateActionButton(session, "theme_toggle",
      label = if (dark) "\u2600 light" else "\u263D dark")
  })

  # ── Debounced page reactive ────────────────────────────────────────────────
  # Prevents rapid prev/next clicks from queuing multiple slow renders.
  # The page counter and navigation update instantly; only the image waits.
  viewer_page_r    <- shiny::reactive(rv$viewer_page)
  viewer_page_slow <- shiny::debounce(viewer_page_r, 350)

  # ── PDF upload ─────────────────────────────────────────────────────────────
  # Build filesystem roots: Home directory (expanded) + system volumes
  # normalizePath() is required — shinyFiles does not resolve '~' itself
  .make_sf_roots <- function() {
    vols <- shinyFiles::getVolumes()()
    home <- c(Home = normalizePath("~", mustWork = FALSE))
    # Add working directory if different from home
    wd   <- normalizePath(getwd(), mustWork = FALSE)
    extra <- if (!wd %in% home) c(`Working dir` = wd) else character(0)
    c(home, extra, vols)
  }

  .sf_roots <- .make_sf_roots()

  shinyFiles::shinyFileChoose(input, "pdf_select",
    roots     = .sf_roots,
    filetypes = c("pdf", "PDF"),
    session   = session
  )

  # shinyFiles doesn't auto-close its modal after selection — dismiss it with JS.
  # Bootstrap 5 (used by bslib v5) requires the native API, not jQuery.
  .close_sf_modal <- function() {
    shinyjs::runjs(
      "document.querySelectorAll('.modal.show').forEach(function(el) {
         var m = bootstrap.Modal.getInstance(el);
         if (m) m.hide();
       });"
    )
  }

  shiny::observeEvent(input$pdf_select, {
    req(!is.integer(input$pdf_select))
    info_df <- shinyFiles::parseFilePaths(.sf_roots, input$pdf_select)
    req(nrow(info_df) > 0)
    fpath       <- normalizePath(as.character(info_df$datapath[[1]]))
    rv$pdf_path <- fpath
    info           <- pdftools::pdf_info(fpath)
    rv$n_pages   <- info$pages
    rv$viewer_page <- 1L
    rv$active_area <- NULL
    rv$page_text   <- NULL
    shiny::updateNumericInput(session, "viewer_page", max = rv$n_pages, value = 1)

    # Serve the file's own directory — no file copy needed
    shiny::addResourcePath(paste0("pdfmacro_", session$token), dirname(fpath))
    rv$pdf_serve_url <- paste0(
      "pdfmacro_", session$token, "/", utils::URLencode(basename(fpath))
    )
    .close_sf_modal()
  })

  output$file_status <- shiny::renderUI({
    req(rv$pdf_path)
    shiny::tags$small(class = "text-muted",
      shiny::icon("check-circle"), " ",
      basename(rv$pdf_path), " \u00b7 ", rv$n_pages, " pages"
    )
  })

  # ── Page navigation ────────────────────────────────────────────────────────
  shiny::observeEvent(input$viewer_page, {
    req(rv$pdf_path)
    pg <- max(1L, min(as.integer(input$viewer_page), rv$n_pages))
    rv$viewer_page <- pg
    rv$active_area <- NULL
    rv$area_active <- FALSE
  })
  shiny::observeEvent(input$prev_pg, {
    pg <- max(1L, rv$viewer_page - 1L)
    rv$viewer_page <- pg
    rv$active_area <- NULL
    rv$area_active <- FALSE
    shiny::updateNumericInput(session, "viewer_page", value = pg)
  })
  shiny::observeEvent(input$next_pg, {
    pg <- min(rv$n_pages, rv$viewer_page + 1L)
    rv$viewer_page <- pg
    rv$active_area <- NULL
    rv$area_active <- FALSE
    shiny::updateNumericInput(session, "viewer_page", value = pg)
  })

  output$page_counter <- shiny::renderUI({
    req(rv$pdf_path)
    shiny::span(paste0(rv$viewer_page, " / ", rv$n_pages))
  })

  # ── Sidebar PDF thumbnail ──────────────────────────────────────────────────
  output$pdf_thumb <- shiny::renderImage({
    req(rv$pdf_path)
    pg <- viewer_page_slow()
    page_raw <- tryCatch(
      pdftools::pdf_render_page(rv$pdf_path, page = pg, dpi = 100, numeric = FALSE),
      error = function(e) NULL
    )
    req(!is.null(page_raw))
    img <- .annotate_page(page_raw, rv$active_area, rv$pdf_path, pg, 100)
    tmp <- tempfile(fileext = ".png")
    if (requireNamespace("magick", quietly = TRUE) && inherits(img, "magick-image")) {
      magick::image_write(img, path = tmp, format = "png")
    } else {
      writeBin(as.raw(img), tmp)
    }
    list(src = tmp, contentType = "image/png", width = "100%", deleteFile = TRUE)
  }, deleteFile = TRUE)

  # ── Extract panel (full-size PDF + controls) ───────────────────────────────
  output$extract_panel <- shiny::renderUI({
    if (is.null(rv$pdf_path)) {
      return(bslib::card(
        bslib::card_body(
          shiny::div(class = "text-center text-muted py-5",
            shiny::icon("file-pdf", style = "font-size:3rem; opacity:0.3;"),
            shiny::br(), shiny::br(),
            "Upload a PDF using the sidebar to get started."
          )
        )
      ))
    }
    ns <- session$ns
    bslib::layout_columns(
      col_widths = c(7, 5),

      # Left: PDF with brush
      bslib::card(
        bslib::card_header(
          shiny::icon("file-pdf"), " PDF — page ", rv$viewer_page,
          shiny::div(
            class = "ms-auto d-flex align-items-center gap-1",
            shiny::actionButton("ep_prev", "<", class = "btn-sm btn-outline-secondary"),
            shiny::numericInput("ep_page", NULL, value = rv$viewer_page,
                                min = 1, max = rv$n_pages, width = "65px"),
            shiny::actionButton("ep_next", ">", class = "btn-sm btn-outline-secondary")
          )
        ),
        bslib::card_body(
          padding = 0,
          shiny::imageOutput("extract_pdf_img", height = "600px",
            brush = shiny::brushOpts(
              id         = "extract_brush",
              fill       = "#c47c0030",
              stroke     = "#c47c00",
              resetOnNew = TRUE
            )
          ),
          shiny::uiOutput("brush_bar")
        )
      ),

      # Right: controls + live table
      bslib::card(
        bslib::card_header(shiny::icon("sliders"), " Extraction controls"),
        bslib::card_body(
          shiny::textInput("ext_label",  "Label",  placeholder = "e.g. calf_monthly"),
          bslib::navset_pill(
            id = "ext_tab",
            bslib::nav_panel("Box",
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput("ext_top",    "Top",    0, min = 0)),
                shiny::column(6, shiny::numericInput("ext_left",   "Left",   0, min = 0))
              ),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput("ext_bottom", "Bottom", 500, min = 0)),
                shiny::column(6, shiny::numericInput("ext_right",  "Right",  800, min = 0))
              ),
              shiny::numericInput("ext_page_box",     "Page",         value = 1, min = 1),
              shiny::numericInput("ext_header_rows",  "Header rows",  value = 1, min = 1),
              shiny::conditionalPanel(
                "input.ext_method == 'bbox'",
                shiny::hr(),
                shiny::tags$small(class = "text-muted fw-semibold", "bbox tuning (optional)"),
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput("ext_row_tol", "Row tol", NA, min = 0, step = 1)),
                  shiny::column(6, shiny::numericInput("ext_col_gap", "Col gap", NA, min = 0, step = 1))
                ),
                shiny::tags$small(class = "text-muted",
                  "Leave blank to auto-detect.")
              )
            ),
            bslib::nav_panel("Index",
              shiny::numericInput("ext_page_idx",   "Page",         value = 1, min = 1),
              shiny::numericInput("ext_tbl_idx",    "Table index",  value = 1, min = 1),
              shiny::numericInput("ext_header_idx", "Header rows",  value = 1, min = 1),
              shiny::actionButton("scan_page_tables", "Scan page",
                icon = shiny::icon("search"),
                class = "btn-outline-secondary btn-sm w-100 mt-1")
            ),
            bslib::nav_panel("Fuzzy",
              shiny::textInput("ext_label_match", "Caption text",
                               placeholder = "e.g. Mart Movements by Breed"),
              shiny::numericInput("ext_max_dist", "Max distance", 0.2, min = 0, max = 1, step = 0.05)
            ),
            bslib::nav_panel("Struct",
              shiny::textInput("gs_entity", "Entity",
                placeholder = "e.g. product, invoice, person"),
              shiny::textInput("gs_page", "Page(s)",
                placeholder = "1  or  1,3  or  1-5"),
              shiny::textAreaInput("gs_fields", "Fields (one per line)",
                placeholder = paste(
                  "name: Full product name",
                  "price: Price with currency",
                  "tier [basic|premium]: Subscription tier",
                  "features [list]: Key features",
                  sep = "\n"),
                rows = 5),
              shiny::selectInput("gs_model", "GLiNER model",
                choices = c("fastino/gliner2-base-v1", "fastino/gliner2-large-v1")),
              shiny::tags$small(class = "text-muted",
                shiny::icon("robot"), " Requires ",
                shiny::tags$code("setup_gliner()"), ". ",
                "Annotate: ", shiny::tags$code("[list]"), " for multi-value, ",
                shiny::tags$code("[v1|v2]"), " for enum. One row per entity found.")
            )
          ),
          shiny::uiOutput("scan_results_ui"),
          shiny::conditionalPanel(
            "input.ext_tab !== 'Struct'",
            shiny::selectInput("ext_method", "Method",
                               choices = c("bbox", "lattice", "stream", "llm", "docling"),
                               selected = "bbox"),
            shiny::conditionalPanel(
              "input.ext_method == 'docling'",
              shiny::hr(),
              shiny::numericInput("docling_table_index", "Table index", value = 1L, min = 1L, width = "40%"),
              shiny::tags$small(class = "text-muted",
                shiny::icon("robot"), " Requires ",
                shiny::tags$code("setup_docling()"), " — run once, then restart R. ",
                "First call on a new page loads ML models (30-60 s); subsequent calls are instant."
              )
            ),
            shiny::conditionalPanel(
              "input.ext_method == 'llm'",
              shiny::hr(),
              shiny::div(class = "d-flex gap-2",
                shiny::selectizeInput("llm_provider", "Provider",
                  choices = c("anthropic", "openai", "google_gemini", "openrouter",
                              "groq", "mistral", "deepseek", "ollama", "openai_compatible"),
                  selected = "anthropic", width = "50%",
                  options = list(create = TRUE, createOnBlur = TRUE)),
                shiny::textInput("llm_model", "Model",
                  placeholder = "default", width = "50%")
              ),
              shiny::conditionalPanel(
                "input.llm_provider === 'openai_compatible'",
                shiny::textInput("llm_base_url", "Base URL",
                  placeholder = "http://localhost:1234/v1")
              ),
              shiny::textAreaInput("llm_schema", "Schema",
                placeholder = "Month: character\nMale: integer\nFemale: integer\nTotal: integer\n\nLeave blank to auto-detect.",
                rows = 5),
              shiny::textAreaInput("llm_prompt", "Extra instructions",
                placeholder = "e.g. Ignore the footnote row at the bottom.",
                rows = 2),
              shiny::tags$small(class = "text-muted",
                shiny::icon("key"), " Set ",
                shiny::tags$code("ANTHROPIC_API_KEY"),
                " / ", shiny::tags$code("OPENAI_API_KEY"),
                " / ", shiny::tags$code("GEMINI_API_KEY"), " in .Renviron"
              )
            )
          ),
          shiny::actionButton("do_extract", "Extract",
                              icon  = shiny::icon("table"),
                              class = "btn-warning w-100 mt-2")
        ),
        bslib::card_header(shiny::icon("table"), " Preview", class = "mt-2"),
        bslib::card_body(
          padding = 0,
          DT::DTOutput("extract_preview_dt")
        )
      )
    )
  })

  # ── Extract page navigation ────────────────────────────────────────────────
  shiny::observeEvent(input$ep_page, {
    req(rv$pdf_path)
    pg <- max(1L, min(as.integer(input$ep_page), rv$n_pages))
    rv$viewer_page <- pg
    rv$active_area <- NULL
    shiny::updateNumericInput(session, "viewer_page", value = pg)
  })
  shiny::observeEvent(input$ep_prev, {
    pg <- max(1L, rv$viewer_page - 1L)
    rv$viewer_page <- pg; rv$active_area <- NULL; rv$area_active <- FALSE
    shiny::updateNumericInput(session, "ep_page",      value = pg)
    shiny::updateNumericInput(session, "viewer_page",  value = pg)
  })
  shiny::observeEvent(input$ep_next, {
    pg <- min(rv$n_pages, rv$viewer_page + 1L)
    rv$viewer_page <- pg; rv$active_area <- NULL; rv$area_active <- FALSE
    shiny::updateNumericInput(session, "ep_page",      value = pg)
    shiny::updateNumericInput(session, "viewer_page",  value = pg)
  })

  # ── Extract panel PDF image ────────────────────────────────────────────────
  output$extract_pdf_img <- shiny::renderImage({
    req(rv$pdf_path)
    pg <- viewer_page_slow()
    page_raw <- tryCatch(
      pdftools::pdf_render_page(rv$pdf_path, page = pg, dpi = 150, numeric = FALSE),
      error = function(e) NULL
    )
    req(!is.null(page_raw))
    img <- .annotate_page(page_raw, rv$active_area, rv$pdf_path, pg, 150)
    tmp <- tempfile(fileext = ".png")
    if (requireNamespace("magick", quietly = TRUE) && inherits(img, "magick-image")) {
      magick::image_write(img, path = tmp, format = "png")
    } else { writeBin(as.raw(img), tmp) }
    list(src = tmp, contentType = "image/png", width = "100%", deleteFile = TRUE)
  }, deleteFile = TRUE)

  # ── Brush capture bar ─────────────────────────────────────────────────────
  rv_brush <- shiny::reactive({
    b <- input$extract_brush; req(b)
    pts <- 72 / 150
    c(top    = b$coords_img$ymin * pts, left   = b$coords_img$xmin * pts,
      bottom = b$coords_img$ymax * pts, right  = b$coords_img$xmax * pts)
  })


  output$brush_bar <- shiny::renderUI({
    area <- tryCatch(rv_brush(), error = function(e) NULL)
    req(!is.null(area))
    shiny::div(
      class = "p-2 d-flex align-items-center gap-2",
      style = paste0(
        "position:sticky; bottom:0; z-index:10;",
        "background:var(--raised); border-top:1px solid var(--border); font-size:11px;"
      ),
      shiny::tags$code(sprintf("top=%.0f  left=%.0f  bottom=%.0f  right=%.0f",
                               area["top"], area["left"], area["bottom"], area["right"])),
      shiny::actionButton("use_brush", "Use selection",
                          class = "btn-sm btn-warning ms-auto")
    )
  })

  shiny::observeEvent(input$use_brush, {
    area <- tryCatch(rv_brush(), error = function(e) NULL); req(!is.null(area))
    rv$area_active <- TRUE
    # unname() converts named numeric vector to plain scalar so jsonlite
    # serialises a number, not an object (fixes updateNumericInput silently
    # ignoring named vectors)
    shiny::updateNumericInput(session, "ext_top",      value = unname(round(area["top"])))
    shiny::updateNumericInput(session, "ext_left",     value = unname(round(area["left"])))
    shiny::updateNumericInput(session, "ext_bottom",   value = unname(round(area["bottom"])))
    shiny::updateNumericInput(session, "ext_right",    value = unname(round(area["right"])))
    shiny::updateNumericInput(session, "ext_page_box", value = unname(rv$viewer_page))
    bslib::nav_select("ext_tab", "Box", session = session)
    shiny::showNotification("Box tab filled. Set a label and click Extract.", type = "message")
  })

  # ── Browse modal — PDF.js canvas renderer ────────────────────────────────
  # Renders the PDF on a <canvas> via PDF.js (loaded from CDN in page head).
  # JS calls Shiny.setInputValue('browse_current_page', n) on every page
  # render so the page number is always current without any iframe hacks.
  shiny::observeEvent(input$open_browse, {
    req(rv$pdf_path)
    if (is.null(rv$pdf_serve_url)) {
      serve_dir <- dirname(rv$pdf_path)
      shiny::addResourcePath(paste0("pdfmacro_", session$token), serve_dir)
      rv$pdf_serve_url <- paste0(
        "pdfmacro_", session$token, "/",
        utils::URLencode(basename(rv$pdf_path))
      )
    }
    start_page <- rv$viewer_page
    n_pages    <- rv$n_pages

    shiny::showModal(shiny::modalDialog(
      title = shiny::div(
        class = "d-flex align-items-center gap-2 w-100",
        shiny::icon("magnifying-glass"), " Browse PDF",
        shiny::tags$small(class = "text-muted ms-2",
          "Navigate to your table, then click Extract.")
      ),
      # PDF rendered on canvas by PDF.js
      shiny::div(
        style = "background:#525659; overflow-y:auto; height:68vh; display:flex; justify-content:center; border-radius:4px;",
        shiny::tags$canvas(id = "pm_pdf_canvas",
                           style = "display:block; max-width:100%;")
      ),
      # Navigation bar — JS updates pm_page_num span and browse_current_page input
      shiny::div(
        class = "d-flex align-items-center gap-2 mt-2",
        shiny::tags$button(
          "‹ Prev", onclick = "pmPrev()",
          class = "btn btn-outline-secondary btn-sm"
        ),
        shiny::div(
          class = "d-flex align-items-center gap-1 px-2",
          style = "font-family:monospace; font-size:13px;",
          "Page ",
          shiny::tags$span(id = "pm_page_num",
                           style = "font-weight:700; min-width:2em; text-align:center;",
                           as.character(start_page)),
          " of ", as.character(n_pages)
        ),
        shiny::tags$button(
          "Next ›", onclick = "pmNext()",
          class = "btn btn-outline-secondary btn-sm"
        ),
        shiny::tags$small(class = "text-muted ms-3",
          shiny::icon("circle-info"),
          " Page counter updates automatically as you navigate.")
      ),
      footer = shiny::tagList(
        shiny::modalButton("Close"),
        shiny::actionButton("browse_use_page", "Extract from this page",
                            icon  = shiny::icon("table"),
                            class = "btn-warning")
      ),
      size = "xl", easyClose = TRUE
    ))

    # Initialise PDF.js 200 ms after modal DOM renders
    shinyjs::delay(200, shinyjs::runjs(paste0(
      "pmInit('", rv$pdf_serve_url, "', ", start_page, ");"
    )))
  })

  # browse_current_page is set by JS pmRender() via Shiny.setInputValue
  shiny::observeEvent(input$browse_current_page, {}, ignoreInit = TRUE)

  # "Extract from this page" — navigate extract panel to the selected page
  shiny::observeEvent(input$browse_use_page, {
    pg <- as.integer(input$browse_current_page %||% input$browse_extract_page %||% rv$viewer_page)
    pg <- max(1L, min(pg, rv$n_pages))
    rv$viewer_page <- pg
    rv$active_area <- NULL
    rv$area_active <- FALSE
    shiny::updateNumericInput(session, "viewer_page",   value = pg)
    shiny::updateNumericInput(session, "ep_page",       value = pg)
    shiny::updateNumericInput(session, "ext_page_box",  value = unname(pg))
    shiny::removeModal()
    shiny::showNotification(
      paste0("Now on page ", pg, " — draw a box on the PDF and click Extract."),
      type = "message"
    )
  })

    # ── Do extract ────────────────────────────────────────────────────────────
  shiny::observeEvent(input$do_extract, {
    # Explicit stopExtractTimer() at EVERY exit point — on.exit is unreliable
    # when req() throws a silent condition and Shiny intercepts it before the
    # session flushes the queued JS message.
    if (is.null(rv$pdf_path)) {
      shinyjs::hide("extract_spinner"); shinyjs::runjs("clearInterval(window._etInterval);window._etInterval=null;")
      return()
    }
    if (nchar(trimws(input$ext_label %||% "")) == 0) {
      shinyjs::hide("extract_spinner"); shinyjs::runjs("clearInterval(window._etInterval);window._etInterval=null;")
      shiny::showNotification(
        "Please enter a table label before extracting.",
        type = "warning", duration = 4
      )
      return()
    }
    label  <- trimws(input$ext_label)
    method <- input$ext_method
    tab    <- input$ext_tab

    result <- tryCatch({

      struct_done <- FALSE

      if (tab == "Box") {
        page        <- as.integer(input$ext_page_box)
        header_rows <- as.integer(input$ext_header_rows)
        # Only use area when the user has actively drawn a box for this page;
        # stale coordinates from a previous page are ignored
        area <- if (isTRUE(rv$area_active)) {
          c(top    = input$ext_top,    left   = input$ext_left,
            bottom = input$ext_bottom, right  = input$ext_right)
        } else {
          NULL
        }
        label_match <- NULL; table_index <- 1L
      } else if (tab == "Index") {
        page        <- as.integer(input$ext_page_idx)
        header_rows <- as.integer(input$ext_header_idx)
        table_index <- as.integer(input$ext_tbl_idx)
        area        <- NULL; label_match <- NULL
        if (method == "bbox") {
          shiny::showNotification(
            "bbox without an area will scan the full page — charts may appear. Use the Box tab for best results.",
            type = "warning", duration = 8
          )
        }
      } else if (tab == "Fuzzy") {
        lm <- trimws(input$ext_label_match %||% "")
        if (nchar(lm) == 0) stop("Enter a caption to search for in the Fuzzy tab.")
        if (is.null(rv$page_text)) rv$page_text <- pdftools::pdf_text(rv$pdf_path)
        found <- .fuzzy_find_page(list(text = rv$page_text), lm, "jw", input$ext_max_dist)
        page <- found$page; header_rows <- 1L; table_index <- 1L
        area <- NULL; label_match <- lm
      } else if (tab == "Struct") {
        page <- .parse_page_range(input$gs_page %||% "") %||% 1L
        fields_text <- trimws(input$gs_fields %||% "")
        if (!nzchar(fields_text)) stop("Define at least one field in the Struct tab.")
        parsed <- .parse_struct_fields(fields_text)
        if (length(parsed$fields) == 0L) stop("No valid 'field: description' pairs found.")
        gs_model <- input$gs_model %||% "fastino/gliner2-base-v1"
        tmp <- new.env(parent = emptyenv())
        tmp$path <- rv$pdf_path; tmp$tables <- list(); tmp$items <- list()
        tmp$structs <- list(); tmp$steps <- list()
        class(tmp) <- "pdfmacro_session"
        entity_val <- trimws(input$gs_entity %||% "")
        if (!nzchar(entity_val)) entity_val <- label
        select_struct(tmp, label = label, entity = entity_val,
          fields = parsed$fields, list_fields = parsed$list_fields,
          enum_fields = parsed$enum_fields,
          page = page, gliner_model = gs_model)
        struct_to_df(tmp, label)
        df <- tmp$tables[[label]]
        if (is.null(df) || nrow(df) == 0L) stop("No structured records found.")
        rv$tables[[label]] <- df
        rv$structs[[label]] <- tmp$structs[[label]]
        for (s in tmp$steps) {
          rv$steps <- .replace_or_append_extraction(rv$steps, label, s)
        }
        rv$active_label <- label
        rv$active_area  <- NULL
        first_page <- if (is.list(page)) page[[1L]] else page[[1L]]
        shiny::updateNumericInput(session, "viewer_page", value = first_page)
        shiny::showNotification(
          paste0("Extracted struct '", label, "': ", nrow(df), " record",
                 if (nrow(df) == 1L) "" else "s", " × ", ncol(df), " field",
                 if (ncol(df) == 1L) "" else "s"),
          type = "message"
        )
        struct_done <- TRUE
      }

      if (!isTRUE(struct_done)) {

      if (method == "docling") {
        if (!requireNamespace("reticulate", quietly = TRUE))
          stop("Install reticulate and run pdfmacro::setup_docling() first.")
        tbl_idx <- as.integer(input$docling_table_index %||% 1L)
        tmp <- new.env(parent = emptyenv())
        tmp$path <- rv$pdf_path; tmp$tables <- list(); tmp$steps <- list()
        tmp$.replaying <- TRUE; class(tmp) <- "pdfmacro_session"
        select_table_docling(tmp, label = label, page = page, table_index = tbl_idx)
        df <- tmp$tables[[label]]
        if (is.null(df) || nrow(df) == 0) stop("Docling returned no data.")
        rv$tables[[label]] <- df
        new_step <- list(
          step = "select_table_docling", label = label,
          page = page, table_index = tbl_idx)
        rv$steps <- .replace_or_append_extraction(rv$steps, label, new_step)
      } else if (method == "llm") {
        if (!requireNamespace("ellmer", quietly = TRUE))
          stop("Install the 'ellmer' package for LLM extraction.")
        schema_val <- .parse_schema_text(input$llm_schema %||% "")
        model_val  <- trimws(input$llm_model  %||% "")
        prompt_val <- trimws(input$llm_prompt %||% "")
        tmp <- new.env(parent = emptyenv())
        tmp$path <- rv$pdf_path; tmp$tables <- list(); tmp$steps <- list()
        tmp$.replaying <- TRUE; class(tmp) <- "pdfmacro_session"
          base_url_val <- trimws(input$llm_base_url %||% "")
        select_table_llm(tmp, label = label, page = page, area = area,
          provider    = input$llm_provider %||% "anthropic",
          model       = if (nchar(model_val) > 0) model_val else NULL,
          base_url    = if (nchar(base_url_val) > 0) base_url_val else NULL,
          schema      = schema_val,
          prompt      = if (nchar(prompt_val) > 0) prompt_val else NULL,
          dpi = 150L, header_rows = header_rows)
        df <- tmp$tables[[label]]
        if (is.null(df) || nrow(df) == 0) stop("LLM returned no data.")
        rv$tables[[label]] <- df
        new_step <- list(
          step     = "select_table_llm", label = label, page = page, area = area,
          provider = input$llm_provider %||% "anthropic",
          model    = if (nchar(model_val) > 0) model_val else NULL,
          base_url = if (nchar(base_url_val) > 0) base_url_val else NULL,
          schema   = if (!is.null(schema_val)) as.list(schema_val) else NULL,
          prompt   = if (nchar(prompt_val) > 0) prompt_val else NULL,
          dpi = 150L, header_rows = header_rows)
        rv$steps <- .replace_or_append_extraction(rv$steps, label, new_step)
      } else if (method == "bbox") {
        rt <- if (is.na(input$ext_row_tol) || input$ext_row_tol == 0) NULL else input$ext_row_tol
        cg <- if (is.na(input$ext_col_gap) || input$ext_col_gap == 0) NULL else input$ext_col_gap
        df <- .extract_bbox(rv$pdf_path, page, area = area,
                            header_rows = header_rows, row_tol = rt, col_gap = cg)
        if (nrow(df) == 0) stop("bbox returned no data. Adjust area or tuning.")
        rv$tables[[label]] <- df
        new_step <- list(
          step = "select_table", label = label, page = page,
          table_index = table_index, area = area, label_match = label_match,
          method = method, header_rows = header_rows)
        rv$steps <- .replace_or_append_extraction(rv$steps, label, new_step)
      } else {
        args <- list(file = rv$pdf_path, pages = page, method = method,
                     output = "matrix", guess = is.null(area))
        if (!is.null(area)) args$area <- list(area)
        if (!requireNamespace("tabulapdf", quietly=TRUE))
          stop("tabulapdf required for lattice/stream. Install with install.packages('tabulapdf'). Java also needed.")
        raw  <- do.call(tabulapdf::extract_tables, args)
        if (length(raw) == 0) stop("No tables found on page ", page)
        df <- .matrix_to_df(raw[[min(table_index, length(raw))]])
        if (header_rows > 1L) df <- .flatten_headers(df, header_rows)
        rv$tables[[label]] <- df
        new_step <- list(
          step = "select_table", label = label, page = page,
          table_index = table_index, area = area, label_match = label_match,
          method = method, header_rows = header_rows)
        rv$steps <- .replace_or_append_extraction(rv$steps, label, new_step)
      }
      rv$active_label <- label
      rv$active_area  <- area
      shiny::updateNumericInput(session, "viewer_page", value = page)
      shiny::showNotification(
        paste0("Extracted '", label, "': ", nrow(df), " \u00d7 ", ncol(df)),
        type = "message"
      )
      } # end if (!isTRUE(struct_done))
      "ok"
    }, error = function(e) e$message)

    shinyjs::hide("extract_spinner"); shinyjs::runjs("clearInterval(window._etInterval);window._etInterval=null;")

    if (!identical(result, "ok")) {
      shiny::showNotification(paste("Extraction failed:", result), type = "error")
    }
  })

  output$extract_preview_dt <- DT::renderDT({
    req(rv$active_label, rv$active_label %in% names(rv$tables))
    DT::datatable(rv$tables[[rv$active_label]],
                  options = list(scrollX = TRUE, pageLength = 8, dom = "tip"),
                  rownames = FALSE)
  })

  # ── Scan page (detect tables) ─────────────────────────────────────────────
  shiny::observeEvent(input$scan_page_tables, {
    req(rv$pdf_path)
    page   <- as.integer(input$ext_page_idx %||% 1L)
    method <- input$ext_method %||% "lattice"
    if (!method %in% c("lattice", "stream", "docling")) method <- "lattice"
    rv$scan_results <- NULL
    id <- shiny::showNotification(
      paste0("Scanning page ", page, " with ", method, "…"),
      duration = NULL, type = "message"
    )
    on.exit(shiny::removeNotification(id))
    rv$scan_results <- tryCatch(
      detect_tables_quietly(rv$pdf_path, pages = page, method = method),
      error = function(e) {
        shiny::showNotification(paste("Scan failed:", e$message), type = "error")
        NULL
      }
    )
  })

  output$scan_results_ui <- shiny::renderUI({
    res <- rv$scan_results
    if (is.null(res)) return(NULL)
    items <- .flatten_scan(res)
    if (length(items) == 0L) {
      return(shiny::div(class = "text-muted small mt-1 mb-1",
        shiny::icon("circle-xmark"), " No tables detected on this page."))
    }
    cards <- lapply(seq_along(items), function(i) {
      it  <- items[[i]]
      nr  <- nrow(it$df); nc <- ncol(it$df)
      cols <- paste(head(names(it$df), 4L), collapse = ", ")
      if (nc > 4L) cols <- paste0(cols, "…")
      shiny::div(
        class = "d-flex align-items-center justify-content-between border rounded px-2 py-1 mb-1",
        shiny::div(
          shiny::div(class = "small fw-semibold",
            paste0("Table ", it$index, " — p.", it$page)),
          shiny::div(class = "text-muted", style = "font-size:0.75rem",
            paste0(nr, " × ", nc, "  ", cols))
        ),
        shiny::actionButton(
          paste0("scan_btn_", i), "Use",
          class = "btn-sm btn-outline-warning py-0",
          onclick = sprintf(
            "Shiny.setInputValue('scan_use_selected', %d, {priority: 'event'})", i)
        )
      )
    })
    shiny::div(class = "mt-1 mb-1",
      shiny::div(class = "text-muted small mb-1",
        shiny::icon("table"), paste0(" ", length(items), " table(s) found:")),
      do.call(shiny::tagList, cards)
    )
  })

  shiny::observeEvent(input$scan_use_selected, {
    req(rv$scan_results)
    i     <- as.integer(input$scan_use_selected)
    items <- .flatten_scan(rv$scan_results)
    if (i < 1L || i > length(items)) return()
    it <- items[[i]]
    shiny::updateNumericInput(session, "ext_page_idx", value = it$page)
    shiny::updateNumericInput(session, "ext_tbl_idx",  value = it$index)
    bslib::nav_select("ext_tab", "Index")
  }, ignoreNULL = TRUE)

  # ── Tables panel ──────────────────────────────────────────────────────────
  output$tables_panel <- shiny::renderUI({
    if (length(rv$tables) == 0) {
      return(bslib::card(bslib::card_body(
        shiny::div(class = "text-center text-muted py-5",
          shiny::icon("table", style = "font-size:3rem; opacity:0.3;"),
          shiny::br(), shiny::br(), "No tables extracted yet.")
      )))
    }
    tabs <- lapply(names(rv$tables), function(lbl) {
      df <- rv$tables[[lbl]]
      bslib::nav_panel(
        title = lbl,
        bslib::card(
          bslib::card_header(
            shiny::icon("table"), " ",
            shiny::tags$span(class = "table-label", lbl),
            shiny::tags$small(class = "ms-2 text-muted",
                              paste0(nrow(df), " \u00d7 ", ncol(df)))
          ),
          bslib::card_body(
            padding = 0,
            DT::DTOutput(paste0("tbl_dt_", lbl))
          ),
          bslib::card_footer(
            shiny::div(
              class = "d-flex gap-2 flex-wrap",
              shiny::actionButton(paste0("btn_rename_",   lbl), "Rename columns",
                                  class = "btn-sm btn-outline-secondary",
                                  icon  = shiny::icon("pen")),
              shiny::actionButton(paste0("btn_cast_",     lbl), "Cast types",
                                  class = "btn-sm btn-outline-secondary",
                                  icon  = shiny::icon("wand-magic-sparkles")),
              shiny::actionButton(paste0("btn_filter_",   lbl), "Filter rows",
                                  class = "btn-sm btn-outline-danger",
                                  icon  = shiny::icon("filter")),
              shiny::actionButton(paste0("btn_validate_", lbl), "Validate",
                                  class = "btn-sm btn-outline-primary",
                                  icon  = shiny::icon("circle-check")),
              shiny::actionButton(paste0("btn_stats_",    lbl), "Column stats",
                                  class = "btn-sm btn-outline-secondary",
                                  icon  = shiny::icon("chart-simple"))
            )
          )
        )
      )
    })
    do.call(bslib::navset_card_underline, c(tabs, list(id = "tables_navset")))
  })

  # Render each table DT and wire up transform buttons dynamically.
  # registered_tbl_labels tracks which labels already have observers so the
  # observe loop below never registers duplicates — re-running on rv$tables
  # change (e.g. after rename/cast writes back) would otherwise stack observers.
  registered_tbl_labels <- character(0)

  shiny::observe({
    new_labels <- setdiff(names(rv$tables), registered_tbl_labels)
    for (lbl in new_labels) {
      registered_tbl_labels <<- c(registered_tbl_labels, lbl)
      local({
        l <- lbl
        output[[paste0("tbl_dt_", l)]] <- DT::renderDT({
          req(l %in% names(rv$tables))
          DT::datatable(rv$tables[[l]],
                        options = list(scrollX = TRUE, pageLength = 10, dom = "tip"),
                        rownames = FALSE)
        })
        # Render eagerly even when tab is not active — prevents empty card on re-visit
        shiny::outputOptions(output, paste0("tbl_dt_", l), suspendWhenHidden = FALSE)

        # Rename modal
        shiny::observeEvent(input[[paste0("btn_rename_", l)]], {
          df   <- rv$tables[[l]]; cols <- names(df)
          rows <- lapply(seq_along(cols), function(i) {
            shiny::fluidRow(
              shiny::column(5, shiny::tags$code(cols[[i]])),
              shiny::column(7, shiny::textInput(paste0("ren_", l, "_", i),
                                                NULL, value = cols[[i]]))
            )
          })
          shiny::showModal(shiny::modalDialog(
            title  = paste("Rename columns:", l),
            shiny::div(rows),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(paste0("apply_rename_", l), "Apply",
                                  class = "btn-warning")
            ), size = "l"
          ))
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("apply_rename_", l)]], {
          df <- rv$tables[[l]]; cols <- names(df)
          mapping <- setNames(
            vapply(seq_along(cols), function(i) {
              v <- input[[paste0("ren_", l, "_", i)]]
              if (is.null(v) || nchar(trimws(v)) == 0) cols[[i]] else trimws(v)
            }, character(1)),
            cols
          )
          changed <- mapping[mapping != cols]
          if (length(changed) > 0) {
            idx <- match(names(changed), names(df))
            names(df)[idx] <- unname(changed)
            rv$tables[[l]] <- df
            rv$steps <- .module_record(rv$steps,
              list(step = "rename_columns", table = l, mapping = as.list(changed)))
          }
          shiny::removeModal()
        }, ignoreInit = TRUE)

        # Cast types modal
        shiny::observeEvent(input[[paste0("btn_cast_", l)]], {
          df   <- rv$tables[[l]]; cols <- names(df)
          type_choices <- c("character", "numeric", "integer",
                            "date:%d/%m/%Y", "date:%Y-%m-%d")
          rows <- lapply(seq_along(cols), function(i) {
            shiny::fluidRow(
              shiny::column(5, shiny::tags$code(cols[[i]])),
              shiny::column(7, shiny::selectInput(paste0("cast_", l, "_", i), NULL,
                               choices = type_choices,
                               selected = .guess_type(df[[cols[[i]]]])))
            )
          })
          shiny::showModal(shiny::modalDialog(
            title  = paste("Cast types:", l),
            shiny::div(rows),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(paste0("apply_cast_", l), "Apply",
                                  class = "btn-warning")
            ), size = "l"
          ))
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("apply_cast_", l)]], {
          df <- rv$tables[[l]]; cols <- names(df)
          types <- setNames(
            vapply(seq_along(cols), function(i)
              input[[paste0("cast_", l, "_", i)]] %||% "character", character(1)),
            cols
          )
          for (col in cols) {
            df[[col]] <- tryCatch(.cast_col(df[[col]], types[[col]]),
                                  error = function(e) df[[col]])
          }
          rv$tables[[l]] <- df
          rv$steps <- .module_record(rv$steps,
            list(step = "cast_types", table = l, types = as.list(types)))
          shiny::removeModal()
        }, ignoreInit = TRUE)

        # Filter modal — GUI column / operator / value → R expression
        shiny::observeEvent(input[[paste0("btn_filter_", l)]], {
          df   <- rv$tables[[l]]
          cols <- names(df)
          shiny::showModal(shiny::modalDialog(
            title = paste("Filter rows:", l),
            shiny::p(class = "text-muted small",
              "Rows where the condition is TRUE will be removed."),
            shiny::fluidRow(
              shiny::column(4,
                shiny::selectInput(paste0("fcol_", l), "Column", choices = cols)
              ),
              shiny::column(4,
                shiny::selectInput(paste0("fop_", l), "Condition",
                                   choices = .filter_op_choices)
              ),
              shiny::column(4,
                shiny::conditionalPanel(
                  paste0("['==','!=','>','>=','<','<=','grepl','!grepl'].indexOf(input['fcol_", l, "']) === -1 || true"),
                  shiny::uiOutput(paste0("fval_ui_", l))
                )
              )
            ),
            shiny::div(class = "mt-2 p-2 rounded",
              style = "background:var(--raised); font-size:12px;",
              "Will remove rows where: ",
              shiny::uiOutput(paste0("fpreview_", l), inline = TRUE)
            ),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(paste0("apply_filter_", l), "Apply",
                                  class = "btn-danger", icon = shiny::icon("filter"))
            )
          ))
        }, ignoreInit = TRUE)

        output[[paste0("fval_ui_", l)]] <- shiny::renderUI({
          op <- input[[paste0("fop_", l)]] %||% "=="
          if (op %in% c("is.na", "!is.na")) return(NULL)
          shiny::textInput(paste0("fval_", l), "Value", placeholder = "e.g. Total")
        })

        output[[paste0("fpreview_", l)]] <- shiny::renderUI({
          col <- input[[paste0("fcol_", l)]]; req(col)
          op  <- input[[paste0("fop_",  l)]] %||% "=="
          val <- input[[paste0("fval_", l)]] %||% ""
          expr <- tryCatch(.build_filter_expr(col, op, val), error = function(e) "...")
          shiny::tags$code(expr)
        })

        shiny::observeEvent(input[[paste0("apply_filter_", l)]], {
          col  <- input[[paste0("fcol_", l)]]; req(col)
          op   <- input[[paste0("fop_",  l)]] %||% "=="
          val  <- input[[paste0("fval_", l)]] %||% ""
          expr <- tryCatch(.build_filter_expr(col, op, val),
                           error = function(e) {
                             shiny::showNotification(e$message, type = "error"); NULL
                           })
          req(!is.null(expr))
          df   <- rv$tables[[l]]
          mask <- tryCatch(eval(parse(text = expr), envir = df),
                           error = function(e) {
                             shiny::showNotification(
                               paste("Filter error:", e$message), type = "error"); NULL
                           })
          req(!is.null(mask), is.logical(mask))
          df2 <- df[!mask, , drop = FALSE]; rownames(df2) <- NULL
          rv$tables[[l]] <- df2
          rv$steps <- .module_record(rv$steps,
            list(step = "filter_rows", table = l, exclude_where = expr))
          shiny::removeModal()
        }, ignoreInit = TRUE)

        # ── Column stats modal ─────────────────────────────────────────────
        shiny::observeEvent(input[[paste0("btn_stats_", l)]], {
          df   <- rv$tables[[l]]
          rows <- lapply(names(df), function(col) {
            x      <- df[[col]]
            is_num <- is.numeric(x)
            n_na   <- sum(is.na(x))
            uniq   <- length(unique(x[!is.na(x)]))
            shiny::fluidRow(
              shiny::column(3, shiny::tags$code(col)),
              shiny::column(2, if (is_num) formatC(min(x, na.rm=TRUE), format="fg", digits=4) else class(x)),
              shiny::column(2, if (is_num) formatC(max(x, na.rm=TRUE), format="fg", digits=4) else paste0(uniq, " unique")),
              shiny::column(2, if (is_num) formatC(mean(x, na.rm=TRUE), format="fg", digits=4) else ""),
              shiny::column(2, paste0(n_na, " NAs  (", round(100*n_na/nrow(df)), "%)")),
              shiny::column(1, if (!is_num) paste0(uniq, " uniq") else "")
            )
          })
          shiny::showModal(shiny::modalDialog(
            title = paste("Column stats:", l),
            shiny::fluidRow(
              shiny::column(3, shiny::tags$b("Column")),
              shiny::column(2, shiny::tags$b("Min")),
              shiny::column(2, shiny::tags$b("Max")),
              shiny::column(2, shiny::tags$b("Mean")),
              shiny::column(2, shiny::tags$b("NAs")),
              shiny::column(1, "")
            ),
            shiny::hr(),
            shiny::div(rows),
            footer = shiny::modalButton("Close"),
            size = "l"
          ))
        }, ignoreInit = TRUE)

        # ── Validate modal ─────────────────────────────────────────────────
        shiny::observeEvent(input[[paste0("btn_validate_", l)]], {
          # Pre-fill with any existing rules for this table from step list
          existing_rules <- ""
          for (s in rev(rv$steps)) {
            if (isTRUE(s$step == "validate_table") && identical(s$table, l)) {
              existing_rules <- paste(
                paste0(names(s$rules), ": ", unlist(s$rules)),
                collapse = "\n")
              break
            }
          }
          shiny::showModal(shiny::modalDialog(
            title = paste("Validate:", l),
            shiny::p(class = "text-muted small",
              "One rule per line: ", shiny::tags$code("rule_name: expression"),
              ". Expressions are evaluated against the data frame columns."),
            shiny::tags$small(class = "text-muted",
              "Examples: ",
              shiny::tags$code("no_na_month: !anyNA(month)"),
              " | ",
              shiny::tags$code("twelve_rows: nrow(.) == 12"),
              " | ",
              shiny::tags$code("positive: all(total > 0, na.rm=TRUE)")
            ),
            shiny::hr(),
            shiny::textAreaInput(paste0("val_rules_", l), "Rules",
                                 value = existing_rules,
                                 rows = 6,
                                 placeholder = "twelve_rows: nrow(.) == 12\nno_na: !anyNA(month)"),
            shiny::checkboxInput(paste0("val_strict_", l), "Strict (error on failure)", value = FALSE),
            shiny::uiOutput(paste0("val_result_", l)),
            footer = shiny::tagList(
              shiny::modalButton("Close"),
              shiny::actionButton(paste0("apply_validate_", l), "Run validation",
                                  class = "btn-primary", icon = shiny::icon("circle-check"))
            ),
            size = "l"
          ))
        }, ignoreInit = TRUE)

        output[[paste0("val_result_", l)]] <- shiny::renderUI({
          res <- rv$validations[[l]]
          if (is.null(res)) return(NULL)
          summ <- res$summary
          rows <- lapply(seq_len(nrow(summ)), function(i) {
            ok  <- summ$fails[i] == 0 && !isTRUE(summ$error[i])
            cls <- if (ok) "text-success" else "text-danger"
            ico <- if (ok) shiny::icon("check") else shiny::icon("xmark")
            shiny::div(class = paste("d-flex gap-2 align-items-center mb-1", cls),
              ico, shiny::tags$code(summ$name[i]),
              shiny::tags$small(paste0(
                summ$passes[i], " pass / ", summ$fails[i], " fail",
                if (summ$nNA[i] > 0) paste0(" / ", summ$nNA[i], " NA") else ""
              ))
            )
          })
          shiny::div(class = "mt-2", rows)
        })

        shiny::observeEvent(input[[paste0("apply_validate_", l)]], {
          txt <- trimws(input[[paste0("val_rules_", l)]] %||% "")
          req(nchar(txt) > 0)
          strict <- isTRUE(input[[paste0("val_strict_", l)]])

          # Parse "name: expression" lines
          lines  <- trimws(strsplit(txt, "\n")[[1]])
          lines  <- lines[nchar(lines) > 0]
          rules  <- vapply(lines, function(ln) {
            parts <- strsplit(ln, ":\\s*", perl = TRUE)[[1]]
            if (length(parts) >= 2) paste(parts[-1], collapse = ": ") else ln
          }, character(1))
          nms    <- vapply(lines, function(ln) {
            strsplit(ln, ":\\s*", perl = TRUE)[[1]][[1]]
          }, character(1))
          names(rules) <- nms

          if (!requireNamespace("validate", quietly = TRUE)) {
            shiny::showNotification("Install the 'validate' package.", type = "error")
            return()
          }

          df    <- rv$tables[[l]]
          exprs <- lapply(rules, function(r) parse(text = r)[[1]])
          names(exprs) <- nms
          v     <- tryCatch(do.call(validate::validator, exprs),
                            error = function(e) {
                              shiny::showNotification(e$message, type = "error"); NULL
                            })
          req(!is.null(v))
          cf   <- validate::confront(df, v)
          summ <- as.data.frame(validate::summary(cf))

          # Store in rv$validations
          if (is.null(rv$validations)) rv$validations <- list()
          rv$validations[[l]] <- list(
            summary = summ,
            failed  = summ$name[summ$fails > 0 | isTRUE(summ$error)],
            passed  = summ$name[summ$fails == 0 & !isTRUE(summ$error)]
          )
          rv$steps <- .module_record(rv$steps,
            list(step = "validate_table", table = l,
                 rules = as.list(rules), strict = strict))

          n_fail <- length(rv$validations[[l]]$failed)
          if (n_fail == 0) {
            shiny::showNotification(
              paste0("All ", nrow(summ), " rules passed for '", l, "'"),
              type = "message")
          } else {
            shiny::showNotification(
              paste0(n_fail, " rule(s) failed for '", l, "'"),
              type = "warning")
          }
        }, ignoreInit = TRUE)
      })
    }
  })

  # ── Items panel ──────────────────────────────────────────────────────────

  output$items_list_ui <- shiny::renderUI({
    if (length(rv$items) == 0) {
      return(shiny::div(class = "text-center text-muted py-4",
        shiny::icon("tag", style = "font-size:2rem;opacity:0.3;"),
        shiny::br(), shiny::br(),
        "No items extracted yet.",
        shiny::br(),
        shiny::tags$small("Use the Extract item button to pull metadata fields like invoice numbers, dates, and totals.")))
    }
    rows <- lapply(names(rv$items), function(lbl) {
      item <- rv$items[[lbl]]
      bslib::card(
        class = "mb-2",
        bslib::card_body(
          class = "py-2",
          shiny::div(
            class = "d-flex align-items-start gap-2",
            shiny::div(
              shiny::tags$small(class = "text-muted", lbl),
              if (length(item$value) > 1L) {
                shiny::div(
                  class = "fw-bold",
                  shiny::tags$ol(
                    class = "mb-0 ps-3",
                    lapply(as.character(item$value), shiny::tags$li)
                  )
                )
              } else {
                shiny::div(class = "fw-bold", as.character(item$value))
              },
              shiny::tags$small(class = "text-muted",
                item$cast, " · ",
                if (identical(item$backend, "gliner")) "GLiNER" else item$provider %||% "llm")
            ),
            shiny::div(
              class = "ms-auto d-flex gap-1",
              shiny::actionButton(paste0("edit_item_", lbl), "",
                                  icon  = shiny::icon("pen"),
                                  class = "btn-sm btn-outline-secondary",
                                  title = "Edit prompt"),
              shiny::actionButton(paste0("del_item_", lbl), "",
                                  icon  = shiny::icon("trash"),
                                  class = "btn-sm btn-outline-danger",
                                  title = "Delete item")
            )
          )
        )
      )
    })
    shiny::div(rows)
  })

  # Observe delete buttons dynamically
  shiny::observe({
    for (lbl in names(rv$items)) {
      local({
        l <- lbl
        shiny::observeEvent(input[[paste0("del_item_", l)]], {
          rv$items[[l]] <- NULL
          # Remove matching select_item step
          rv$steps <- rv$steps[!vapply(rv$steps, function(s)
            isTRUE(s$step == "select_item") && identical(s$label, l), logical(1))]
          shiny::showNotification(paste0("Deleted item '", l, "'"), type = "message")
        }, ignoreInit = TRUE)
      })
    }
  })

  # Extract item modal
  shiny::observeEvent(input$open_extract_item, {
    # Pre-fill area from the current brush selection if one exists
    brush_area <- tryCatch(rv_brush(), error = function(e) NULL)
    has_brush  <- !is.null(brush_area)

    shiny::showModal(shiny::modalDialog(
      title = "Extract item",
      shiny::p(class = "text-muted small",
        "Items are single metadata fields (invoice number, date, total etc.) ",
        "extracted via LLM or the local GLiNER model."),
      # Label + backend on the same row
      shiny::div(
        class = "d-flex gap-2 mb-2 align-items-end",
        shiny::div(class = "flex-fill",
          shiny::textInput("new_item_label", "Label",
                           placeholder = "e.g. invoice_number")),
        shiny::div(
          shiny::selectInput("new_item_backend", "Backend",
            choices  = c("LLM" = "llm", "GLiNER (local)" = "gliner"),
            selected = "llm", width = "160px"))
      ),
      shiny::textAreaInput("new_item_prompt", "Prompt / field description",
                           placeholder = "Extract the invoice ID or reference number.",
                           rows = 2),
      shiny::div(
        class = "d-flex gap-2 mb-2",
        shiny::selectInput("new_item_cast", "Cast type",
          choices = c("character", "numeric", "integer",
                      "date:%d/%m/%Y", "date:%Y-%m-%d"),
          width = "35%"),
        shiny::textInput("new_item_page", "Page(s) (blank = whole file)",
                         placeholder = "1  or  1,3  or  1-5", width = "35%")
      ),
      # LLM-only: provider selector
      shiny::conditionalPanel(
        "input.new_item_backend == 'llm'",
        shiny::selectizeInput("new_item_provider", "Provider",
          choices = c("anthropic", "openai", "google_gemini", "openrouter",
                      "groq", "mistral", "deepseek", "ollama", "openai_compatible"),
          selected = "anthropic",
          options = list(create = TRUE, createOnBlur = TRUE))
      ),
      # GLiNER-only: model selector + all_matches + setup hint
      shiny::conditionalPanel(
        "input.new_item_backend == 'gliner'",
        shiny::div(
          class = "d-flex gap-2 align-items-end",
          shiny::div(class = "flex-fill",
            shiny::selectInput("new_item_gliner_model", "GLiNER model",
              choices = c("Base (205M)"  = "fastino/gliner2-base-v1",
                          "Large (340M)" = "fastino/gliner2-large-v1"),
              selected = "fastino/gliner2-base-v1")),
          shiny::div(class = "pb-2",
            shiny::checkboxInput("new_item_all_matches", "All matches", value = FALSE))
        ),
        shiny::tags$small(class = "text-muted",
          shiny::icon("robot"),
          " Requires ", shiny::tags$code("setup_gliner()"),
          " — run once, then restart R. ",
          shiny::tags$em("All matches"), " returns every occurrence found.")
      ),
      # Area coordinates — LLM only, shown when a page is specified
      shiny::conditionalPanel(
        "input.new_item_backend == 'llm' && /^\\s*\\d+\\s*$/.test(input.new_item_page || '')",
        shiny::div(
          class = "border rounded p-2 mb-2",
          style = "background:var(--raised);",
          shiny::div(
            class = "d-flex justify-content-between align-items-center mb-1",
            shiny::tags$small(class = "fw-semibold", "Area (optional — leave blank for full page)"),
            if (has_brush)
              shiny::actionButton("item_use_brush", "Use drawn area",
                                  class = "btn-sm btn-outline-warning",
                                  icon  = shiny::icon("vector-square"))
            else
              shiny::tags$small(class = "text-muted",
                "Draw a box on the Extract tab PDF to pre-fill these.")
          ),
          shiny::fluidRow(
            shiny::column(3, shiny::numericInput("new_item_top",    "Top",    value = NA, min = 0)),
            shiny::column(3, shiny::numericInput("new_item_left",   "Left",   value = NA, min = 0)),
            shiny::column(3, shiny::numericInput("new_item_bottom", "Bottom", value = NA, min = 0)),
            shiny::column(3, shiny::numericInput("new_item_right",  "Right",  value = NA, min = 0))
          )
        )
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("do_extract_item", "Extract",
                            class = "btn-warning", icon = shiny::icon("tag"))
      )
    ))

    # If a brush area exists, pre-fill the area inputs immediately
    if (has_brush) {
      shiny::updateNumericInput(session, "new_item_top",    value = unname(round(brush_area["top"])))
      shiny::updateNumericInput(session, "new_item_left",   value = unname(round(brush_area["left"])))
      shiny::updateNumericInput(session, "new_item_bottom", value = unname(round(brush_area["bottom"])))
      shiny::updateNumericInput(session, "new_item_right",  value = unname(round(brush_area["right"])))
    }
  })

  # "Use drawn area" button inside the item modal
  shiny::observeEvent(input$item_use_brush, {
    area <- tryCatch(rv_brush(), error = function(e) NULL)
    req(!is.null(area))
    shiny::updateNumericInput(session, "new_item_top",    value = unname(round(area["top"])))
    shiny::updateNumericInput(session, "new_item_left",   value = unname(round(area["left"])))
    shiny::updateNumericInput(session, "new_item_bottom", value = unname(round(area["bottom"])))
    shiny::updateNumericInput(session, "new_item_right",  value = unname(round(area["right"])))
    shiny::updateTextInput(session, "new_item_page", value = as.character(unname(rv$viewer_page)))
    shiny::showNotification("Area filled from brush selection.", type = "message")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$do_extract_item, {
    lbl     <- trimws(input$new_item_label %||% "")
    prompt  <- trimws(input$new_item_prompt %||% "")
    backend <- input$new_item_backend %||% "llm"
    if (nchar(lbl) == 0 || nchar(prompt) == 0) {
      shinyjs::hide("item_spinner")
      shiny::showNotification("Label and prompt are required.", type = "warning")
      return()
    }
    if (is.null(rv$pdf_path)) {
      shinyjs::hide("item_spinner")
      return()
    }
    page_raw <- trimws(input$new_item_page %||% "")
    page_val <- if (backend == "gliner") {
      .parse_page_range(page_raw)
    } else {
      if (nzchar(page_raw)) as.integer(page_raw) else NULL
    }
    cast_val         <- input$new_item_cast %||% "character"
    gliner_model_val <- input$new_item_gliner_model %||% "fastino/gliner2-base-v1"
    provider_val     <- input$new_item_provider %||% "anthropic"

    # Area only applies to LLM + page is set + all four coords filled
    area_val <- if (backend == "llm" && !is.null(page_val) &&
                    !is.na(input$new_item_top)    && !is.na(input$new_item_left) &&
                    !is.na(input$new_item_bottom) && !is.na(input$new_item_right)) {
      c(top    = input$new_item_top,    left   = input$new_item_left,
        bottom = input$new_item_bottom, right  = input$new_item_right)
    } else NULL

    tryCatch({
      tmp <- new.env(parent = emptyenv())
      tmp$path   <- rv$pdf_path
      tmp$tables <- list(); tmp$items <- list(); tmp$steps <- list()
      tmp$.replaying <- TRUE; class(tmp) <- "pdfmacro_session"

      all_matches_val <- isTRUE(input$new_item_all_matches) && backend == "gliner"
      if (backend == "gliner") {
        select_item(tmp, label = lbl, prompt = prompt,
                    cast = cast_val, page = page_val,
                    backend = "gliner", gliner_model = gliner_model_val,
                    all_matches = all_matches_val)
        rv$steps <- .module_record(rv$steps, list(
          step = "select_item", label = lbl, prompt = prompt,
          cast = cast_val, page = page_val,
          backend = "gliner", gliner_model = gliner_model_val,
          all_matches = all_matches_val
        ), session = NULL)
      } else {
        select_item(tmp, label = lbl, prompt = prompt,
                    cast = cast_val, page = page_val, area = area_val,
                    backend = "llm", provider = provider_val, dpi = 120L)
        rv$steps <- .module_record(rv$steps, list(
          step = "select_item", label = lbl, prompt = prompt,
          cast = cast_val, page = page_val, area = area_val,
          backend = "llm", provider = provider_val, dpi = 120L
        ), session = NULL)
      }

      rv$items[[lbl]] <- tmp$items[[lbl]]
      shinyjs::hide("item_spinner")
      shiny::removeModal()
      bslib::nav_select("main_tabs", "Items", session = session)
      shiny::showNotification(
        paste0("'", lbl, "': ", as.character(tmp$items[[lbl]]$value)),
        type = "message")
    }, error = function(e) {
      shinyjs::hide("item_spinner")
      shiny::showNotification(paste("Extraction failed:", e$message), type = "error")
    })
  })

  # ── Batch GLiNER items modal ─────────────────────────────────────────────
  shiny::observeEvent(input$open_batch_items, {
    shiny::showModal(shiny::modalDialog(
      title = "Batch extract items (GLiNER)",
      shiny::p(class = "text-muted small",
        "Enter one field per line as ", shiny::tags$code("label: description"),
        ". All fields are extracted in a single GLiNER model pass."),
      shiny::textAreaInput("batch_items_text",
        label = "Fields (label: description, one per line)",
        placeholder = paste(
          "invoice_no: Invoice ID or reference number",
          "total: Grand total amount payable",
          "date: Invoice date",
          sep = "\n"
        ),
        rows = 6),
      shiny::div(
        class = "d-flex gap-2 mb-2 align-items-end",
        shiny::textInput("batch_items_page", "Page(s) (blank = whole file)",
                         placeholder = "1  or  1,3  or  1-5", width = "35%"),
        shiny::selectInput("batch_items_model", "GLiNER model",
          choices = c("Base (205M)"  = "fastino/gliner2-base-v1",
                      "Large (340M)" = "fastino/gliner2-large-v1"),
          selected = "fastino/gliner2-base-v1", width = "45%"),
        shiny::div(class = "pb-2",
          shiny::checkboxInput("batch_items_all_matches", "All matches", value = FALSE))
      ),
      shiny::tags$small(class = "text-muted",
        shiny::icon("robot"),
        " Requires ", shiny::tags$code("setup_gliner()"), " — run once, restart R. ",
        shiny::tags$em("All matches"), " returns every occurrence per field."),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("do_batch_items", "Extract all",
                            class = "btn-warning", icon = shiny::icon("layer-group"))
      )
    ))
  })

  shiny::observeEvent(input$do_batch_items, {
    raw_text <- trimws(input$batch_items_text %||% "")
    if (nchar(raw_text) == 0) {
      shinyjs::hide("item_spinner")
      shiny::showNotification("Enter at least one label: description line.",
                              type = "warning")
      return()
    }
    if (is.null(rv$pdf_path)) {
      shinyjs::hide("item_spinner")
      return()
    }

    # Parse "label: description" lines
    lines <- Filter(nzchar, trimws(strsplit(raw_text, "\n")[[1]]))
    parsed <- lapply(lines, function(l) {
      colon <- regexpr(":", l, fixed = TRUE)
      if (colon < 2L) return(NULL)
      list(
        label = trimws(substr(l, 1L, colon - 1L)),
        desc  = trimws(substr(l, colon + 1L, nchar(l)))
      )
    })
    parsed <- Filter(Negate(is.null), parsed)
    if (length(parsed) == 0) {
      shinyjs::hide("item_spinner")
      shiny::showNotification(
        "Could not parse any lines. Use \"label: description\" format.",
        type = "warning")
      return()
    }

    items_vec <- setNames(
      vapply(parsed, `[[`, character(1), "desc"),
      vapply(parsed, `[[`, character(1), "label")
    )
    page_val  <- .parse_page_range(input$batch_items_page %||% "")
    model_val       <- input$batch_items_model %||% "fastino/gliner2-base-v1"
    all_matches_val <- isTRUE(input$batch_items_all_matches)

    tryCatch({
      tmp <- new.env(parent = emptyenv())
      tmp$path <- rv$pdf_path
      tmp$tables <- list(); tmp$items <- list(); tmp$steps <- list()
      tmp$.replaying <- TRUE; class(tmp) <- "pdfmacro_session"

      select_items_batch(tmp, items = items_vec, page = page_val,
                         gliner_model = model_val, all_matches = all_matches_val)

      for (lbl in names(tmp$items)) {
        rv$items[[lbl]] <- tmp$items[[lbl]]
      }
      rv$steps <- c(rv$steps, tmp$steps)

      shinyjs::hide("item_spinner")
      shiny::removeModal()
      bslib::nav_select("main_tabs", "Items", session = session)
      shiny::showNotification(
        paste0("Extracted ", length(tmp$items), " item",
               if (length(tmp$items) != 1) "s" else "", " via GLiNER."),
        type = "message")
    }, error = function(e) {
      shinyjs::hide("item_spinner")
      shiny::showNotification(paste("Batch extraction failed:", e$message),
                              type = "error")
    })
  })

  output$items_json_preview <- shiny::renderText({
    if (length(rv$items) == 0 && length(rv$tables) == 0)
      return("# Extract items and tables to preview JSON")
    tmp_sess <- new.env(parent = emptyenv())
    tmp_sess$path   <- rv$pdf_path %||% "unknown"
    tmp_sess$items  <- rv$items
    tmp_sess$tables <- rv$tables
    tmp_sess$steps  <- list()
    class(tmp_sess) <- "pdfmacro_session"
    tryCatch(
      as.character(export_json(tmp_sess, pretty = TRUE)),
      error = function(e) paste("JSON error:", e$message)
    )
  })

  output$dl_json <- shiny::downloadHandler(
    filename = function() paste0(
      tools::file_path_sans_ext(basename(rv$pdf_path %||% "export")), ".json"),
    content = function(file) {
      tmp_sess <- new.env(parent = emptyenv())
      tmp_sess$path   <- rv$pdf_path %||% "unknown"
      tmp_sess$items  <- rv$items
      tmp_sess$tables <- rv$tables
      tmp_sess$steps  <- rv$steps
      class(tmp_sess) <- "pdfmacro_session"
      writeLines(as.character(export_json(tmp_sess, pretty = TRUE)), file)
    }
  )

  # ── Steps panel ───────────────────────────────────────────────────────────
  output$steps_panel <- shiny::renderUI({
    n <- length(rv$steps)
    if (n == 0) return(shiny::p("No steps recorded yet.", class = "text-muted p-3"))
    badges <- lapply(seq_len(n), function(i) {
      s    <- rv$steps[[i]]
      flag <- isTRUE(s$.flagged)
      cls  <- if (flag) "step-badge flagged" else "step-badge"
      shiny::tags$span(class = cls,
        paste0("[", i, "] ", s$step, " / ", s$label %||% s$table %||% "?"))
    })
    shiny::div(class = "p-3", badges)
  })

  output$remove_step_ui <- shiny::renderUI({
    n <- length(rv$steps)
    if (n == 0) return(NULL)
    shiny::div(
      class = "d-flex gap-1 align-items-center",
      shiny::numericInput("remove_step_idx", "Remove step #",
                          value = n, min = 1, max = n, width = "120px"),
      shiny::actionButton("do_remove_step", "Remove",
                          class = "btn-outline-danger btn-sm",
                          icon  = shiny::icon("minus"))
    )
  })

  shiny::observeEvent(input$do_remove_step, {
    idx <- as.integer(input$remove_step_idx)
    n   <- length(rv$steps)
    req(idx >= 1, idx <= n)
    removed      <- rv$steps[[idx]]
    remaining    <- rv$steps[-idx]   # steps after removal

    is_extraction <- isTRUE(removed$step %in% c("select_table", "select_table_llm", "select_table_docling", "stack_pages"))
    lbl           <- removed$label

    # Only wipe the table if no other extraction step for this label remains
    if (is_extraction && !is.null(lbl)) {
      other_extract <- any(vapply(remaining, function(s)
        isTRUE(s$step %in% c("select_table", "select_table_llm", "select_table_docling")) &&
        identical(s$label, lbl), logical(1)))

      rv$steps <- remaining
      if (!other_extract) {
        rv$tables[[lbl]] <- NULL
        if (identical(rv$active_label, lbl)) rv$active_label <- NULL
        shiny::showNotification(
          paste0("Removed step [", idx, "] and cleared table '", lbl, "'."),
          type = "message"
        )
      } else {
        shiny::showNotification(
          paste0("Removed step [", idx, "]: ", removed$step, " / ", lbl,
                 " (table kept — another extraction step remains)."),
          type = "message"
        )
      }
    } else {
      rv$steps <- remaining
      shiny::showNotification(
        paste0("Removed step [", idx, "]: ", removed$step, " / ",
               lbl %||% removed$table %||% "?"),
        type = "message"
      )
    }
  })

  shiny::observeEvent(input$clear_all_steps, {
    rv$steps <- list()
    shiny::showNotification("All steps cleared.", type = "message")
  })

  # ── Macro load ─────────────────────────────────────────────────────────────
  shiny::observeEvent(input$load_macro_file, {
    req(input$load_macro_file)
    tryCatch({
      m <- yaml::read_yaml(input$load_macro_file$datapath)
      rv$steps <- m$steps
      shiny::showNotification(
        paste0("Macro loaded: ", m$macro$name, " (", length(m$steps), " steps)"),
        type = "message"
      )
    }, error = function(e) {
      shiny::showNotification(paste("Load failed:", e$message), type = "error")
    })
  })

  output$macro_status <- shiny::renderUI({
    req(input$load_macro_file)
    shiny::tags$small(class = "text-muted",
      shiny::icon("check-circle"), " ", input$load_macro_file$name,
      " \u00b7 ", length(rv$steps), " steps"
    )
  })

  # ── Export — table selector ────────────────────────────────────────────────
  output$export_table_sel <- shiny::renderUI({
    req(length(rv$tables) > 0)
    shiny::selectInput("export_tbl_sel", "Table to export",
                       choices = names(rv$tables),
                       selected = rv$active_label %||% names(rv$tables)[[1]])
  })

  # ── Export — downloads ────────────────────────────────────────────────────
  output$dl_csv <- shiny::downloadHandler(
    filename = function() paste0(input$export_tbl_sel %||% "table", ".csv"),
    content  = function(file) {
      lbl <- input$export_tbl_sel; req(lbl %in% names(rv$tables))
      utils::write.csv(rv$tables[[lbl]], file, row.names = FALSE)
    }
  )

  output$dl_xlsx <- shiny::downloadHandler(
    filename = function() paste0(input$export_tbl_sel %||% "table", ".xlsx"),
    content  = function(file) {
      req(requireNamespace("openxlsx", quietly = TRUE) ||
          requireNamespace("writexl",  quietly = TRUE))
      lbl <- input$export_tbl_sel; req(lbl %in% names(rv$tables))
      if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(rv$tables[[lbl]], file)
      } else {
        openxlsx::write.xlsx(rv$tables[[lbl]], file)
      }
    }
  )

  output$dl_all_xlsx <- shiny::downloadHandler(
    filename = function() "pdfmacro_tables.xlsx",
    content  = function(file) {
      req(length(rv$tables) > 0)
      if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(rv$tables, file)
      } else if (requireNamespace("openxlsx", quietly = TRUE)) {
        wb <- openxlsx::createWorkbook()
        for (nm in names(rv$tables)) {
          openxlsx::addWorksheet(wb, nm)
          openxlsx::writeData(wb, nm, rv$tables[[nm]])
        }
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        stop("Install writexl or openxlsx for Excel export.")
      }
    }
  )

  output$dl_all_csv_zip <- shiny::downloadHandler(
    filename = function() "pdfmacro_tables.zip",
    content  = function(file) {
      req(length(rv$tables) > 0)
      tmp_dir <- tempfile(); dir.create(tmp_dir)
      csv_files <- vapply(names(rv$tables), function(nm) {
        p <- file.path(tmp_dir, paste0(nm, ".csv"))
        utils::write.csv(rv$tables[[nm]], p, row.names = FALSE)
        p
      }, character(1))
      zip::zip(file, files = csv_files, mode = "cherry-pick")
    }
  )

  # ── Export — macro YAML preview + download ────────────────────────────────
  # Update the shinyAce YAML editor whenever steps change
  shiny::observe({
    clean <- lapply(rv$steps, function(s) s[!grepl("^\\.", names(s))])
    txt <- if (length(clean) == 0L) {
      "# No steps recorded yet."
    } else {
      yaml::as.yaml(list(
        macro = list(name    = input$macro_name_export %||% "my_macro",
                     created = format(Sys.time(), "%Y-%m-%d %H:%M"),
                     source  = if (!is.null(rv$pdf_path)) basename(rv$pdf_path) else "unknown",
                     n_steps = length(clean)),
        steps = clean
      ))
    }
    shinyAce::updateAceEditor(session, "macro_yaml_editor", value = txt)
  })

  # "Apply edits" — parse editor YAML back into rv$steps
  shiny::observeEvent(input$yaml_apply_edits, {
    txt <- input$macro_yaml_editor
    tryCatch({
      parsed <- yaml::read_yaml(text = txt)
      steps  <- parsed$steps
      if (!is.null(steps) && length(steps) > 0L) {
        rv$steps <- steps
        shiny::showNotification(
          paste0("Applied ", length(steps), " step(s) from YAML editor."),
          type = "message"
        )
      } else {
        shiny::showNotification("No steps found in YAML.", type = "warning")
      }
    }, error = function(e) {
      shiny::showNotification(paste("YAML parse error:", e$message), type = "error")
    })
  })

  output$dl_macro_yml <- shiny::downloadHandler(
    filename = function() paste0(input$macro_name_export %||% "my_macro", ".yml"),
    content  = function(file) {
      req(length(rv$steps) > 0)
      clean <- lapply(rv$steps, function(s) s[!grepl("^\\.", names(s))])
      macro <- list(
        macro = list(name    = input$macro_name_export %||% "my_macro",
                     created = format(Sys.time(), "%Y-%m-%d %H:%M"),
                     source  = if (!is.null(rv$pdf_path)) basename(rv$pdf_path) else "unknown",
                     n_steps = length(clean)),
        steps = clean
      )
      yaml::write_yaml(macro, file)
    }
  )

  shiny::observeEvent(input$save_macro_disk, {
    req(length(rv$steps) > 0)
    name <- trimws(input$macro_name_export %||% "my_macro")
    dir  <- trimws(input$macro_dir_export  %||% ".")
    tryCatch({
      clean <- lapply(rv$steps, function(s) s[!grepl("^\\.", names(s))])
      out   <- file.path(dir, paste0(name, ".yml"))
      yaml::write_yaml(list(
        macro = list(name = name, created = format(Sys.time(), "%Y-%m-%d %H:%M"),
                     source = if (!is.null(rv$pdf_path)) basename(rv$pdf_path) else "unknown",
                     n_steps = length(clean)),
        steps = clean
      ), out)
      shiny::showNotification(paste0("Saved: ", out), type = "message")
    }, error = function(e) {
      shiny::showNotification(paste("Save failed:", e$message), type = "error")
    })
  })

# --------------------------------------------------------------------------- #
#  Replay + Batch server logic                                                 #
# --------------------------------------------------------------------------- #

  .sf_roots_rb <- .make_sf_roots()

  shinyFiles::shinyFileChoose(input, "replay_macro_file",
    roots = .sf_roots_rb, filetypes = list(YAML = c("yml", "yaml")), session = session)
  shinyFiles::shinyFileChoose(input, "replay_pdf_file",
    roots = .sf_roots_rb, filetypes = list(PDF = "pdf"), session = session)
  shinyFiles::shinyFileChoose(input, "batch_macro_file",
    roots = .sf_roots_rb, filetypes = list(YAML = c("yml", "yaml")), session = session)
  shinyFiles::shinyFileChoose(input, "batch_pdf_files",
    roots = .sf_roots_rb, filetypes = list(PDF = "pdf"), session = session)

  rv_replay <- shiny::reactiveValues(macro_path=NULL, pdf_path=NULL, log="No replay run yet.")
  rv_batch  <- shiny::reactiveValues(macro_path=NULL, pdf_paths=NULL, results=NULL)

  shiny::observeEvent(input$replay_macro_file, {
    req(!is.integer(input$replay_macro_file))
    i <- shinyFiles::parseFilePaths(.sf_roots_rb, input$replay_macro_file)
    req(nrow(i) > 0); rv_replay$macro_path <- normalizePath(as.character(i$datapath[[1]]))
    .close_sf_modal()
  })
  shiny::observeEvent(input$replay_pdf_file, {
    req(!is.integer(input$replay_pdf_file))
    i <- shinyFiles::parseFilePaths(.sf_roots_rb, input$replay_pdf_file)
    req(nrow(i) > 0); rv_replay$pdf_path <- normalizePath(as.character(i$datapath[[1]]))
    .close_sf_modal()
  })
  shiny::observeEvent(input$batch_macro_file, {
    req(!is.integer(input$batch_macro_file))
    i <- shinyFiles::parseFilePaths(.sf_roots_rb, input$batch_macro_file)
    req(nrow(i) > 0); rv_batch$macro_path <- normalizePath(as.character(i$datapath[[1]]))
    .close_sf_modal()
  })
  shiny::observeEvent(input$batch_pdf_files, {
    req(!is.integer(input$batch_pdf_files))
    i <- shinyFiles::parseFilePaths(.sf_roots_rb, input$batch_pdf_files)
    req(nrow(i) > 0); rv_batch$pdf_paths <- normalizePath(as.character(i$datapath))
    .close_sf_modal()
  })

  output$replay_macro_status <- shiny::renderUI({
    req(rv_replay$macro_path)
    shiny::tags$small(class="text-success", shiny::icon("check"), " ", basename(rv_replay$macro_path))
  })
  output$replay_pdf_status <- shiny::renderUI({
    req(rv_replay$pdf_path)
    shiny::tags$small(class="text-success", shiny::icon("check"), " ", basename(rv_replay$pdf_path))
  })
  output$batch_macro_status <- shiny::renderUI({
    req(rv_batch$macro_path)
    shiny::tags$small(class="text-success", shiny::icon("check"), " ", basename(rv_batch$macro_path))
  })
  output$batch_pdf_status <- shiny::renderUI({
    req(rv_batch$pdf_paths)
    shiny::tags$small(class="text-success", shiny::icon("check"), " ", length(rv_batch$pdf_paths), " file(s)")
  })

  shiny::observeEvent(input$do_replay, {
    req(rv_replay$macro_path, rv_replay$pdf_path)
    lines <- c(paste0("Macro: ", basename(rv_replay$macro_path)),
               paste0("File:  ", basename(rv_replay$pdf_path)), "")
    result <- tryCatch({
      steps  <- load_macro(rv_replay$macro_path)
      tables <- pdf_replay(rv_replay$pdf_path, steps)
      for (nm in names(tables)) rv$tables[[nm]] <- tables[[nm]]
      c(lines,
        paste0("✔ ", length(tables), " table(s) loaded into Tables tab."),
        paste(paste0("  ", names(tables), ": ",
                     vapply(tables, nrow, integer(1)), " rows"), collapse="
"))
    }, error = function(e) c(lines, paste0("✗ Error: ", e$message)))
    rv_replay$log <- paste(result, collapse="
")
    bslib::nav_select("main_tabs", "Tables", session=session)
  })
  output$replay_log <- shiny::renderText(rv_replay$log)

  shiny::observeEvent(input$do_batch, {
    req(rv_batch$macro_path, rv_batch$pdf_paths)
    shiny::showNotification(paste0("Batch: ", length(rv_batch$pdf_paths), " files..."), type="message")
    tryCatch({
      steps <- load_macro(rv_batch$macro_path)
      rv_batch$results <- pdf_replay_batch(rv_batch$pdf_paths, steps)
      shiny::showNotification("Batch complete.", type="message")
    }, error = function(e) shiny::showNotification(e$message, type="error"))
  })

  output$batch_results_ui <- shiny::renderUI({
    res <- rv_batch$results; req(!is.null(res))
    shiny::div(lapply(names(res), function(nm) {
      tbls <- res[[nm]]
      if (is.null(tbls)) shiny::div(class="text-danger", shiny::icon("xmark"), " ", nm, " failed")
      else shiny::div(class="text-success mb-1", shiny::icon("check"), " ", nm, " — ",
             paste(names(tbls), vapply(tbls, nrow, integer(1)), sep="=", collapse=", "))
    }))
  })

  output$dl_batch_xlsx <- shiny::downloadHandler(
    filename = function() "batch_results.xlsx",
    content  = function(file) {
      res <- rv_batch$results; req(!is.null(res))
      all_tbls <- unlist(lapply(names(res), function(f) {
        if (is.null(res[[f]])) return(list())
        setNames(res[[f]], paste0(tools::file_path_sans_ext(f), "_", names(res[[f]])))
      }), recursive = FALSE)
      if (requireNamespace("writexl", quietly=TRUE)) writexl::write_xlsx(all_tbls, file)
    }
  )
  output$dl_batch_zip <- shiny::downloadHandler(
    filename = function() "batch_results.zip",
    content  = function(file) {
      res <- rv_batch$results; req(!is.null(res))
      tmp <- tempfile(); dir.create(tmp)
      for (f in names(res)) {
        tbls <- res[[f]]; if (is.null(tbls)) next
        sub  <- file.path(tmp, tools::file_path_sans_ext(f)); dir.create(sub, showWarnings=FALSE)
        for (nm in names(tbls))
          utils::write.csv(tbls[[nm]], file.path(sub, paste0(nm,".csv")), row.names=FALSE)
      }
      zip::zip(file, files=list.files(tmp, recursive=TRUE, full.names=TRUE), mode="cherry-pick")
    }
  )


}  # end .pdf_app_server

# --------------------------------------------------------------------------- #
#  Filter expression builder                                                   #
# --------------------------------------------------------------------------- #

.filter_op_choices <- c(
  "equals"            = "==",
  "not equals"        = "!=",
  "greater than"      = ">",
  "greater/equal"     = ">=",
  "less than"         = "<",
  "less/equal"        = "<=",
  "contains"          = "grepl",
  "does not contain"  = "!grepl",
  "is blank / NA"     = "is.na",
  "is not blank / NA" = "!is.na"
)

.build_filter_expr <- function(col, op, val) {
  col_r <- paste0("`", col, "`")
  num_v <- suppressWarnings(as.numeric(val))
  val_r <- if (!is.na(num_v)) as.character(num_v) else paste0("'", val, "'")
  switch(op,
    "==" = paste0(col_r, " == ", val_r),
    "!=" = paste0(col_r, " != ", val_r),
    ">"  = paste0(col_r, " > ",  val_r),
    ">=" = paste0(col_r, " >= ", val_r),
    "<"  = paste0(col_r, " < ",  val_r),
    "<=" = paste0(col_r, " <= ", val_r),
    "grepl"  = paste0("grepl(",  val_r, ", ", col_r, ", ignore.case = TRUE)"),
    "!grepl" = paste0("!grepl(", val_r, ", ", col_r, ", ignore.case = TRUE)"),
    "is.na"  = paste0("is.na(",  col_r, ")"),
    "!is.na" = paste0("!is.na(", col_r, ")"),
    paste0(col_r, " == ", val_r)
  )
}

# --------------------------------------------------------------------------- #
#  Extraction step deduplication helper                                        #
# --------------------------------------------------------------------------- #

# When the user re-extracts a table with the same label, replace the existing
# extraction step in-place rather than appending a duplicate.
# Downstream transform steps (rename, cast, filter etc.) are preserved so they
# still apply to the refreshed data on next replay.
.parse_page_range <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(as.character(txt)))) return(NULL)
  txt   <- gsub(" ", "", as.character(txt))
  parts <- strsplit(txt, ",")[[1L]]
  pages <- integer(0)
  for (p in parts) {
    if (grepl("^\\d+-\\d+$", p)) {
      bounds <- as.integer(strsplit(p, "-")[[1L]])
      pages  <- c(pages, seq(bounds[[1L]], bounds[[2L]]))
    } else {
      v <- suppressWarnings(as.integer(p))
      if (!is.na(v)) pages <- c(pages, v)
    }
  }
  pages <- sort(unique(pages[pages >= 1L]))
  if (length(pages) == 0L) NULL else pages
}

.flatten_scan <- function(res) {
  items <- list()
  for (pg_chr in names(res)) {
    pg   <- as.integer(pg_chr)
    tbls <- res[[pg_chr]]
    for (j in seq_along(tbls)) {
      items[[length(items) + 1L]] <- list(page = pg, index = j, df = tbls[[j]])
    }
  }
  items
}

.parse_struct_fields <- function(text) {
  # Parses lines of the form:
  #   name: description                     → str field
  #   name [list]: description              → list field
  #   name [basic|premium]: description     → enum field (choices = [basic|premium])
  # Returns list(fields = c(name=desc), list_fields = c(...), enum_fields = c(name="[v1|v2]"))
  lines <- strsplit(trimws(text), "\n")[[1L]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  fields      <- character(0)
  list_fields <- character(0)
  enum_fields <- character(0)

  for (line in lines) {
    colon <- regexpr(":", line, fixed = TRUE)
    if (colon < 2L) next
    raw_name   <- trimws(substr(line, 1L, colon - 1L))
    field_desc <- trimws(substr(line, colon + 1L, nchar(line)))
    if (!nzchar(raw_name) || !nzchar(field_desc)) next

    # Detect bracket annotation: "name [list]" or "name [v1|v2]"
    bracket <- regmatches(raw_name, regexpr("\\[([^\\]]+)\\]", raw_name, perl = TRUE))
    if (length(bracket) == 1L) {
      field_name <- trimws(sub("\\s*\\[[^\\]]+\\]", "", raw_name, perl = TRUE))
      spec <- gsub("\\[|\\]", "", bracket)
      if (identical(tolower(spec), "list")) {
        list_fields <- c(list_fields, field_name)
      } else {
        enum_fields[[field_name]] <- paste0("[", spec, "]")
      }
    } else {
      field_name <- raw_name
    }
    if (!nzchar(field_name)) next
    fields[[field_name]] <- field_desc
  }
  list(fields = fields, list_fields = list_fields, enum_fields = enum_fields)
}

.replace_or_append_extraction <- function(steps, label, new_step) {
  extract_types <- c("select_table", "select_table_llm", "select_table_docling",
                     "select_struct")
  idx <- which(vapply(steps, function(s)
    isTRUE(s$step %in% extract_types) && identical(s$label, label),
    logical(1)))

  if (length(idx) > 0L) {
    # Replace the FIRST matching step; drop any additional duplicates
    steps[[idx[[1L]]]] <- new_step
    if (length(idx) > 1L) steps <- steps[-idx[-1L]]
  } else {
    steps <- .module_record(steps, new_step, session = NULL)
  }
  steps
}
