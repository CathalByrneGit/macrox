library(shiny)
library(bslib)
library(macrox)

# Standalone demo app showing the macrox Shiny module.
# Run with: shiny::runApp("inst/examples/demo_app.R")

ui <- page_navbar(
  title = "macrox demo",
  theme = bs_theme(version = 5),
  nav_panel("Extract",   macrox_ui("pdf1")),
  nav_panel("Extracted", verbatimTextOutput("summary"))
)

server <- function(input, output, session) {
  result <- macrox_server("pdf1")

  output$summary <- renderText({
    tbls <- result$tables()
    if (!length(tbls)) return("No tables extracted yet.\n\nUpload a PDF and extract some tables first.")
    lines <- vapply(names(tbls), function(nm) {
      df <- tbls[[nm]]
      paste0(nm, "  [", nrow(df), " x ", ncol(df), "]  cols: ",
             paste(names(df), collapse = ", "))
    }, character(1))
    paste(lines, collapse = "\n")
  })
}

shinyApp(ui, server)


# ── Massey Ferguson integration example ──────────────────────────────────────
#
# UI (inside an accordion):
#
#   bslib::accordion_panel(
#     "PDF Import",
#     icon = shiny::icon("file-pdf"),
#     macrox::macrox_ui(ns("pdf_import"), title = "PDF \u00b7 Table Extractor")
#   )
#
#
# Server:
#
#   pdf_result <- macrox::macrox_server("pdf_import")
#
#   observe({
#     pdf_tables <- pdf_result$tables()
#     req(length(pdf_tables) > 0)
#
#     for (nm in names(pdf_tables)) {
#       validated <- your_validate_fn(pdf_tables[[nm]], table_name = nm)
#       your_upload_fn(validated, target = nm)
#     }
#   })
