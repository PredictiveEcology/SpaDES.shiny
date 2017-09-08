exportCsvUI <- function(id) {
  ns <- NS(id)
  
  actionButton(ns("export_button"), "Export CSV")
}

exportCsv <- function(input, output, session) {
  observeEvent(input$export_button, {
    cat("File was exported.")
  })
}