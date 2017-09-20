#' Export CSV module
#'
#' Proof-of-concept
#'
#' @importFrom shiny actionButton NS
#' @rdname exportCsv
exportCsvUI <- function(id) {
  ns <- NS(id)

  actionButton(ns("export_button"), "Export CSV")
}

#' @importFrom shiny observeEvent
#' @rdname exportCsv
exportCsv <- function(input, output, session) {
  observeEvent(input$export_button, {
    cat("File was exported.")
  })
}
