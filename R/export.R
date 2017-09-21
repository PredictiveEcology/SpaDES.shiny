#' Export CSV module
#'
#' Proof-of-concept
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#'
#' @importFrom shiny actionButton NS
#' @rdname exportCsv
exportCsvUI <- function(id) {
  ns <- NS(id)

  actionButton(ns("export_button"), "Export CSV")
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#'
#' @importFrom shiny observeEvent
#' @rdname exportCsv
exportCsv <- function(input, output, session) {
  observeEvent(input$export_button, {
    cat("File was exported.")
  })
}
