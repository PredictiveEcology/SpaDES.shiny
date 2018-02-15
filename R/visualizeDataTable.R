#' Display Data Table Module
#'
#' @description Shiny module which displays a data in table format.
#'              Both \code{data.table} and \code{data.frame} can be used as arguments
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @param ... Additional parameters passed to \code{link[DT]{dataTableOutput}}
#'
#' @return None. Invoked for the side-effect of generating UI for plot
#'
#' @export
#' @importFrom shiny NS
#' @importFrom DT dataTableOutput
#' @rdname visualizeDataTable
visualizeDataTableUI <- function(id, ...) {
  ns <- NS(id)

  dataTableOutput(ns("dataTable"), ...)
}

#' @param input Shiny server input object
#'
#' @param output Shiny server output object
#'
#' @param session Shiny server session object
#'
#' @param data Data table which should be displayed.
#'
#' @return None. Invoked for the side-effect of rendering bar plot.
#'
#' @export
#' @importFrom shiny renderPlot is.reactive
#' @rdname visualizeDataTable
visualizeDataTable <- function(input, output, session, data) {
  output$dataTable <- renderDataTable({
    if (is.reactive(data)) {
      data <- data()
    }
    data
  })
}
