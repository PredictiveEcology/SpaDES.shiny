#' Display data.table (shiny module)
#'
#' Display tabular data.
#' Both \code{data.table} and \code{data.frame} can be used as arguments.
#'
#' @template id
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

#' @template input
#' @template output
#' @template session
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
