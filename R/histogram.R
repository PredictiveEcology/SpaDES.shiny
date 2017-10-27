#' Histogram Module UI Function
#'
#' @description UI function of a shiny module which creates a histogram
#'              using barplot based on the data received
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#'
#' @param ... Additional UI parameters passed to \code{link[shiny]{plotOutput}},
#'            or additional server arguments passed to \code{\link[graphics]{barplot}}.
#'
#' @return None. Invoked for the side-effect of rendering bar plot.
#'
#' @export
#' @importFrom  shiny plotOutput NS
#' @rdname histogram
histogramUI <- function(id, ...) {
  ns <- NS(id)

  plotOutput(ns("histogram"), ...)
}

#' Histogram Module Server Function
#'
#' @description Server function of a shiny module which creates a histogram
#'              using barplot based on the data received.
#'
#' @param input Shiny server input object
#'
#' @param output Shiny server output object
#'
#' @param session Shiny server session object
#'
#' @param data Reactive value containing a data table. This is global data.
#'             Desired subtables can be retrieved using \code{chosenCategories}
#'             and \code{chosenValues} parameters.
#'
#' @param addAxisParams Reactive value with parameters to \code{\link[graphics]{axis}}.
#'                      If \code{NULL} (default) then no axis is drawn.
#'
#' @export
#' @importFrom graphics axis barplot
#' @importFrom shiny renderPlot
#' @importFrom utils head
#' @rdname histogram
histogram <- function(input, output, session, data, addAxisParams = NULL, ...) {
  output$histogram <- renderPlot({
    if (is.reactive(data)) {
      data <- data()
    }

    barplot(data, ...)

    if (!is.null(addAxisParams)) {
      do.call(axis, addAxisParams())
    }
  })
}
