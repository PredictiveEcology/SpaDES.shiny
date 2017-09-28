#' Histogram Module
#'
#' @description Shiny module which creates a histogram using barplot based on the data received
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#'
#' @param ... Additional parameters passed to \code{link[shiny]{plotOutput}}
#'
#' @return None. Invoked for the side-effect of generating UI for plot
#'
#' @export
#' @importFrom  shiny plotOutput NS
#' @rdname histogram
histogramUI <- function(id, ...) {
  ns <- NS(id)

  plotOutput(ns("histogram"), ...)
}

#' @param input Shiny server input object
#'
#' @param output Shiny server output object
#'
#' @param session Shiny server session object
#'
#' @param data Data used to render bar plot. See \code{\link[graphics]{barplot}} for reference.
#'
#' @return None. Invoked for the side-effect of rendering bar plot.
#'
#' @export
#' @importFrom graphics barplot
#' @importFrom shiny renderPlot
#' @rdname histogram
histogram <- function(input, output, session, data) {
  output$histogram <- renderPlot(
    barplot(data)
  )
}
