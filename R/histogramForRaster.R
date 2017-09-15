#' Histogram for Raster Shiny Module
#'
#' @description This creates UI for shiny module creating histogram of data contained in raste
#'
#' @param id An ID string that corresponds with the ID used to call the module server function
#'
#' @export
plotRasterSummaryFunctionUI <- function(id) {
  ns <- NS(id)

  box(width = 4, solidHeader = TRUE, collapsible = TRUE,
      h4(paste("Current time since distribution distribution")),
      withSpinner(plotOutput(ns("histogram"), height = 600))
  )
}

#' Histogram for Raster Shiny Module
#'
#' @description
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny server session object
#' @param raster Reactive value containing raster
#'
#' @export
plotRasterSummaryFunction <- function(input, output, session, raster, plotFunction) {
  output$histogram <- renderPlot({
    numberOfBreaks <- ceiling(maxValue(raster())/10)

    plotFunction(raster())
  })
}
