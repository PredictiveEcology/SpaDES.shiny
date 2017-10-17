#' Visualize Raster Module UI Function
#'
#' @description Shiny module which visualizes raster.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @param ... Additional parameters passed to \code{link[shiny]{plotOutput}}.
#'
#' @return None. Invoked for the side-effect of generating UI for plot.
#'
#' @export
#' @importFrom shiny NS plotOutput
visualizeRasterUI <- function(id, ...) {
  ns <- NS(id)

  plotOutput(ns("rasterVisualization"), ...)
}

#' Visualize Raster Module Server Function
#'
#' @param input Shiny server input object.
#'
#' @param output Shiny server output object.
#'
#' @param session Shiny server session object.
#'
#' @param raster Raster which should be visualized. Might be a reactive value
#'               containing raster.
#'
#' @param ... Additional parameters passed to \code{link[rasterVis]{levelplot}}.
#'
#' @return None. Invoked for the side-effect of rendering bar plot.
#'
#' @export
#' @importFrom rasterVis levelplot
#' @importFrom shiny renderPlot
visualizeRaster <- function(input, output, session, raster, ...) {
  output$rasterVisualization <- renderPlot({
    if (is.reactive(raster)) {
      raster <- raster()
    }
    levelplot(raster, ...)
  })
}
