#' Visualize raster (shiny module)
#'
#' Display a level plot for a raster.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @param ... Additional UI parameters passed to \code{link[shiny]{plotOutput}},
#'            or server parameters passed to \code{link[rasterVis]{levelplot}}.
#'
#' @return None. Invoked for the side-effect of rendering bar plot.
#'
#' @export
#' @importFrom shiny NS plotOutput
#' @rdname visualizeRaster
visualizeRasterUI <- function(id, ...) {
  ns <- NS(id)

  plotOutput(ns("rasterVisualization"), ...)
}

#' @param input Shiny server input object.
#'
#' @param output Shiny server output object.
#'
#' @param session Shiny server session object.
#'
#' @param raster Raster which should be visualized.
#'               Might be a reactive value containing raster.
#'
#' @export
#' @importFrom rasterVis levelplot
#' @importFrom shiny is.reactive renderPlot
#' @rdname visualizeRaster
visualizeRaster <- function(input, output, session, raster, ...) {
  output$rasterVisualization <- renderPlot({
    if (is.reactive(raster)) {
      raster <- raster()
    }
    rasterVis::levelplot(raster, ...)
  })
}
