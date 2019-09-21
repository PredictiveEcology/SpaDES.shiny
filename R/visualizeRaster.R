#' Visualize raster (shiny module)
#'
#' Display a level plot for a raster.
#'
#' @template id
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

#' @template input
#'
#' @template output
#'
#' @template session
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
