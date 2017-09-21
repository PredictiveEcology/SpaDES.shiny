#' Histogram for Raster Shiny Module
#'
#' @description This creates UI for shiny module creating histogram of data contained in raste
#'
#' @param id An ID string that corresponds with the ID used to call the module server function
#' @param title Optional title for the histogram. Any shiny tag can be used.
#' @param ... Additional parameters passed to \code{\link[shiny]{box}} tag from \pkg{shiny}.
#'
#' @importFrom shiny NS plotOutput
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#'
#' @rdname histogramForRaster
#'
#' @export
histogramForRasterUI <- function(id, title = "", ...) {
  ns <- NS(id)

  box(title, shinycssloaders::withSpinner(plotOutput(ns("histogram"), height = 600)), ...)
}

#' Histogram for Raster Shiny Module
#'
#' @description
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny server session object
#' @param raster Reactive value containing raster
#' @param scale Number used for scaling heights of histogram bars. When set to 1 (default) histogram bar height represents
#' amount of raster cells with value from bar interval. If the resolution of raster is known, scale parameter can
#' be used to transform these heights into the ones representing area covered by cells.
#' @param histogramBreaks Reactive value which is responsible for \code{breaks} parameter as in \code{hist} function from \pkg{graphics} package.
#' See \code{\link[graphics]{hist}} for reference.
#' @param addAxisParams Reactive value with parameters to \code{axis} function from \pkg{graphics} package. See \code{\link[graphics]{axis}} for reference. If \code{NULL}
#' (default) then no axis is drawn.
#' @param ... Additional graphic parameters to \code{barplot} function from \pkg{graphics} package. See \code{\link[graphics]{barplot}} for reference.
#'
#' @importFrom shiny renderPlot
#' @importFrom raster hist
#' @importFrom graphics axis barplot
#'
#' @rdname histogramForRaster
#'
#' @export
histogramForRaster <- function(input, output, session, raster, scale = 1, histogramBreaks, addAxisParams = NULL,  ...) {
  output$histogram <- renderPlot({
    histogram <- raster::hist(raster(), plot = FALSE, breaks = histogramBreaks())

    barplot(histogram$counts*scale, ...)

    if(!is.null(addAxisParams)) {
      do.call(axis, addAxisParams())
    }
  })
}
