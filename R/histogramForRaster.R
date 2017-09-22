#' Histogram for Raster Shiny Module
#'
#' @description A shiny module creating histogram of data contained in a raster.
#'
#' @param id An ID string that corresponds with the ID used to call the module server function.
#'
#' @param title Optional title for the histogram. Any shiny tag can be used.
#'
#' @param plotParameters A list of parameters passed to \code{\link[shiny]{plotOutput}}.
#'
#' @param ... Additional parameters passed to \code{\link[shinydashboard]{box}}
#'            in \code{histogramForRasterUI}, and \code{\link[graphics]{barplot}}
#'            in \code{histogramForRaster}.
#'
#' @return None. Invoked for the side-effect of creating shiny UI.
#'
#' @export
#' @importFrom shiny NS plotOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinydashboard box
#' @rdname histogramForRaster
histogramForRasterUI <- function(id, title = "", plotParameters, ...) {
  ns <- NS(id)

  plotParameters["outputId"] <- NULL
  plotOutputParameters <- c(ns("histogram"), plotParameters)

  box(title,
      shinycssloaders::withSpinner(
        do.call(plotOutput, plotOutputParameters)
      ),
      ...)
}

#' Histogram for Raster Shiny Module
#'
#' @param input Shiny server input object
#'
#' @param output Shiny server output object
#'
#' @param session Shiny server session object
#'
#' @param raster Reactive value containing raster
#'
#' @param scale Number used for scaling heights of histogram bars.
#'              When set to 1 (default) histogram bar height represents the number
#'              of raster cells with value from bar interval.
#'              If the resolution of raster is known, \code{scale} can be used
#'              to transform these heights into the ones representing area covered
#'              by cells (count).
#'              When set to 2 (or, more generally, some number n with no further meaning)
#'              this will just increase the height of each histogram bar by 2
#'              (n, respectively).
#'              So, in this scenario, each histogram's bar height is just count
#'              times 2 (count times n).
#'
#' @param histogramBreaks Reactive value which is responsible for \code{breaks}
#'                        parameter, as in \code{\link[graphics]{hist}}.
#'
#' @param addAxisParams Reactive value with parameters to \code{\link[graphics]{axis}}.
#'                      If \code{NULL} (default) then no axis is drawn.
#'
#' @return None. Invoked for the side-effect of rendering histogram plot.
#'
#' @export
#' @importFrom graphics axis barplot
#' @importFrom shiny renderPlot
#' @importFrom raster hist
#' @rdname histogramForRaster
histogramForRaster <- function(input, output, session, raster, histogramBreaks,
                               scale = 1, addAxisParams = NULL,  ...) {
  output$histogram <- renderPlot({
    histogram <- raster::hist(raster(), plot = FALSE, breaks = histogramBreaks())

    barplot(histogram$counts * scale, ...)

    if (!is.null(addAxisParams)) {
      do.call(axis, addAxisParams())
    }
  })
}
