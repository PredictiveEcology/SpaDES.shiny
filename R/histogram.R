#' Histogram Module UI Function
#'
#' @description UI function of a shiny module which creates a histogram
#'              using barplot based on the data received
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
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
#' @param histdata  Reactive value containing a numeric vector of proportions corresponding
#'                  to each histogram bin (i.e., the output of \code{hist(..., plot = FALSE)}
#'                  represented as a proportion).
#'                  Desired subtables can be retrieved using \code{chosenCategories}
#'                  and \code{chosenValues} parameters.
#'
#' @param addAxisParams Reactive value with parameters to \code{\link[graphics]{axis}}.
#'                      If \code{NULL} (default) then no axis is drawn.
#'
#' @param verticalBar  Numeric value at which to add an \code{abline} to the histogram.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom graphics abline axis barplot
#' @importFrom shiny renderPlot
#' @importFrom utils head
#' @rdname histogram
histogram <- function(input, output, session, histdata, addAxisParams = NULL,
                      verticalBar = NULL, ...) {
  output$histogram <- renderPlot({
    if (is.reactive(histdata)) {
      hst <- histdata()
    } else {
      hst <- histdata
    }
    assertthat::assert_that(is.numeric(hst))

    barplot(hst, ...)

    if (!is.null(addAxisParams)) {
      axps <- if (is.reactive(addAxisParams)) addAxisParams() else addAxisParams
      do.call(axis, axps)
    }
    if (!is.null(verticalBar)) {
      abline(v = verticalBar, col = "red", lwd = 3)
    }
  })
}
