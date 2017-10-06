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
#' @param data Subset of \code{globalData} created during slicing.
#'             See \code{?slicer} for reference.
#'
#' @param globalData Data in form of data table. This data was sliced during slicing.
#'                   See \code{?slicer} for reference.
#'
#' @param chosenCategories List with categories that where chosen when slicing the data.
#'
#' @param numberOfSimulationTimes How many simulation times occurred in simulation.
#'
#' @return None. Invoked for the side-effect of rendering bar plot.
#'
#' @export
#' @importFrom graphics barplot
#' @importFrom shiny renderPlot
#' @rdname histogram
histogram <- function(input, output, session, data, globalData,
                      chosenCategories, numberOfSimulationTimes) {
  output$histogram <- renderPlot({
    maxNumClusters <- globalData[ageClass==chosenCategories[[3]] & polygonID==chosenCategories[[2]],
                                 .N, by = c("vegCover","rep")]$N + 1
    maxNumClusters <- if(length(maxNumClusters)==0) 6 else pmax(6, max(maxNumClusters))
    numberOfPatchesInTime <- rep(0, numberOfSimulationTimes)
    if(NROW(data)) {
      numberOfPatchesByTime <- data[,.N,by="rep"]
      numberOfPatchesInTime[seq_len(NROW(numberOfPatchesByTime))] <- numberOfPatchesByTime$N
    }
    breaksLabels <- 0:(maxNumClusters)
    breaks <- breaksLabels - 0.5
    barplotBreaks <- breaksLabels + 0.5

    actualPlot <- hist(numberOfPatchesInTime,
                       breaks = breaks)
    barplot(actualPlot$counts/sum(actualPlot$counts),
            xlim = range(breaks),
            xlab="", ylab = "Proportion in NRV",
            col="darkgrey",border="grey", main = "",
            width = rep(1, length(numberOfPatchesInTime)), space = 0)
    axis(1, at = barplotBreaks, labels = breaksLabels)
  })
}
