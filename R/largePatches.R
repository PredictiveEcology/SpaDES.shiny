if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".N"))
}

#' Large patches (shiny module)
#'
#' Create summary for large patches function.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @return shiny module UI.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom shiny fluidRow NS
#' @importFrom shinydashboard box
#' @rdname largePatches
largePatchesUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE,
      clumpMod2UI(ns("largePatches"))
    ),
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE, slicerUI(ns("slicer"))
    )
  )
}

#' @param input    Shiny server input object.
#'
#' @param output   Shiny server output object.
#'
#' @param session  Shiny server session object.
#'
#' @param numberOfSimulationTimes How many simulation time stamps there are.
#'
#' @param clumpMod2Args Reactive list containing named arguments passed to \code{clumpMod2}.
#'                      All arguments except \code{id} should be determined.
#'                      Any \code{id} parameter will be ignored.
#'
#' @return Shiny module server function.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom shiny callModule reactive
#' @importFrom data.table data.table
#' @importFrom graphics hist
#' @importFrom purrr map
#' @rdname largePatches
largePatches <- function(session, input, output, numberOfSimulationTimes, clumpMod2Args) {
  patchSize <- callModule(slider, "slider")

  uiSequence <- data.table(category = c("ageClass", "polygonID", "vegCover"),
                           uiType = c("tab", "tab", "box"))

  clumpMod2Args <- reactive({
    args <- clumpMod2Args()
    args["id"] <- NULL
    args
  })

  clumpsReturn <- do.call(callModule, c(list(clumpMod2, "largePatches"), clumpMod2Args))

  largePatchesData <- clumpsReturn$Clumps

  callModule(slicer, "slicer", largePatchesData, "LargePatches",
             uiSequence = uiSequence,
             serverFunction = function(data, chosenCategories, chosenValues) {
               observeEvent(data, {
                 histogramReactiveParams <- reactive({
                   data <- data()

                   subtableWith3DimensionsFixed <- getSubtable(data,
                                                               chosenCategories,
                                                               chosenValues)
                   ageClassPolygonSubtable <- getSubtable(data,
                                                          head(chosenCategories, 2),
                                                          head(chosenValues, 2))

                   numOfClusters <- ageClassPolygonSubtable[, .N, by = c("vegCover", "rep")]$N
                   maxNumClusters <- if (length(numOfClusters) == 0) {
                     6
                   } else {
                     pmax(6, max(numOfClusters) + 1)
                   }

                   patchesInTimeDistribution <- if (NROW(subtableWith3DimensionsFixed)) {
                     numOfPatchesInTime <- subtableWith3DimensionsFixed[, .N, by = "rep"]
                     numOfTimesWithPatches <- NROW(numOfPatchesInTime)

                     seq(1, numberOfSimulationTimes) %>%
                       map(function(simulationTime) {
                         if (simulationTime <= numOfTimesWithPatches) {
                           numOfPatchesInTime$N[simulationTime]
                         } else {
                           0
                         }
                       })
                   } else {
                     rep(0, numberOfSimulationTimes)
                   }

                   breaksLabels <- 0:(maxNumClusters)
                   breaks <- breaksLabels - 0.5
                   barplotBreaks <- breaksLabels + 0.5

                   return(list(breaks = breaks,
                               distribution = as.numeric(patchesInTimeDistribution),
                               breaksLabels = breaksLabels, barplotBreaks = barplotBreaks))
                 })

                 breaks <- reactive(histogramReactiveParams()$breaks)

                 distribution <- reactive(histogramReactiveParams()$distribution)

                 addAxisParams <- reactive(
                   list(side = 1,
                        labels = histogramReactiveParams()$breaksLabels,
                        at = histogramReactiveParams()$barplotBreaks)
                 )

                 histogramData <- reactive({
                   actualPlot <- hist(distribution(),
                                      breaks = breaks())

                   actualPlot$counts / sum(actualPlot$counts)
                 })

                 observeEvent({
                   histogramData
                   breaks
                   distribution
                   addAxisParams
                 }, {
                   breaks <- breaks()
                   distribution <- distribution()

                   callModule(histogram, "histogram",
                              histogramData, addAxisParams,
                              width = rep(1, length(distribution)),
                              xlim = range(breaks), xlab = "",
                              ylab = "Proportion in NRV", ## TODO: don't hardcode this
                              col = "darkgrey", border = "grey", main = "",
                              space = 0)
                 })
               })
             },
             uiFunction = function(ns) {
               histogramUI(ns("histogram"), height = 300)
             })
}
