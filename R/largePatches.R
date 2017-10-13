#' Large Patches Module
#'
#' @description Shiny module used to create summary for large patches function.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#'
#' @return Shiny module UI.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom shiny NS fluidRow
#' @importFrom shinydashboard box
#' @rdname largePatches
largePatchesUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE,
      clumpMod2Input(ns("largePatches"))
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
#' @param clumpMod2Args Arguments passed to \code{clumpMod2}. All arguments except
#'                      \code{id} should be determined. Any \code{id} parameter will
#'                      be surpressed.
#'
#' @return Shiny module server function.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom shiny callModule reactive
#' @importFrom data.table data.table
#' @rdname largePatches
largePatches <- function(session, input, output, numberOfSimulationTimes, clumpMod2Args) {
  patchSize <- callModule(slider, "slider")

  uiSequence <- data.table(category = c("ageClass", "polygonID", "vegCover"),
                           uiType = c("tab", "tab", "box"))

  clumpMod2Args["id"] <- NULL

  clumpsReturn <- do.call(callModule,
                           c(list(clumpMod2, "largePatches"), clumpMod2Args))

  largePatchesData <- reactive(clumpsReturn()$Clumps)

  callModule(slicer, "slicer", largePatchesData,
             "LargePatches", uiSequence = uiSequence,
             serverFunction = function(data, chosenCategories, chosenValues) {
               observeEvent(data, {
                 histogramReactiveParams <- reactive({
                   data <- data()

                   subtableWith3DimensionsFixed <- getSubtable(data, chosenCategories, chosenValues)
                   subtableWith2DimensionsFixed <- getSubtable(data,
                                                               head(chosenCategories, 2),
                                                               head(chosenValues, 2))

                   maxNumClusters <- subtableWith2DimensionsFixed[, .N, by = c("vegCover", "rep")]$N + 1
                   maxNumClusters <- if (length(maxNumClusters) == 0) 6 else pmax(6, max(maxNumClusters))
                   numberOfPatchesInTime <- rep(0, numberOfSimulationTimes)
                   if (NROW(subtableWith3DimensionsFixed)) {
                     numberOfPatchesByTime <- subtableWith3DimensionsFixed[, .N, by = "rep"]
                     numberOfPatchesInTime[seq_len(NROW(numberOfPatchesByTime))] <- numberOfPatchesByTime$N
                   }
                   breaksLabels <- 0:(maxNumClusters)
                   breaks <- breaksLabels - 0.5
                   barplotBreaks <- breaksLabels + 0.5

                   return(list(breaks = breaks, numberOfPatchesInTime = numberOfPatchesInTime,
                               breaksLabels = breaksLabels, barplotBreaks = barplotBreaks))
                 })

                 breaks <- reactive(histogramReactiveParams()$breaks)

                 numberOfPatchesInTime <- reactive(histogramReactiveParams()$numberOfPatchesInTime)

                 addAxisParams <- reactive(
                   list(side = 1,
                        breaksLabels = histogramReactiveParams()$breaksLabels,
                        barplotBreaks = histogramReactiveParams()$barplotBreaks)
                 )

                 histogramData <- reactive({
                   actualPlot <- hist(numberOfPatchesInTime(),
                                      breaks = breaks())

                   actualPlot$counts / sum(actualPlot$counts)
                 })

                 observeEvent(histogramData,{
                   breaks <- breaks()
                   numberOfPatchesInTime <- numberOfPatchesInTime()

                   callModule(histogram, "histogram",
                              histogramData, addAxisParams,
                              width = rep(1, length(numberOfPatchesInTime)),
                              xlim = range(breaks))
                 })
               })
             },
             uiFunction = function(ns) {
               histogramUI(ns("histogram"), height = 500)
             })
}
