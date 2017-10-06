#' Slicer Module
#'
#' @description Shiny module used to: \cr
#'              1.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#'
#' @return Shiny module UI.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom shiny NS fluidRow
#' @importFrom shinydashboard box
#' @rdname slicer
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

largePatches <- function(session, input, output, numberOfSimulationTimes, clumpMod2Args) {
  patchSize <- callModule(slider, "slider")

  uiSequence <- data.table(category = c("ageClass", "polygonID", "vegCover"),
                           uiType = c("tab", "tab", "box"))

  ClumpsReturn2 <- do.call(callModule,
                           c(list(clumpMod2, "largePatches"), clumpMod2Args))

  callModule(slicer, "slicer", reactive({ClumpsReturn2()$Clumps}),
             reactive({ClumpsReturn2()$Clumps}), "LargePatches", uiSequence = uiSequence,
             serverFunction = function(data, globalData, chosenCategories) {
               callModule(histogram, "histogram", data,
                          globalData, chosenCategories,
                          numberOfSimulationTimes = numberOfSimulationTimes)
             },
             uiFunction = function(ns) {
               histogramUI(ns("histogram"), height = 500)
             },
             chosenCategories = NULL)
}
