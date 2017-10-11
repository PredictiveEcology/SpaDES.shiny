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

  ClumpsReturn2 <- do.call(callModule,
                           c(list(clumpMod2, "largePatches"), clumpMod2Args))

  largePatchesData <- reactive(ClumpsReturn2()$Clumps)

  callModule(slicer, "slicer", largePatchesData,
             "LargePatches", uiSequence = uiSequence,
             serverFunction = function(data, chosenCategories, chosenValues) {
               callModule(histogram, "histogram",
                          data, chosenCategories, chosenValues,
                          numberOfSimulationTimes = numberOfSimulationTimes)
             },
             uiFunction = function(ns) {
               histogramUI(ns("histogram"), height = 500)
             })
}
