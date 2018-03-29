#' Shiny module for selecting a polygon to display an a map
#'
#' Provides a dropdown list of polygons available for mapping, sorted alphabetically.
#'
#' @param id An ID string that corresponds with the ID used to call the module server function.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny uiOutput
#' @rdname polygonChooser
polygonChooserUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("polyChooser"))
}

#' @param input         Shiny server input object.
#' @param output        Shiny server output object.
#' @param session       Shiny server session object.
#' @param polygonList   A nested list with the following structure:
#'                      # TODO: fill this in
#' @param selectedPoly  The name of the polygon to select by default.
#'
#' @return A reactive containing the name of the selected polygon.
#'
#' @export
#' @importFrom shiny selectInput
#' @rdname polygonChooser
#'
#' @examples
#' \dontrun{
#' library(leaflet)
#' library(magrittr)
#' library(shiny)
#'
#' shinyApp(
#'   ui = fluidPage(
#'     leafletOutput("map"),
#'     polygonChooserUI("polyPicker")
#'   ),
#'   server = function(input, output, session) {
#'     dummyPoly1 <- SpaDES.tools::randomPolygon(matrix(c(-120, 60), ncol = 2), 100)
#'     dummyPoly2 <- SpaDES.tools::randomPolygon(matrix(c(-115, 60), ncol = 2), 100)
#'     polygonList <- list(caribou = dummyPoly1, ecozones = dummyPoly2)
#'     chosenPolyName <- callModule(polygonChooser, "polyPicker", polygonList, "ecozones")
#'
#'     output$map <- renderLeaflet({
#'       leaflet() %>%
#'         addTiles() %>%
#'         addPolygons(data = polygonList[[chosenPolyName()]])
#'     })
#'   }
#' )
#' }
polygonChooser <- function(input, output, session, polygonList, selectedPoly = NULL) {
  # TODO: assert that list has correct structure
  polyNames <- names(polygonList)

  chosen <- if (is.null(selectedPoly)) {
    1
  } else {
    which(polyNames == selectedPoly)
  }

  output$polyChooser <- renderUI({
    ns <- session$ns

    # TODO: display in alphabetical order
    selectInput(ns("polyLayer"), "Polygon layer:", polyNames, selected = polyNames[[chosen]])
  })

  return(reactive({
    validate(need(input$polyLayer, message = "Please select a polygon layer."))
    input$polyLayer
  }))
}
