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
#' @param rctPolygonList  A reactive nested list with the following structure:
#'                        # TODO: fill this in
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
#' library(SpaDES.tools)
#'
#' shinyApp(
#'   ui = fluidPage(
#'     leafletOutput("map"),
#'     polygonChooserUI("polyPicker")
#'   ),
#'   server = function(input, output, session) {
#'     dummyPoly <- function() {
#'       randomPolygon(matrix(c(-120, 60), ncol = 2), 100)
#'     }
#'     dummyPoly1 <- dummyPoly()
#'     dummyPoly2 <- dummyPoly()
#'     dummyPoly3 <- dummyPoly()
#'     polygonList <- reactive(list(caribou = dummyPoly1, ecozones = dummyPoly2, fmu = dummyPoly3))
#'     chosenPolyName <- callModule(polygonChooser, "polyPicker", polygonList, "ecozones")
#'
#'     output$map <- renderLeaflet({
#'       leaflet() %>%
#'         addTiles() %>%
#'         addPolygons(data = polygonList()[[chosenPolyName()]])
#'     })
#'   }
#' )
#' }
polygonChooser <- function(input, output, session, rctPolygonList, selectedPoly = NULL) {
  # TODO: assert that list has correct structure

  output$polyChooser <- renderUI({
    ns <- session$ns

    # TODO: display in alphabetical order
    #selectInput(ns("polyLayer"), "Polygon layer:", names(rctPolygonList()), selected = selectedPoly)
    shinyWidgets::pickerInput(
      inputId = ns("polyLayer"),
      label = "Polygon layer:",
      choices = names(rctPolygonList()),
      selected = selectedPoly,
      options = list(`actions-box` = TRUE),
      multiple = FALSE,
      inline = TRUE
    )

  })

  return(reactive({
    validate(need(input$polyLayer, message = "Please select a polygon layer."))
    input$polyLayer
  }))
}
