#' Shiny module for selecting a polygon to display an a map
#'
#' Provides a dropdown list of polygons available for mapping, sorted alphabetically.
#'
#' @param id An ID string that corresponds with the ID used to call the module server function.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny br fluidRow uiOutput
#' @rdname polygonChooser
polygonChooserUI <- function(id) {
  ns <- NS(id)

  tagList(
    column(width = 12, uiOutput(ns("polyChooser"))),
    hr(),
    column(width = 12, uploadPolygonUI(ns("uploadPolygon")))
  )
}

#' @param input           Shiny server input object.
#' @param output          Shiny server output object.
#' @param session         Shiny server session object.
#' @param rctPolygonList  A reactive nested list with the following structure:
#'                        # TODO: fill this in
#' @param selectedPoly    The name of the polygon to select by default.
#' @param uploadOpts      A list of options for use with file uploads:
#'                        \code{auth} logical indicating whether user is authorized to upload;
#'                        \code{path} a directory path to use for file uploads;
#'                        \code{user} the current username (used for creating user-specific paths);
#'                        The default for all options is \code{NULL}, which means do not use.
#' @param studyArea       A \code{Spatial} object used as a template for post-processing
#'                        the uploaded polygon, which is cropped, reprojected, etc.
#'                        to match \code{studyArea}. See \code{\link[SpaDES.tools]{postProcess}}.
#'
#' @return A reactive list with elements \code{polygons} (a list of polygons) and
#'         \code{selected} (the name of the selected polygon).
#'
#' @export
#' @include uploadPolygon.R
#' @importFrom shiny isTruthy need validate
#' @importFrom shinyWidgets pickerInput
#' @rdname polygonChooser
#'
#' @examples
#' \dontrun{
#' if require(SpaDES.tools) {
#'   library(leaflet)
#'   library(magrittr)
#'   library(shiny)
#'
#'   shinyApp(
#'     ui = fluidPage(
#'       htmlOutput("infobox"),
#'       leafletOutput("map"),
#'       polygonChooserUI("polyPicker")
#'     ),
#'     server = function(input, output, session) {
#'       dummyPoly <- function() {
#'         randomPolygon(matrix(c(-120, 60), ncol = 2), 100)
#'       }
#'       dummyPoly1 <- dummyPoly()
#'       dummyPoly2 <- dummyPoly()
#'       dummyPoly3 <- dummyPoly()
#'       dummyPoly4 <- dummyPoly()
#'
#'       tmpf <- tempfile(fileext = ".shp")
#'       raster::shapefile(dummyPoly4, filename = tmpf)
#'
#'       polygonList <- reactive(list(caribou = dummyPoly1, ecozones = dummyPoly2, fmu = dummyPoly3))
#'       out <- callModule(polygonChooser, "polyPicker", polygonList, "ecozones",
#'                         uploadOpts = list(
#'                           auth = TRUE,
#'                           path = dirname(tmpf),
#'                           user = "username"
#'                         ))
#'
#'       updatedPolygonList <- reactive(out()$polygons)
#'       chosenPolyName <- reactive(out()$selected)
#'       chosenPoly <- reactive(updatedPolygonList()[[chosenPolyName()]])
#'
#'       output$infobox <- renderUI({
#'         fluidRow(
#'           h4("Currently viewing the ", chosenPolyName(), " polygon.")
#'         )
#'       })
#'
#'       output$map <- renderLeaflet({
#'         leaflet() %>%
#'           addTiles() %>%
#'           addPolygons(data = spTransform(chosenPoly(), CRSobj = proj4stringLFLT))
#'       })
#'     }
#'   )
#' }
#' }
#'
polygonChooser <- function(input, output, session, rctPolygonList, selectedPoly = NULL,
                           uploadOpts = list(auth = NULL, path = NULL, user = NULL),
                           studyArea = NULL) {

  rctPolygonListUser <- reactive({
    assertthat::assert_that(all(vapply(rctPolygonList(), function(x) {
     inherits(x, "SpatialPolygons")
    }, logical(1))))

    ns <- session$ns
    if (all(vapply(uploadOpts, isTruthy, logical(1)))) {
      if (isTruthy(uploadOpts$auth)) {
        auth <- uploadOpts$auth
        userDir <- file.path(uploadOpts$path, uploadOpts$user)

        rctUploadedPolygonList <- callModule(uploadPolygon, "uploadPolygon", auth, userDir, studyArea)

        append(rctPolygonList(), rctUploadedPolygonList())
      } else {
        rctPolygonList()
      }
    } else {
      rctPolygonList()
    }
  })

  output$polyChooser <- renderUI({
    ns <- session$ns

    # TODO: display in alphabetical order?
    #selectInput(ns("polyLayer"), "Polygon layer:", names(rctPolygonList()), selected = selectedPoly)
    shinyWidgets::pickerInput(
      inputId = ns("polyLayer"),
      label = "Polygon layer:",
      choices = names(rctPolygonListUser()),
      selected = selectedPoly,
      options = list(`actions-box` = TRUE),
      multiple = FALSE,
      inline = TRUE,
      width = "100%"
    )
  })

  observeEvent(rctPolygonListUser(), {
    origChoices <- names(rctPolygonList())
    userChoices <- names(rctPolygonListUser())
    choices <- if (identical(origChoices, userChoices)) {
      origChoices
    } else {
      userChoices
    }
    selected <- if (identical(origChoices, userChoices)) {
      selectedPoly
    } else {
      userChoices[[length(userChoices)]]
    }

    shinyWidgets::updatePickerInput(
      session,
      inputId = session$ns("polyLayer"),
      label = "Polygon layer:",
      choices = choices,
      selected = selected
    )
  })

  return(reactive({
    validate(need(input$polyLayer, message = "Please select a polygon layer."))
    list(selected = input$polyLayer, polygons = rctPolygonListUser())
  }))
}
