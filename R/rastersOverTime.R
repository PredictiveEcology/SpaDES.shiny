#' Rasters-over-time shiny module
#'
#' A shiny module showing the values from a raster in time on a predefined map.
#' Allows changing polygons and rasters via slider.
#' Additionally, a histogram summary for each raster choice is shown.
#'
#' @param id An ID string that corresponds with the ID used to call the module server function.
#'
#' @return None. Invoked for the side-effect of creating a shiny UI.
#'
#' @author Damian Rodziewicz
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS
#' @rdname rasterOverTime
rastersOverTimeUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("rotUI"))
}

#' @param input           Shiny server input object.
#' @param output          Shiny server output object.
#' @param session         Shiny server session object.
#' @param rasterList      List of rasters to be displayed.
#' @param urlTemplate     The url template for leaflet map tiles
#' @param rctPolygonList  Reactive list with sets of polygons to be displayed on a leaflet map.
#'                        # TODO: decribe the format of the list!
#' @param defaultPolyName Name of the polygon to use as the default for mapping.
#' @param map             Leaflet map to show raster and polygons on.
#' @param colorTable      File that contains colour values for tiles (passed to \code{\link{gdal2Tiles}}).
#' @param histTitle       Title to be shown above the histogram.
#' @param mapTitle        Title to be shown above the map.
#' @param sliderTitle     Title to be shown above the slider.
#' @param nPolygons       The number of available polygons.
#' @param nRasters        The number of available rasters.
#' @param rasterStepSize  Size of step in the raster slider.
#'
#' @return Reactive polygon selected by the user with the \code{polygonChooser} module.
#'          Invoked for the side-effect of creating shiny server and ui components. # TODO: reword
#'
#' @export
#' @importFrom leaflet JS layersControlOptions leaflet leafletOptions leafletOutput leafletProxy
#' @importFrom leaflet providerTileOptions renderLeaflet setView tileOptions
#' @importFrom raster cellFromXY crs extract filename hist maxValue ncell
#' @importFrom raster sampleRegular res rowColFromCell xmax xmin ymax ymin
#' @importFrom reproducible asPath Cache
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinydashboard box
#' @importFrom shiny animationOptions br callModule h4 isolate observe reactive renderPlot tagList
#' @importFrom sp SpatialPoints spTransform
#' @importFrom SpaDES.core cachePath outputPath paddedFloatToChar
#' @rdname rasterOverTime
rastersOverTime <- function(input, output, session, rctRasterList, urlTemplate,
                            rctPolygonList, defaultPolyName = NULL, map = leaflet(), colorTable,
                            histTitle = "", sliderTitle = "", mapTitle = "",
                            nPolygons, nRasters, rasterStepSize = 10) {
  ns <- session$ns

  output$map <- renderLeaflet(map)
  mapProxy <- leafletProxy("map")

  rasterIndexValue <- callModule(slider, "rastersSlider", label = sliderTitle,
                                 min = 0, max = (nRasters - 1) * rasterStepSize,
                                 value = 0, step = rasterStepSize,
                                 animate = animationOptions(interval = 2500, loop = FALSE))

  rctPoly4Map <- reactive({
    polyList <- rctPolygonList()
    polyList[[chosenPolyName()]][["crsLFLT"]][["shpSubStudyRegion"]]
  })

  chosenPolyName <- callModule(polygonChooser, "polyDropdown", rctPolygonList, defaultPolyName) ## reactive character

  rast <- reactive({
    rasterIndex <- if (is.null(rasterIndexValue())) {
      1
    } else {
      rasterIndexValue() / rasterStepSize + 1
    }

    rst <- rasterList[[rasterIndex]]

    return(rst);
  })

  sampledRaster <- reactive({
    # TODO: make this adjust to input$map_bounds
    if (ncell(rast()) > 3e5) {
      sampledRaster <- Cache(raster::sampleRegular, rast(), size = 4e5, asRaster = TRUE)
    } else {
      sampledRaster <- rast()
    }
    sampledRaster[sampledRaster[] == 0] <- NA

    sampledRaster
  })

  numberOfBreaks <- reactive(ceiling(maxValue(rast()) / 10))
  breaks <- reactive(numberOfBreaks())

  addAxisParams <- reactive({
    numberOfBreaks <- numberOfBreaks()
    return(list(side = 1, at = 0:numberOfBreaks, labels = 0:numberOfBreaks * 10))
  })

  rasterScale <- reactive(prod(raster::res(rast())) / 1e4)

  addTilesParameters <- list(
    option = tileOptions(tms = TRUE, minZoom = 1, maxZoom = 10, opacity = 0.8)
  )

  click <- reactive(input$map_shape_click)

  urlTemplate2 <- urlTemplate # TODO: chop off "www/" and enusre it's only one element
  callModule(tilesUpdater, "tilesUpdater", mapProxy, urlTemplate2, ns("tiles"), ## don't change ns
             addTilesParameters = addTilesParameters, addLayersControlParameters = NULL)

  callModule(summaryPopups, "popups", mapProxy, click, rast, rctPoly4Map)

  callModule(polygonsUpdater, "polygonsUpdater", mapProxy, rctPoly4Map,
             fillOpacity = 0.0, weight = 0.5)

  callModule(histogramForRaster, "histogram", sampledRaster, histogramBreaks = breaks,
             scale = rasterScale(), addAxisParams = addAxisParams,
             width = 1, space = 0)

  output$rotUI <- renderUI({
    ns <- session$ns

    tagList(
      box(width = 8, solidHeader = TRUE, collapsible = TRUE, h4(mapTitle),
          shinycssloaders::withSpinner(leaflet::leafletOutput(ns("map"), height = 600)),
          sliderUI(ns("rastersSlider")),
          polygonChooserUI(ns("polyDropdown"))
      ),
      histogramForRasterUI(ns("histogram"), title = h4(histTitle),
                           plotParameters = list(height = 600), solidHeader = TRUE,
                           collapsible = TRUE, width = 4)
    )
  })

  return(chosenPolyName) ## the reactive polygon selected by the user
}
