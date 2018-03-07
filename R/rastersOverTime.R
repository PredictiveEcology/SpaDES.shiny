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
#' @param polygonList     List with sets of polygons. Each such set can be displayed on a leaflet map.
#' @param map             Leaflet map to show raster and polygons on.
#' @param colorTable      File that contains color values for tiles (passed to \code{\link{gdal2Tiles}}).
#' @param histTitle       Title to be shown above the histogram.
#' @param mapTitle        Title to be shown above the map.
#' @param sliderTitle     Title to be shown above the slider.
#' @param nPolygons       The number of available polygons.
#' @param nRasters        The number of available rasters.
#' @param rasterStepSize  Size of step in the raster slider.
#' @param sim             A SpaDES simulation object (\code{simList}).
#' @param cachePath       Path to cache folder.
#' @param cacheNotOlderThan  Load an artifact from cache only if it was created after notOlderThan.
#'
#' @return None. Invoked for the side-effect of creating a shiny server part.
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
#' @importFrom SpaDES.core getPaths paddedFloatToChar
#' @rdname rasterOverTime
rastersOverTime <- function(input, output, session, rasterList, polygonList, map = leaflet(),
                            colorTable, histTitle = "", sliderTitle = "", mapTitle = "",
                            nPolygons, nRasters, rasterStepSize = 10, sim = NULL,
                            cachePath = getPaths()$cachePath,
                            cacheNotOlderThan = Sys.time()) {
  ns <- session$ns

  output$map <- renderLeaflet(map)
  mapProxy <- leafletProxy("map")

  rasterIndexValue <- callModule(slider, "rastersSlider")
  polygonIndexValue <- callModule(slider, "polygonsSlider")

  pols <- reactive({
    index <- if (is.null(polygonIndexValue())) {
      1
    } else {
      polygonIndexValue()
    }
    return(polygonList[[index]])
  })

  if (is.null(sim)) {
    cachePath <- "cache"
    outputSubPath <- "outputs"
  } else {
    cachePath <- cachePath(sim)
    outputSubPath <- outputPath(sim)
  }

  rstr <- reactive({
    rasterIndex <- if (is.null(rasterIndexValue())) {
      1
    } else {
      rasterIndexValue() / rasterStepSize + 1
    }

    rast <- rasterList[[rasterIndex]]

    outputPath <- file.path("www", basename(outputSubPath), ns("map-tiles"))
    Cache(gdal2Tiles, rast, outputPath = outputPath,
          zoomRange = 1:10, colorTableFile = asPath(colorTable),
          cacheRepo = cachePath, notOlderThan = cacheNotOlderThan, digestPathContent = TRUE)

    return(rast);
  })

  sampledRaster <- reactive({
    if (ncell(rstr()) > 3e5) {
      sampledRaster <- Cache(raster::sampleRegular, rstr(), size = 4e5,
                             notOlderThan = NULL, asRaster = TRUE, cacheRepo = cachePath)
    } else {
      sampledRaster <- rstr()
    }
    sampledRaster[sampledRaster[] == 0] <- NA

    sampledRaster
  })

  numberOfBreaks <- reactive(ceiling(maxValue(rstr()) / 10))
  breaks <- reactive(numberOfBreaks())

  addAxisParams <- reactive({
    numberOfBreaks <- numberOfBreaks()
    return(list(side = 1, at = 0:numberOfBreaks, labels = 0:numberOfBreaks * 10))
  })

  rasterScale <- isolate(prod(raster::res(rstr())) / 1e4)

  urlTemplate <- reactive({
    rasterFilename <- strsplit(basename(filename(rstr())), "\\.")[[1]][[1]]
    file.path(basename(outputSubPath), ns("map-tiles"),
              paste0("out", rasterFilename, "/{z}/{x}/{y}.png"))
  })

  addTilesParameters <- list(
    option = tileOptions(tms = TRUE, minZoom = 1, maxZoom = 10, opacity = 0.8)
  )

  click <- reactive(input$map_shape_click)

  callModule(tilesUpdater, "tilesUpdater", mapProxy, urlTemplate, ns("tiles"),
             addTilesParameters = addTilesParameters, addLayersControlParameters = NULL)

  callModule(summaryPopups, "popups", mapProxy, click, rstr, polygonList)

  callModule(polygonsUpdater, "polygonsUpdater", mapProxy, polygonList, weight = 0.2)

  callModule(histogramForRaster, "histogram", sampledRaster, histogramBreaks = breaks,
             scale = rasterScale, addAxisParams = addAxisParams,
             width = 1, space = 0)

  output$rotUI <- renderUI({
    tagList(
      box(width = 8, solidHeader = TRUE, collapsible = TRUE, h4(mapTitle),
          shinycssloaders::withSpinner(leaflet::leafletOutput(ns("map"), height = 600)),
          sliderUI(ns("rastersSlider"), sliderTitle, min = 0,
                   max = (nRasters - 1) * rasterStepSize,
                   value = 0, step = rasterStepSize,
                   animate = animationOptions(interval = 2500, loop = FALSE)),
          sliderUI(ns("polygonsSlider"), "Change polygons", min = 1, max = nPolygons,
                   value = 1, step = 1, animate = animationOptions(interval = 5000, loop = TRUE))
      ),
      histogramForRasterUI(ns("histogram"), title = h4(histTitle),
                           plotParameters = list(height = 600), solidHeader = TRUE,
                           collapsible = TRUE, width = 4)
    )
  })
}
