#' Rasters Over Time Shiny Module
#'
#' @description A shiny module showing the values from a raster in time.
#'              Allows changing polygons and rasters via slider.
#'
#' @param id An ID string that corresponds with the ID used to call the module server function.
#'
#' @param mapTitle Title to be shown above the map.
#' @param sliderTitle Title to be shown above the slider.
#' @param histogramTitle Title to be shown above the histogram.
#' @param polygonsNumber The number of available polygons.
#' @param rastersNumber The number of available rasters.
#' @param rasterStepSize Size of step in the raster slider.
#'
#' @return None. Invoked for the side-effect of creating a shiny UI.
#'
#' @author Damian Rodziewicz
#'
#' @export
#' @importFrom leaflet leafletOutput
#' @importFrom raster sampleRegular
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny NS tagList h4 animationOptions
#' @importFrom shinydashboard box
rastersOverTimeUI <- function(id, mapTitle, sliderTitle, histogramTitle,
                              polygonsNumber, rastersNumber, rasterStepSize = 10) {
  ns <- NS(id)

  tagList(
    box(width = 8, solidHeader = TRUE, collapsible = TRUE, h4(mapTitle),
        shinycssloaders::withSpinner(leaflet::leafletOutput(ns("map"), height = 600)),
        sliderUI(ns("rastersSlider"), sliderTitle, min = 0,
                 max = (rastersNumber - 1) * rasterStepSize,
                 value = 0, step = rasterStepSize,
                 animate = animationOptions(interval = 2500, loop = FALSE)),
        sliderUI(ns("polygonsSlider"), "Change polygons", min = 1, max = polygonsNumber,
                 value = 1, step = 1, animate = animationOptions(interval = 5000, loop = TRUE))
    ),
    histogramForRasterUI(ns("histogram"),
                         title = h4(histogramTitle),
                         plotParameters = list(height = 600), solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 4)
  )
}

#' Rasters Over Time Shiny Module
#'
#' @description Function \code{rastersOverTime} creates a shiny module server function
#'              which displays rasters changing in time on a predefined map.
#'              A slider, on which you can choose which raster should be currently displayed,
#'              is also created. Moreover, a histogram summary for each raster choice is shown.
#'
#' @param input Shiny server input object.
#' @param output Shiny server output object.
#' @param session Shiny server session object.
#' @param rasters Set of rasters to be displayed.
#' @param polygonsList List with sets of polygons. Each such set can be displayed on a leaflet map.
#' @param colorTableFile File that contains color values for tiles.
#' @param map Leaflet map to show raster and polygons on.
#' @param rasterStepSize .
#' @param cachePath Path to cache folder.
#' @param cacheNotOlderThan Load an artifact from cache only if it was created after notOlderThan.
#'
#' @return None. Invoked for the side-effect of creating a shiny server part.
#'
#' @importFrom leaflet JS layersControlOptions leaflet leafletOptions leafletProxy
#' @importFrom leaflet providerTileOptions renderLeaflet setView tileOptions
#' @importFrom shiny br callModule isolate observe reactive renderPlot
#' @importFrom sp SpatialPoints spTransform
#' @importFrom raster cellFromXY crs extract filename maxValue ncell rowColFromCell
#' @importFrom raster xmax xmin ymax ymin hist res
#' @importFrom reproducible asPath Cache
#' @importFrom SpaDES.core paddedFloatToChar end
#'
#' @author Damian Rodziewicz
#'
#' @export
rastersOverTime <- function(input, output, session, rasters, polygonsList, colorTableFile,
                            map = leaflet(), rasterStepSize = 10, cachePath = "cache",
                            cacheNotOlderThan = Sys.time()) {
  output$map <- renderLeaflet(map)
  mapProxy <- leafletProxy("map")

  rasterIndexValue <- callModule(slider, "rastersSlider")
  polygonIndexValue <- callModule(slider, "polygonsSlider")

  polygons <- reactive({
    index <- if (is.null(polygonIndexValue())) {
      1
    } else {
      polygonIndexValue()
    }
    return(polygonsList[[index]])
  })

  raster <- reactive({
    rasterIndex <- if (is.null(rasterIndexValue())) {
      1
    } else {
      rasterIndexValue() / rasterStepSize + 1
    }

    raster <- rasters[[rasterIndex]]

    Cache(gdal2Tiles, raster, outputPath = file.path("www", session$ns("map-tiles")),
          zoomRange = 1:10, colorTableFile = asPath(colorTableFile),
          cacheRepo = cachePath, notOlderThan = cacheNotOlderThan, digestPathContent = TRUE)

    return(raster);
  })

  sampledRaster <- reactive({
    if (ncell(raster()) > 3e5) {
      sampledRaster <- Cache(raster::sampleRegular, raster(), size = 4e5, notOlderThan = Sys.time(),
                             asRaster = TRUE, cacheRepo = cachePath)
      sampledRaster[sampledRaster[] == 0] <- NA
    }

    sampledRaster
  })

  numberOfBreaks <- reactive(ceiling(maxValue(raster()) / 10))
  breaks <- reactive(numberOfBreaks())

  addAxisParams <- reactive({
    numberOfBreaks <- numberOfBreaks()
    return(list(side = 1, at = 0:numberOfBreaks, labels = 0:numberOfBreaks * 10))
  })

  rasterScale <- isolate(prod(raster::res(raster())) / 1e4)

  urlTemplate <- reactive({
    rasterFilename <- strsplit(basename(filename(raster())), "\\.")[[1]][[1]]
    file.path(session$ns("map-tiles"), paste0("out", rasterFilename, "/{z}/{x}/{y}.png"))
  })

  addTilesParameters <- list(
    option = tileOptions(tms = TRUE, minZoom = 1, maxZoom = 10, opacity = 0.8)
  )

  click <- reactive(input$map_shape_click)

  callModule(tilesUpdater, "tilesUpdater", mapProxy, urlTemplate, session$ns("tiles"),
             addTilesParameters = addTilesParameters, addLayersControlParameters = NULL)

  callModule(summaryPopups, "popups", mapProxy, click, raster, polygons)

  callModule(polygonsUpdater, "polygonsUpdater", mapProxy, polygons)

  callModule(histogramForRaster, "histogram", sampledRaster, histogramBreaks = breaks,
             scale = rasterScale, addAxisParams = addAxisParams,
             width = 1, space = 0)
}
