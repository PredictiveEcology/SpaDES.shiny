#' Time series of rasters shiny module
#'
#' Function \code{timeSeriesofRasters} creates a shiny module server function
#' which displays rasters changing in time on a predefined map.
#' A slider, on which you can choose which raster should be currently displayed,
#' is also created.
#' A histogram summary for each raster choice is also shown.
#'
#' @param id An ID string that corresponds with the ID used to call the module server function.
#'
#' @param rastersNumber How many rasters can we choose from.
#'
#' @return None. Invoked for the side-effect of creating a shiny UI.
#'
#' @export
#' @importFrom leaflet leafletOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny NS tagList h4 animationOptions
#' @importFrom shinydashboard box
#' @rdname timeSeriesofRasters
timeSeriesofRastersUI <- function(id, rastersNumber) {
  ns <- NS(id)

  rastersOverTimeUI(ns("rastersOverTime"), mapTitle = "", sliderTitle = "", histogramTitle = "",
                    polygonsNumber = 1, rastersNumber = rastersNumber, rasterStepSize = 10)
}

#' @param input Shiny server input object.
#' @param output Shiny server output object.
#' @param session Shiny server session object.
#' @param rasters Set of rasters to be displayed.
#' @param polygonsList List with sets of polygons. Each such set can be displayed on a leaflet map.
#' @param shpStudyRegionFull Study area region.
#' @param colorTableFile File that contains color values for tiles.
#' @param timeSeriesofRastersPalette Color palette for time since fire.
#' @param maxAge Maximum simulation age.
#' @param leafletZoomInit Initial leaflet zoom.
#' @param studyArea Size of study area. Options: \code{"FULL"}, \code{"EXTRALARGE"},
#'                  \code{"LARGE"}, \code{"MEDIUM"}, \code{"NWT"}, \code{"SMALL"}.
#' @param sim A SpaDES simulation object (\code{simList}).
#'
#' @return None. Invoked for the side-effect of creating a shiny server part.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom graphics axis barplot
#' @importFrom leaflet addEasyButton addLegend addMeasure addMiniMap addPolygons
#' @importFrom leaflet addLayersControl addPopups addProviderTiles
#' @importFrom leaflet clearPopups colorFactor easyButton JS
#' @importFrom leaflet layersControlOptions leaflet leafletOptions leafletProxy
#' @importFrom leaflet providerTileOptions renderLeaflet setView tileOptions
#' @importFrom shiny br callModule isolate observe reactive renderPlot
#' @importFrom sp SpatialPoints spTransform
#' @importFrom raster cellFromXY crs extract filename maxValue ncell rowColFromCell
#' @importFrom raster hist xmax xmin ymax ymin
#' @importFrom reproducible asPath Cache
#' @importFrom SpaDES.core paddedFloatToChar
#' @rdname timeSeriesofRasters
#'
timeSeriesofRasters <- function(input, output, session, rasters, polygonsList, shpStudyRegionFull,
                                colorTableFile, timeSeriesofRastersPalette, maxAge, leafletZoomInit = 5,
                                studyArea = "SMALL", sim = NULL) {

  polygonsInput <- reactive({
    spTransform(shpStudyRegionFull, crs(polygonsList[[3]]))
  })

  leafZoom <- leafletZoomInit

  pol <- polygonsList[[4]]
  shpStudyRegionFullLFLT <- spTransform(shpStudyRegionFull, crs(isolate(polygonsInput())))

  leafMap <- leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10)) %>%
    addProviderTiles("Thunderforest.OpenCycleMap", group = "Open Cycle Map",
                     options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
    addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI World Imagery",
                     options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
    addLegend(position = "bottomright", pal = timeSeriesofRastersPalette,
              values = 1:maxAge,
              title = paste0("Time since fire", br(), "(years)")) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "kilometers",
      primaryAreaUnit = "hectares",
      activeColor = "#3D535D",
      completedColor = "#7D4479") %>%
    addEasyButton(easyButton(
      icon = "fa-map", title = "Zoom to Demonstration Area",
      onClick = JS(paste0("function(btn, map){ map.fitBounds([[", ymin(pol), ", ",
                          xmin(pol), "], [", ymax(pol), ", ", xmax(pol), "]])}")))) %>%
    addEasyButton(easyButton(
      icon = "fa-globe", title = "Zoom out to LandWeb study area",
      onClick = JS(paste0("function(btn, map){ map.setView([",
                          mean(c(ymin(shpStudyRegionFullLFLT), ymax(shpStudyRegionFullLFLT))),
                          ", ", mean(c(xmin(shpStudyRegionFullLFLT),
                                       xmax(shpStudyRegionFullLFLT))), "], 5)}")))) %>%
    addMiniMap(
      tiles = leaflet::providers$OpenStreetMap,
      toggleDisplay = TRUE) %>%
    setView(mean(c(xmin(shpStudyRegionFullLFLT), xmax(shpStudyRegionFullLFLT))),
            mean(c(ymin(shpStudyRegionFullLFLT), ymax(shpStudyRegionFullLFLT))),
            zoom = leafZoom
    ) %>%
    addPolygons(data = isolate(polygonsInput()), group = "Fire return interval",
                fillOpacity = 0.3, weight = 1, color = "blue",
                fillColor = ~colorFactor("Spectral", fireReturnInterval)(fireReturnInterval))

  callModule(rastersOverTime, "rastersOverTime", rasters, polygonsList, colorTableFile,
                              leafMap, rasterStepSize = 10,
                              cacheNotOlderThan = NULL, sim = sim)
}
