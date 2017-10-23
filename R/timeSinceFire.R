#' Time-Since-Fire shiny module
#'
#' @description A shiny module showing the time-since-fire values from a raster.
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
#' @rdname timeSinceFire
timeSinceFireUI <- function(id, rastersNumber) {
  ns <- NS(id)

  rastersOverTimeUI(ns("rastersOverTime"), mapTitle = "", sliderTitle = "", histogramTitle = "",
                    polygonsNumber = 1, rastersNumber = rastersNumber, rasterStepSize = 10)
}

#' Time Since Fire Shiny Module
#'
#' @description Function \code{timeSinceFire} creates a shiny module server function
#'              which displays rasters changing in time on a predefined map.
#'              A slider, on which you can choose which raster should be currently displayed,
#'              is also created. Moreover, a histogram summary for each raster choice is shown.
#'
#' @param input Shiny server input object.
#' @param output Shiny server output object.
#' @param session Shiny server session object.
#' @param rasters Set of rasters to be displayed.
#' @param polygonsList List with sets of polygons. Each such set can be displayed on a leaflet map.
#' @param leafletZoomInit Initial leaflet zoom.
#' @param studyArea Size of study area. Options: \code{"FULL"}, \code{"EXTRALARGE"},
#'                  \code{"LARGE"}, \code{"MEDIUM"}, \code{"NWT"}, \code{"SMALL"}.
#'
#' @return None. Invoked for the side-effect of creating a shiny server part.
#'
#' @importFrom graphics axis barplot
#' @importFrom leaflet addEasyButton addLegend addMeasure addMiniMap addPolygons addLayersControl
#' @importFrom leaflet addPopups addProviderTiles clearPopups colorFactor easyButton
#' @importFrom leaflet JS layersControlOptions leaflet leafletOptions leafletProxy
#' @importFrom leaflet providerTileOptions renderLeaflet setView tileOptions
#' @importFrom shiny br callModule isolate observe reactive renderPlot
#' @importFrom sp SpatialPoints spTransform
#' @importFrom raster cellFromXY crs extract filename maxValue ncell rowColFromCell
#' @importFrom raster xmax xmin ymax ymin hist
#' @importFrom reproducible asPath Cache
#' @importFrom SpaDES.core paddedFloatToChar end
#'
#' @author Mateusz Wyszynski
#'
#' @rdname timeSinceFire
#'
#' @export
timeSinceFire <- function(input, output, session, rasters, polygonsList, leafletZoomInit = 5,
                          studyArea = "SMALL") {

  polygonsInput <- reactive({
    spTransform(shpStudyRegionFull, crs(polygonsList[[3]]))
  })

  leafZoom <- leafletZoomInit

  pol <- polygonsList[[4]]
  shpStudyRegionFullLFLT <- spTransform(shpStudyRegionFull, crs(isolate(polygonsInput())))

  leafMap <- leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10)) %>%
    addProviderTiles("Thunderforest.OpenCycleMap", group = "Open Cycle Map",
                     options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery",
                     options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
    addLegend(position = "bottomright", pal = timeSinceFirePalette,
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
      tiles = providers$OpenStreetMap,
      toggleDisplay = TRUE) %>%
    setView(mean(c(xmin(shpStudyRegionFullLFLT), xmax(shpStudyRegionFullLFLT))),
            mean(c(ymin(shpStudyRegionFullLFLT), ymax(shpStudyRegionFullLFLT))),
            zoom = leafZoom
    ) %>%
    addPolygons(data = isolate(polygonsInput()), group = "Fire return interval",
                fillOpacity = 0.3, weight = 1, color = "blue",
                fillColor = ~colorFactor("Spectral", fireReturnInterval)(fireReturnInterval))

  callModule(rastersOverTime, "rastersOverTime", rasters, polygonsList, colorTableFile,
                              leafMap, rasterStepSize = 10, cachePath = "cache",
                              cacheNotOlderThan = Sys.time())
}
