#' Time series of rasters shiny module
#'
#' A shiny module that displays rasters changing in time on a predefined map,
#' with the user choosing which raster should be displayed by using the slider.
#' A histogram summary for each raster choice is also shown.
#'
#' @param id An ID string that corresponds with the ID used to call the module server function.
#'
#' @return Invoked for the side-effect of creating a shiny UI.
#'
#' @export
#' @importFrom shiny NS
#' @rdname timeSeriesofRasters
timeSeriesofRastersUI <- function(id) {
  ns <- NS(id)

  rastersOverTimeUI(ns("rastersOverTime"))
}

#' @inheritParams rastersOverTime
#'
#' @param shpStudyRegionFull  Study area region. # TODO: fix this
#' @param palette             Color palette for the rasters.
#' @param maxAge              Maximum simulation age.
#' @param zoom                Initial leaflet zoom.
#' @param studyArea           Size of study area.
#'                            Options: \code{"FULL"}, \code{"EXTRALARGE"}, \code{"LARGE"},
#'                            \code{"MEDIUM"}, \code{"NWT"}, \code{"SMALL"}.
#'                            # TODO: use actual study area!
#'
#' @return Invoked for the side-effect of creating shiny server and ui components.
#'
#' @author Mateusz Wyszynski
#' @author Alex Chubaty
#' @export
#' @importFrom graphics axis barplot
#' @importFrom leaflet addEasyButton addLegend addMeasure addMiniMap addPolygons
#' @importFrom leaflet addLayersControl addPopups addProviderTiles
#' @importFrom leaflet clearPopups colorFactor easyButton JS
#' @importFrom leaflet layersControlOptions leaflet leafletOptions leafletOutput leafletProxy
#' @importFrom leaflet providerTileOptions renderLeaflet setView tileOptions
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinydashboard box
#' @importFrom shiny animationOptions br callModule h4 isolate observe reactive renderPlot tagList
#' @importFrom sp SpatialPoints spTransform
#' @importFrom raster cellFromXY crs extract filename hist maxValue ncell
#' @importFrom raster rowColFromCell xmax xmin ymax ymin
#' @importFrom reproducible asPath Cache
#' @importFrom SpaDES.core paddedFloatToChar
#' @rdname timeSeriesofRasters
#'
timeSeriesofRasters <- function(input, output, session, rasters, polygons, shpStudyRegionFull,
                                colorTable, palette, maxAge, zoom = 5,
                                studyArea = "SMALL", sim = NULL,
                                mapTitle = "", sliderTitle = "", histTitle = "",
                                nPolygons, nRasters, rasterStepSize = 10) {

  polygonsInput <- reactive({
    spTransform(shpStudyRegionFull, crs(polygons[[3]])) # TODO: why polygons[[3]]??
  })

  leafZoom <- zoom

  pol <- polygons[[4]] # TODO: why this particular list entry only? is this the smallest area?
  shpStudyRegionFullLFLT <- spTransform(shpStudyRegionFull, crs(isolate(polygonsInput())))

  leafMap <- leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10)) %>%
    addProviderTiles("Thunderforest.OpenCycleMap", group = "Open Cycle Map",
                     options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
    addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI World Imagery",
                     options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
    addLegend(position = "bottomright", pal = palette,
              values = 1:maxAge,
              title = paste0("Time since fire", br(), "(years)")) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "kilometers",
      primaryAreaUnit = "hectares",
      activeColor = "#3D535D",
      completedColor = "#7D4479") %>%
    addEasyButton(easyButton(
      icon = "fa-map", title = "Zoom to focal area", # TODO: generalize this
      onClick = JS(paste0("function(btn, map){ map.fitBounds([[", ymin(pol), ", ",
                          xmin(pol), "], [", ymax(pol), ", ", xmax(pol), "]])}")))) %>%
    addEasyButton(easyButton(
      icon = "fa-globe", title = "Zoom out to full study area",
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

  callModule(rastersOverTime, "rastersOverTime", rasters = rasters, polygonsList = polygons,
             map = leafMap,  colorTable = colorTable,
             histogramTitle = histTitle, sliderTitle = sliderTitle, mapTitle = mapTitle,
             nPolygons = nPolygons, nRasters = nRasters, rasterStepSize = 10, sim = sim,
             cacheNotOlderThan = NULL)
}
