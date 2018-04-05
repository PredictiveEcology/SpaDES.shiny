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
#' @importFrom shinycssloaders withSpinner
#' @rdname timeSeriesofRasters
timeSeriesofRastersUI <- function(id) {
  ns <- NS(id)

  shinycssloaders::withSpinner(rastersOverTimeUI(ns("rastersOverTime")))
}

#' @inheritParams rastersOverTime
#'
#' @param mapLegend           The legend text to add to the leaflet map.
#' @param shpStudyRegionName  Name of the study area region (from \code{rctRasterList}).
#' @param maxAge              Maximum simulation age.
#' @param zoom                Initial leaflet zoom.
#'
#' @return  Reactive polygon selected by the user with the \code{polygonChooser} module.
#'          Invoked for the side-effect of creating shiny server and ui components. # TODO: reword
#'
#' @author Mateusz Wyszynski
#' @author Alex Chubaty
#' @export
#' @importFrom graphics axis barplot
#' @importFrom leaflet addEasyButton addLegend addMeasure addMiniMap addPolygons
#' @importFrom leaflet addLayersControl addPopups addProviderTiles
#' @importFrom leaflet clearPopups colorFactor easyButton JS
#' @importFrom leaflet fitBounds layersControlOptions leaflet leafletOptions leafletOutput
#' @importFrom leaflet leafletProxy providerTileOptions renderLeaflet tileOptions
#' @importFrom raster cellFromXY crs extract filename hist maxValue ncell
#' @importFrom raster rowColFromCell xmax xmin ymax ymin
#' @importFrom reproducible asPath Cache
#' @importFrom shiny animationOptions br callModule h4 isolate observe reactive renderPlot tagList
#' @importFrom shinydashboard box
#' @importFrom sp SpatialPoints spTransform
#' @importFrom SpaDES.core paddedFloatToChar
#' @rdname timeSeriesofRasters
#'
timeSeriesofRasters <- function(input, output, session, rctRasterList, rctUrlTemplate,
                                rctPolygonList, defaultPolyName = NULL, shpStudyRegionName = NULL,
                                colorPalette, maxAge, zoom = 5, mapLegend = "",
                                mapTitle = "", sliderTitle = "", histTitle = "",
                                nPolygons, nRasters, rasterStepSize = 10) {

  rctChosenPolyName <- reactive({
    assertthat::assert_that(is.list(rctPolygonList())) ## TODO: test structure of the list, etc.

    polyList <- rctPolygonList()

    ## the full study region, using leaflet projection (used for map only here)
    shpStudyRegion <- if (is.null(shpStudyRegionName)) {
      polyList[[1]][["crsLFLT"]][["shpStudyRegion"]]
    } else {
      polyList[[shpStudyRegionName]][["crsLFLT"]][["shpStudyRegion"]]
    }

    ## the sub study region, using leaflet projection (used for map only here)
    subRegion <- if (is.null(shpStudyRegionName)) {
      polyList[[1]][["crsLFLT"]][["shpSubStudyRegion"]]
    } else {
      polyList[[shpStudyRegionName]][["crsLFLT"]][["shpSubStudyRegion"]]
    }

    leafMap <- leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10)) %>%
      addProviderTiles("Thunderforest.OpenCycleMap", group = "Open Cycle Map",
                       options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
      addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI World Imagery",
                       options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
      addLegend(position = "bottomright", pal = colorPalette, values = 1:maxAge, title = mapLegend) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit = "hectares",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      addEasyButton(easyButton(
        icon = "fa-map", title = "Zoom to focal area",
        onClick = JS(paste0("function(btn, map){ map.fitBounds([[",
                            ymin(subRegion), ", ", xmin(subRegion), "], [",
                            ymax(subRegion), ", ", xmax(subRegion), "]])}")))) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Zoom out to full study area",
        onClick = JS(paste0("function(btn, map){ map.setView([",
                            mean(c(ymin(shpStudyRegion), ymax(shpStudyRegion))), ", ",
                            mean(c(xmin(shpStudyRegion), xmax(shpStudyRegion))), "],",
                            zoom, ")}")))) %>%
      addMiniMap(tiles = leaflet::providers$OpenStreetMap, toggleDisplay = TRUE) %>%
      fitBounds(xmin(subRegion), ymin(subRegion), xmax(subRegion), ymax(subRegion))

    ## this module will return a reactive value:
    rctChosenPolName <- callModule(rastersOverTime, "rastersOverTime",
                                   rctRasterList = rctRasterList,
                                   rctUrlTemplate = rctUrlTemplate,
                                   rctPolygonList = rctPolygonList,
                                   defaultPolyName = defaultPolyName,
                                   map = leafMap,
                                   colorPalette = colorPalette(0:maxAge),
                                   histTitle = histTitle,
                                   sliderTitle = sliderTitle,
                                   mapTitle = mapTitle,
                                   nPolygons = nPolygons,
                                   nRasters = nRasters,
                                   rasterStepSize = rasterStepSize) # from global variable summaryInterval

    return(rctChosenPolName())
  })

  return(rctChosenPolyName)
}
