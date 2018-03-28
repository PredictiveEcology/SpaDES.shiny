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
#' @param mapLegend           The legend text to add to the leaflet map.
#' @param shpStudyRegionName  Name of the study area region (from \code{rasterList})..
#' @param palette             Color palette for the rasters.
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
#' @importFrom leaflet layersControlOptions leaflet leafletOptions leafletOutput leafletProxy
#' @importFrom leaflet providerTileOptions renderLeaflet setView tileOptions
#' @importFrom shiny br callModule isolate observe reactive renderPlot
#' @importFrom sp SpatialPoints spTransform
#' @importFrom raster cellFromXY crs extract filename maxValue ncell rowColFromCell
#' @importFrom raster hist xmax xmin ymax ymin
#' @importFrom reproducible asPath Cache
#' @importFrom shiny animationOptions br callModule h4 isolate observe reactive renderPlot tagList
#' @importFrom shinydashboard box
#' @importFrom sp SpatialPoints spTransform
#' @importFrom SpaDES.core paddedFloatToChar
#' @rdname timeSeriesofRasters
#'
timeSeriesofRasters <- function(input, output, session, rasterList, polygonList,
                                defaultPolyName = NULL, shpStudyRegionName = NULL,
                                colorTable, palette, maxAge, zoom = 5,
                                sim = NULL, mapLegend = "",
                                mapTitle = "", sliderTitle = "", histTitle = "",
                                nPolygons, nRasters, rasterStepSize = 10) {

  observeEvent(polygonList, {
    assertthat::assert_that(is.list(polygonList)) ## TODO: test structure of the list, etc.

    ## the full study region, using leaflet projection (used for map only here)
    shpStudyRegion <- if (is.null(shpStudyRegionName)) {
      polys[[1]][["crsLFLT"]][["shpStudyRegion"]]
    } else {
      polys[[shpStudyRegionName]][["crsLFLT"]][["shpStudyRegion"]]
    }

    ## the sub study region, using leaflet projection (used for map only here)
    subRegion <- if (is.null(shpStudyRegionName)) {
      polys[[1]][["crsLFLT"]][["shpSubStudyRegion"]]
    } else {
      polys[[shpStudyRegionName]][["crsLFLT"]][["shpSubStudyRegion"]]
    }

    leafMap <- leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10)) %>%
      addProviderTiles("Thunderforest.OpenCycleMap", group = "Open Cycle Map",
                       options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
      addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI World Imagery",
                       options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
      addLegend(position = "bottomright", pal = palette, values = 1:maxAge, title = mapLegend) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit = "hectares",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      addEasyButton(easyButton(
        icon = "fa-map", title = "Zoom to focal area",
        onClick = JS(paste0("function(btn, map){ map.fitBounds([[", ymin(subRegion), ", ",
                            xmin(subRegion), "], [", ymax(subRegion), ", ", xmax(subRegion), "]])}")))) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Zoom out to full study area",
        onClick = JS(paste0("function(btn, map){ map.setView([",
                            mean(c(ymin(shpStudyRegion),
                                   ymax(shpStudyRegion))), ", ",
                            mean(c(xmin(shpStudyRegion),
                                   xmax(shpStudyRegion))), "], 5)}")))) %>%
      addMiniMap(tiles = leaflet::providers$OpenStreetMap, toggleDisplay = TRUE) %>%
      setView(mean(c(xmin(shpStudyRegion), xmax(shpStudyRegion))),
              mean(c(ymin(shpStudyRegion), ymax(shpStudyRegion))),
              zoom = zoom)# %>%
      # addPolygons(data = isolate(shpStudyRegion),
      #             group = "Fire return interval", # TODO: generalize this
      #             fillOpacity = 0.3, weight = 1, color = "blue",
      #             fillColor = ~colorFactor("Spectral", fireReturnInterval)(fireReturnInterval)) # TODO: generalize this
  })

  chosenPoly <- callModule(rastersOverTime, "rastersOverTime", rasterList = rasterList,
                           defaultPolyName = defaultPolyName, polygonList = polys,
                           map = leafMap,  colorTable = colorTable,
                           histTitle = histTitle, sliderTitle = sliderTitle, mapTitle = mapTitle,
                           nPolygons = nPolygons, nRasters = nRasters, rasterStepSize = 10,
                           sim = sim, cacheNotOlderThan = NULL)

  return(chosenPoly)
}
