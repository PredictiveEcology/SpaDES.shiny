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
#' @param rctChosenPoly       The user-selected polygon.
#' @param defaultPolyName     Name of the polygon to use as the default for mapping.
#' @param mapLegend           The legend text to add to the leaflet map.
#' @param shpStudyRegionName  Name of the study area region (from \code{rctRasterList}).
#' @param shpStudyRegionLFLT  \code{SpatialPolygonDataFrame} for the study region.
#' @param maxAge              Maximum simulation age.
#' @param zoom                Initial leaflet zoom.
#' @param uploadOpts    A list of options for use with file uploads:
#'                      \code{auth} logical indicating whether user is authorized to upload;
#'                      \code{path} a directory path to use for file uploads;
#'                      \code{user} the current username (used for creating user-specific paths).
#'                      The default for all options is \code{NULL}, which means do not use.
#' @param rctStudyArea  A reactive \code{SpatialPolygons*} object for the whole study area region.
#' @param thinKeep      Proportion of points in polygons to keep when thinning or plotting.
#'                      See \code{\link[rmapshaper]{ms_simplify}}.
#'
#' @return  Reactive polygon selected by the user with the \code{polygonChooser} module.
#'          Invoked for the side-effect of creating shiny server and ui components. # TODO: reword
#'
#' @author Mateusz Wyszynski
#' @author Alex Chubaty
#' @export
#' @importFrom future future
#' @importFrom graphics axis barplot
#' @importFrom leaflet addEasyButton addLegend addMeasure addMiniMap addPolygons
#' @importFrom leaflet addLayersControl addPopups addProviderTiles
#' @importFrom leaflet clearPopups colorFactor easyButton JS
#' @importFrom leaflet fitBounds layersControlOptions leaflet leafletOptions leafletOutput
#' @importFrom leaflet leafletProxy providerTileOptions renderLeaflet tileOptions
#' @importFrom leaflet.extras enableTileCaching
#' @importFrom promises %...>%
#' @importFrom raster cellFromXY crs extract filename hist maxValue ncell
#' @importFrom raster rowColFromCell xmax xmin ymax ymin
#' @importFrom reproducible asPath Cache
#' @importFrom rmapshaper check_sys_mapshaper ms_simplify
#' @importFrom shiny animationOptions br callModule h4 isolate observe reactive renderPlot tagList
#' @importFrom shinydashboard box
#' @importFrom sp SpatialPoints spTransform
#' @importFrom SpaDES.core paddedFloatToChar updateList
#' @include polygonChooser.R
#' @include rastersOverTime.R
#' @rdname timeSeriesofRasters
#'
timeSeriesofRasters <- function(input, output, session, rctRasterList, rctUrlTemplate,
                                rctPolygonList, rctChosenPoly, defaultPolyName = NULL,
                                shpStudyRegionName = NULL, shpStudyRegionLFLT = NULL,
                                colorPalette, maxAge, zoom = 5, mapTilesDir = "www/",
                                mapLegend = "", mapTitle = "", sliderTitle = "", histTitle = "",
                                nPolygons, nRasters, rasterStepSize = 10,
                                uploadOpts = list(auth = NULL, path = NULL, user = NULL),
                                rctStudyArea = NULL, thinKeep = 0.05) {

  observe({
    polyList <- rctPolygonList()
    polyName <- rctChosenPoly()$selected
    shpStudyArea <- polyList[[1]][["crsLFLT"]]

    ## thin the study area polygon in a separate process and create leaflet map
    future({
      # TODO: speed up polygon mapping in leaflet (see #24)
      shpStudyAreaThinned <- Cache(rmapshaper::ms_simplify,
                                   input = shpStudyArea,
                                   keep = thinKeep,
                                   sys = rmapshaper::check_sys_mapshaper(verbose = FALSE))

      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10)) %>%
        enableTileCaching() %>%
        addProviderTiles("Stamen.Terrain", group = "Terrain Map",
                         options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
        addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI World Imagery",
                         options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
        addLegend(position = "bottomright", pal = colorPalette, values = 1:maxAge,
                  title = mapLegend) %>%
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "kilometers",
          primaryAreaUnit = "hectares",
          activeColor = "#3D535D",
          completedColor = "#7D4479") %>%
        addEasyButton(easyButton(
          icon = "fa-map", title = "Zoom to focal area",
          onClick = JS(paste0("function(btn, map){ map.fitBounds([[",
                              ymin(shpStudyArea), ", ", xmin(shpStudyArea), "], [",
                              ymax(shpStudyArea), ", ", xmax(shpStudyArea), "]])}")))) %>%
        addEasyButton(easyButton(
          icon = "fa-globe", title = "Zoom out to full study area",
          onClick = JS(paste0("function(btn, map){ map.setView([",
                              mean(c(ymin(shpStudyRegionLFLT), ymax(shpStudyRegionLFLT))), ", ",
                              mean(c(xmin(shpStudyRegionLFLT), xmax(shpStudyRegionLFLT))), "],",
                              zoom, ")}")))) %>%
        addMiniMap(tiles = leaflet::providers$OpenStreetMap, toggleDisplay = TRUE) %>%
        addPolygons(data = shpStudyAreaThinned, color = "blue",
                    group = "Selected Polygon",
                    ## use weight = 3 to be consistent with polygonUpdater module
                    fillOpacity = 0.0, weight = 3) %>%
        fitBounds(xmin(shpStudyArea), ymin(shpStudyArea), xmax(shpStudyArea), ymax(shpStudyArea))
    }) %...>% (function(leafMap) {
      callModule(rastersOverTime, "rastersOverTime",
                 rctRasterList = rctRasterList,
                 rctUrlTemplate = rctUrlTemplate,
                 rctPolygonList = reactive(polyList),
                 rctChosenPolyName = reactive(polyName),
                 map = leafMap,
                 mapTilesDir = mapTilesDir,
                 colorPalette = colorPalette(0:maxAge),
                 histTitle = histTitle,
                 sliderTitle = sliderTitle,
                 mapTitle = mapTitle,
                 nPolygons = nPolygons,
                 nRasters = nRasters,
                 rasterStepSize = rasterStepSize)
    })
  })
}
