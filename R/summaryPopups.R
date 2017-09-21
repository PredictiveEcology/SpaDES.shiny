pasteListArgumentAndItsName <- function(argumentName, list, sep) {
  paste(argumentName, list[argumentName], sep = sep)
}

extractValuesWithNames <- function(polygons, spatialPoints, extractedValues) {

  extracted <- polygons %>%
    extract(spatialPoints)

  if(!is.null(extractedValues)) {
    extracted <- extracted[extractedValues]
  }

  names(extracted) %>% sapply(pasteListArgumentAndItsName,
                              list = extracted, sep = ": ")
}

displayPopupWithSummary <- function(x=NULL, y=NULL, proxy, raster, polygons, rasterValueLabel = "Raster value: %s", extractedValues = NULL) {
  spatialPointFromClick <- SpatialPoints(cbind(x,y), proj4string = crs(polygons))

  valueExtractedFromRaster <- extract(raster, spatialPointFromClick)

  rasterInformation <- if(!is.na(valueExtractedFromRaster)) {
    paste(sprintf(rasterValueLabel, valueExtractedFromRaster), "<br>")
  } else {
    ""
  }

  polygonInformation <- paste(extractValuesWithNames(polygons, spatialPointFromClick, extractedValues), collapse = "<br>")

  popupContent <- paste0(rasterInformation,
                    polygonInformation, "<br>",
                    "Lat/Long: ", round(y,4),", ", round(x,4))

  proxy %>% clearPopups() %>% addPopups(x, y, popup = popupContent)
}

#' Summary Popups Shiny Module
#'
#' @description This creates a shiny module which adds popups with polygons and raster summary on a leaflet map.
#'
#' @param input    Shiny server input object
#' @param output   Shiny server output object
#' @param session  Shiny server session object
#' @param proxy Leaflet proxy which manages a connected leaflet map.
#' @param click Reactive value with click on shape input from leaflet map.
#' @param raster Reactive value with raster to summarize by.
#' @param polygons Reactive value with current set of polygons on map
#' @param rasterValueLabel String with description of raster value. Uses \code{sprintf}, so must include exactly one \code{%s}
#' in order to display raster value. Dafult is a string \code{"Raster value: %s"}.
#' @param extractedValues List of attributes from \code{SpatialPolygonDataFrame} which should be included in popup summary.
#' When \code{NULL} (default) all attributes are included.
#'
#' @return None. Invoked for the side-effect of creating a shiny observer.
#'
#' @importFrom shiny observe
#' @importFrom sp SpatialPoints
#' @importFrom raster crs extract
#' @importFrom leaflet clearPopups addPopups
#'
#' @author Mateusz Wyszynski
#'
#' @rdname summaryPopups
#'
#' @export
summaryPopups <- function(input, output, session, proxy, click, raster, polygons, rasterValueLabel = "Raster value: %s", extractedValues = NULL){
  observe({
    if (!is.null(click())) {
      polygons <- polygons()
      raster <- raster()
      displayPopupWithSummary(x = click()$lng, y = click()$lat, proxy, raster, polygons, rasterValueLabel, extractedValues)
    }
  })
}
