pasteListArgumentAndItsName <- function(argumentName, list, sep) {
  paste(argumentName, list[argumentName], sep = sep)
}

#' @importFrom raster extract
extractValuesWithNames <- function(polygons, spatialPoints, extractedValues) {
  extracted <- polygons %>% extract(spatialPoints) # nolint

  if (!is.null(extractedValues)) {
    extracted <- extracted[extractedValues]
  }

  names(extracted) %>% sapply(pasteListArgumentAndItsName,
                              list = extracted, sep = ": ")
}

#' @importFrom leaflet addPopups clearPopups
#' @importFrom sp SpatialPoints
#' @importFrom raster crs extract
displayPopupWithSummary <- function(x, y, proxy, raster, polygons,
                                    rasterValueLabel = "Raster value: %s",
                                    extractedValues = NULL) {
  spatialPointFromClick <- SpatialPoints(cbind(x, y), proj4string = crs(polygons))

  valueExtractedFromRaster <- extract(raster, spatialPointFromClick)

  rasterInformation <- if (!is.na(valueExtractedFromRaster)) {
    paste(sprintf(rasterValueLabel, valueExtractedFromRaster), "<br>")
  } else {
    ""
  }

  polygonInformation <- paste(
    extractValuesWithNames(polygons, spatialPointFromClick, extractedValues),
    collapse = "<br>"
  )

  popupContent <- paste0(rasterInformation, polygonInformation, "<br>",
                         "Lat/Long: ", round(y, 4), ", ", round(x, 4))

  proxy %>% clearPopups() %>% addPopups(x, y, popup = popupContent) # nolint
}

#' Summary popups (shiny module)
#'
#' Add popups with polygon and raster summaries on a leaflet map.
#'
#' @note This is a server-only module with no UI component.
#'
#' @param input            Shiny server input object.
#' @param output           Shiny server output object.
#' @param session          Shiny server session object.
#' @param proxy            Leaflet proxy which manages a connected leaflet map.
#' @param click            Reactive value with click on shape input from leaflet map.
#' @param rast             Reactive value with raster to summarize by.
#' @param polys            Reactive value with current set of polygons on map
#' @param rasterValueLabel String with description of raster value. Uses \code{sprintf},
#'                         so must include exactly one \code{\%s} in order to display raster value.
#'                         Default is a string \code{"Raster value: \%s"}.
#' @param extractedValues List of attributes from \code{SpatialPolygonDataFrame}
#'                        which should be included in popup summary.
#'                        When \code{NULL} (default) all attributes are included.
#'
#' @return None. Invoked for the side-effect of creating a shiny observer.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom shiny observe
#' @importFrom sp SpatialPoints
#' @importFrom raster crs extract
#' @importFrom leaflet addPopups clearPopups
#' @rdname summaryPopups
summaryPopups <- function(input, output, session, proxy, click, rast, polys,
                          rasterValueLabel = "Raster value: %s", extractedValues = NULL) {
  observe({
    req(click())

    displayPopupWithSummary(x = click()$lng, y = click()$lat, proxy = proxy,
                            raster = rast(), polygons = polys(),
                            rasterValueLabel = rasterValueLabel,
                            extractedValues = extractedValues)
  })
}
