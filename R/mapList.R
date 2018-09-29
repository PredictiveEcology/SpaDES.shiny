#' The \code{mapList} class
#'
#' Contains a common system for organzing vector and raster
#' layers, principly for use with leaflet and shiny.
#'
#' @slot metadata    List of character names specifying which modules to load.
#'
#' @slot maps Named list of map-type objects (e.g., \code{sf}, \code{Raster*},
#'            \code{Spatial*}.
#'
#' @slot rasterTiles Paths to rasterTiles
#'
#' @slot CRS  The common crs of all layers
#' @slot rasterTemplate The empty raster with all metadata filled
#' @slot analyses    A data.table or data.frame of the types of analyses to perform
#'
#' @slot analysesData A data.table or data.frame of the results of the analyses
#'
#' @aliases mapList
#' @rdname mapList-class
#' @exportClass mapList
#' @importFrom data.table data.table
#' @importFrom raster crs raster
setClass(
  "mapList",
  slots = list(
    metadata = "data.table",
    maps = "environment",
    rasterTiles = "character",
    CRS = "CRS",
    rasterTemplate = "RasterLayer",
    analyses = "data.table",
    analysesData = "list"
  ),
  prototype = list(
    metadata = data.table(layerName = character(), layerType = character(),
                          sourceURL = character(),
                          columnNameForLabels = character(),
                          leafletVisible = logical(), studyArea = logical()),
    maps = new.env(),
    rasterTiles = character(),
    rasterTemplate = NULL,
    CRS = sp::CRS(),
    analyses = data.table::data.table(),
    analysesData = list()
  ),
  validity = function(object) {
    #if (is.na(object@simtimes$end)) {
    #  stop("simulation end time must be specified.")
    #} else {
    #  if (object@simtimes$start > object@simtimes$end) {
    #    stop("simulation end time cannot be before start time.")
    #  }
    #}
  }
)


#' Append a spatial object to mapList
#' @export
#' @examples
#' library(sp)
#' library(raster)
#' coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98,
#'                       59.9, 65.73, 63.58, 54.79, 59.9),
#'                     .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' StudyArea <- SpatialPolygons(list(Srs1), 1L)
#' crs(StudyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'
#' ml <- new("mapList")
#' appendMapList(StudyArea, ml, studyArea = TRUE, layerName = "newPoly")
#'
appendMapList <- function(object, ...) UseMethod("appendMapList")

#' @export
#' @importFrom reproducible prepInputs
appendMapList.default <- function(object = NULL, mapList = NULL,
                                  layerName = NULL, sourceURL = NULL,
                                  columnNameForLabels = character(),
                                  leafletVisible = TRUE, studyArea = FALSE, ...) {
  if (is.null(mapList)) {
    mapList <- new("mapList")
  }
  if (is.null(object)) {
    if (is.null(sourceURL)) {
      stop("Must provide either object or sourceURL")
    } else {
      object <- prepInputs(url = sourceURL)
    }
    mapList <- appendMapList(object, mapList = mapList, layerName = layerName,
                  sourceURL = sourceURL, columnNameForLabels = columnNameForLabels,
                  leafletVisible = leafletVisible, studyArea = studyArea)
  }

  mapList
}

#' @export
#' @importFrom reproducible fixErrors projectInputs postProcess
#' @importFrom data.table rbindlist
appendMapList.SpatialPolygons <- function(object, mapList = NULL, layerName = NULL,
                                          sourceURL = NULL,
                                          columnNameForLabels = NULL,
                                          leafletVisible = TRUE, studyArea = NULL, ...) {
  if (is.null(studyArea(mapList))) {
    if (is.na(crs(mapList))) {
      warning("There is no studyArea and no CRS")
    } else {
      object <- fixErrors(object)
      object <- projectInputs(object, targetCRS = crs(mapList))
    }
  } else {
    if (is.na(crs(mapList))) {
      warning("There is no studyArea and no CRS")
    } else {
      object <- postProcess(object, studyArea = studyArea(mapList))
    }
  }

  # Put map into map slot
  assign(layerName, object, envir = mapList@maps)

  b <- .singleMetadataNAEntry
  if (isTRUE(studyArea)) {
    if (!is.null(studyArea(mapList))) {
      message("mapList already has a studyArea; adding another one at rank ", length(studyArea(mapList)))
    } else {
      mapList@CRS <- raster::crs(object)
    }
    set(b, , "studyArea", TRUE)
  }
  if (!is.null(sourceURL))
    set(b, , "sourceURL", sourceURL)
  set(b, , "layerName", layerName)
  set(b, , "layerType", class(object))
  if (!is.null(columnNameForLabels)) {
    if (is(object, "SpatialPolygonsDataFrame")) {
      set(b, , "columnNameForLabels", columnNameForLabels)
    }
  }
  if (isFALSE(leafletVisible))
    set(b, , "leafletVisible", leafletVisible)

  mapList@metadata <- rbindlist(list(mapList@metadata, b), use.names = TRUE, fill = TRUE)
  return(mapList)
}

#' @importMethodsFrom raster crs
#' @importFrom raster crs
#' @exportMethod crs
setMethod("crs",
          signature = "mapList",
          function(x, ...) {
            if (!is.null(x@CRS))
              x@CRS
            else
              NA
})

#' @export
studyAreaName <- function(mapList, layer = 1) {
  if (sum(mapList@metadata$studyArea)) {
    mapList@metadata[studyArea == TRUE, layerName][layer]
  } else {
    NULL
  }
}

#' @export
studyArea <- function(mapList, layer = 1) {
  if (sum(mapList@metadata$studyArea)) {
    get(mapList@metadata[studyArea == TRUE, layerName][layer], mapList@maps)
  } else {
    NULL
  }
}

.singleMetadataNAEntry <-
  data.table::data.table(layerName = NA_character_, layerType = NA_character_,
             sourceURL = NA_character_,
             columnNameForLabels = NA_character_,
             leafletVisible = TRUE, studyArea = FALSE)

