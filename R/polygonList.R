#' Create a new \code{polygonList} object
#'
#' @param studyArea  A template \code{Spatial*} object whose projection, extent,
#'                   etc. will be used for the polygons being added.
#' @param ...        \code{SpatialPolygonsDataFrame} objects to be added.
#'
#' @export
#' @importFrom sp spTransform
#' @importFrom SpaDES.tools maskInputs
#' @importFrom raster crs
#' @rdname newPolygonList
polygonList <- function(studyArea, ...) {
  dots <- list(...)

  stopifnot(is(studyArea, "Spatial"), all(vapply(dots, is, logical(1), class2 = "Spatial")))

  polyList <- lapply(dots, function(x) {
    polySR <- x
    polySRsub <- maskInputs(x, studyArea)
    polyLFLT <- spTransform(x, proj4stringLFLT)
    polyLFLTsub <- spTransform(polySRsub, proj4stringLFLT)
    ## TODO: thin the lflt polygons

    list(
      crsSR = list(shpStudyRegion = polySR, shpStudySubRegion = polySRsub),
      crsLFLT = list(shpStudyRegion = polyLFLT, shpStudySubRegion = polyLFLTsub)
    )
  })

  class(polyList) <- "polygonList"
  polyList
}
