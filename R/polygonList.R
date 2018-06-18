#' @keywords internal
.polygonList <- function() {
  polyList <- list(
    list(
      crsSR = NULL,
      crsLFLT = NULL
    )
  )

  class(polyList) <- c("polygonList", is(list())) ## TODO: how to properly inherit S3 classes??
  polyList
}

#' Create a new \code{polygonList} object
#'
#' @param studyArea  A template \code{Spatial*} object whose projection, extent,
#'                   etc. will be used for the polygons being added.
#' @param ...        \code{SpatialPolygonsDataFrame} objects to be added.
#'
#' @export
#' @importFrom sp spTransform
#' @importFrom reproducible maskInputs
#' @importFrom raster crs
#' @rdname newPolygonList
polygonList <- function(studyArea, ...) {
  dots <- list(...)
  stopifnot(inherits(studyArea, "SpatialPolygons"),
            all(vapply(dots, is, logical(1), class2 = "SpatialPolygons")))

  polyList <- Cache(Map, x = dots, n = names(dots), f = function(x, n) {
    polySR <- tryCatch(Cache(postProcess, x = x, studyArea = studyArea, useSAcrs = TRUE,
                             postProcessedFilename = FALSE),
                          error = function(e) {
                            message("Error intersecting polygon ", n, " with studyArea.")
                            NULL
                          })

    polyLFLT <- tryCatch(Cache(spTransform, x = polySR, CRSobj = proj4stringLFLT),
                         error = function(e) {
                           message("Error transforming polygon ", n, " to leaflet projection.")
                           NULL
                         })
    list(
      crsSR = polySR,
      crsLFLT = polyLFLT
    )
  })

  class(polyList) <- c("polygonList", is(list())) ## TODO: how to properly inherit S3 classes??
  polyList
}
