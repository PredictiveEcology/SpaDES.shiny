setOldClass("polygonList")

#' @keywords internal
.polygonList <- function() {
  polyList <- list(
    list(
      crsSR = NULL,
      crsLFLT = NULL
    )
  )

  class(polyList) <- "polygonList"
  polyList
}

#' Update polygon list
#'
#'
#' @param x   a named list
#' @param y   a named list
#'
#' @return A named list, with elements sorted by name.
#'          The values of matching elements in list `y`
#'          replace the values in list `x`.
#'
#' @note This is a temporary workaround until we resolve inheritance of S3 classes (see `.polygonList`).
#'
#' @author Alex Chubaty
#' @export
#' @importFrom SpaDES.core updateList
#' @rdname updateList
setMethod("updateList",
          signature = c("polygonList", "polygonList"),
          definition = function(x, y) {
            class(x) <- "list"
            class(y) <- "list"
            z <- updateList(x, y)
            class(z) <- "polygonList"
            return(z)
})

#' Create a new `polygonList` object
#'
#' @param studyArea  A template `Spatial*` object whose projection, extent,
#'                   etc. will be used for the polygons being added.
#' @param ...        `SpatialPolygonsDataFrame` objects to be added.
#'
#' @export
#' @importFrom sp spTransform
#' @importFrom reproducible maskInputs postProcess
#' @importFrom raster crs
#' @rdname newPolygonList
polygonList <- function(studyArea, ...) {
  dots <- list(...)
  stopifnot(inherits(studyArea, "SpatialPolygons"),
            all(vapply(dots, is, logical(1), class2 = "SpatialPolygons")))

  polyList <- Cache(Map, x = dots, n = names(dots), f = function(x, n) {
    polySR <- tryCatch(Cache(postProcess, x = x, studyArea = studyArea, useSAcrs = TRUE,
                             filename2 = FALSE),
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

  class(polyList) <- "polygonList"
  polyList
}
