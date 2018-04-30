#' @keywords internal
.polygonList <- function() {
  polyList <- list(
    list(
      crsSR = list(shpStudyRegion = NULL, shpStudySubRegion = NULL),
      crsLFLT = list(shpStudyRegion = NULL, shpStudySubRegion = NULL)
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
#' @importFrom SpaDES.tools maskInputs
#' @importFrom raster crs
#' @rdname newPolygonList
polygonList <- function(studyArea, subStudyArea, ...) {
  dots <- list(...)
  stopifnot(inherits(studyArea, "SpatialPolygons"),
            all(vapply(dots, is, logical(1), class2 = "SpatialPolygons")))

  polyList <- Cache(Map, x = dots, n = names(dots), f = function(x, n) {
    polySR <- x
    polySRsub <- tryCatch(Cache(maskInputs, x = x, studyArea = subStudyArea),
                          error = function(e) {
                            message("Error intersecting polygon ", n, " with studyArea.")
                            NULL
                          })

    ## TODO: thin the lflt polygons
    polyLFLT <- tryCatch(Cache(spTransform, x = x, CRSobj = proj4stringLFLT),
                         error = function(e) {
                           message("Error transforming polygon ", n, " to leaflet projection.")
                           NULL
                         })
    polyLFLTsub <- tryCatch(Cache(spTransform, x = polySRsub, CRSobj = proj4stringLFLT),
                            error = function(e) {
                              message("Error transforming intersected polygon ", n,
                                      " to leaflet projection.")
                              NULL
                            })

    list(
      crsSR = list(shpStudyRegion = polySR, shpStudySubRegion = polySRsub),
      crsLFLT = list(shpStudyRegion = polyLFLT, shpStudySubRegion = polyLFLTsub)
    )
  })

  class(polyList) <- c("polygonList", is(list())) ## TODO: how to properly inherit S3 classes??
  polyList
}

#' Remove elements of a list whose subelements contain a NULL value
#'
#' @param x   A named list.
#' @param ... Additonal arguments for specific methods.
#'
#' @author Alex Chubaty
#' @export
rmNulls <- function(x, ...) UseMethod("rmNulls")

rmNulls.polygonList <- function(x) {
  plout <- x
  sapply(names(x), function(n1) {
    sapply(names(x[[n1]]), function(n2) {
      sapply(names(x[[n1]][[n2]]), function(n3) {
        if (is.null(x[[n1]][[n2]][[n3]])) {
          plout[[n1]] <<- NULL
        }
      })
    })
  })
  plout
}
