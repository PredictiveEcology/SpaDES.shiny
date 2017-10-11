#' Raster Updater Module
#'
#' @description Creates a shiny module which uses \code{\link[leaflet]{leafletProxy}}
#'              to display \code{RasterLayer} object on a leaflet map. This is only
#'              suitable for small to medium sized rasters.
#'
#' @param input    Shiny server input object.
#'
#' @param output   Shiny server output object.
#'
#' @param session  Shiny server session object.
#'
#' @param proxy    Leaflet proxy which manages a connected leaflet map.
#'                 When \code{NULL} (default) a basic leaflet map is created
#'                 and raster is displayed on this map.
#'
#' @param raster   Reactive value containing \code{RasterLayer} object.
#'
#' @param group    Group of rasters we want to update
#'
#' @param ...      Additional parameters passed to \code{\link[leaflet]{addRasterImage}}
#'                 function.
#'
#' @return None. Invoked for the side-effect of sending information to leaflet proxy.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom leaflet addRasterImage clearGroup
#' @importFrom shiny observeEvent
#' @rdname rasterUpdater
rasterUpdater <- function(input, output, session, proxy, raster, group, ...) {
  observeEvent(raster, {
    proxy %>%
      clearGroup(group) %>%
      addRasterImage(raster, group = group, ...)
  })
}
