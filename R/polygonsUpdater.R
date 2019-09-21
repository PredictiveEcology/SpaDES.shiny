#' Polygons updater (shiny module)
#'
#' Update polygons displayed on a leaflet map using \code{\link[leaflet]{leafletProxy}}.
#'
#' @note This is a server-only module with no UI component.
#'
#' @template input
#' @template output
#' @template session
#' @param proxy    Leaflet proxy which manages a connected leaflet map.
#' @param group    Group of polygons we want to update.
#' @param poly     A reactive containing a polygon to be displayed on the leaflet map.
#' @param ...      Additional parameters of \code{\link[leaflet]{addPolygons}} function.
#'
#' @return None. Invoked for the side-effect of creating an observer, which
#'         manages current set of polygons on a leaflet map using leaflet proxy.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom leaflet addPolygons clearGroup
#' @importFrom shiny observeEvent
#' @rdname polygonsUpdater
polygonsUpdater <- function(input, output, session, proxy, poly, group = "group", ...) {
  observeEvent(poly(), {
    proxy %>%
      clearGroup(group = group) %>%
      addPolygons(data = poly(), group = group, ...)
  })
}
