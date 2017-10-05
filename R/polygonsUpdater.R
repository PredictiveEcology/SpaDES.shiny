#' Polygons Updater Module
#'
#' @description Creates a shiny module which uses \code{\link[leaflet]{leafletProxy}}
#'              to update polygons displayed on a leaflet map.
#'
#' @param input    Shiny server input object.
#' @param output   Shiny server output object.
#' @param session  Shiny server session object.
#' @param proxy    Leaflet proxy which manages a connected leaflet map.
#' @param group    Group of polygons we want to update.
#' @param polygons Reactive value which contains a set of polygons which should
#'                 be displayed on the leaflet map.
#' @param ...      Additional parameters of \code{\link[leaflet]{addPolygons}} function.
#'
#' @return None. Invoked for the side-effect of creating an observer, which
#'         manages current set of polygons on a leaflet map using leaflet proxy.
#'
#' @importFrom leaflet addPolygons
#' @importFrom shiny observeEvent
#'
#' @author Mateusz Wyszynski
#'
#' @rdname polygonsUpdater
#'
#' @export
polygonsUpdater <- function(input, output, session, proxy, polygons, group = "group", ...) {
  observeEvent(polygons(), {
    proxy %>%
      clearGroup(group = group) %>%
      addPolygons(data = polygons(), group = group, ...)
  })
}
