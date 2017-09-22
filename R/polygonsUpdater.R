#' Polygons Updater Module
#'
#' @description This creates a shiny module which uses leaflet proxy (see \code{\link[leaflet]{leafletProxy}})
#'              to update polygons displayed on a leaflet map.
#'
#' @param input    Shiny server input object
#' @param output   Shiny server output object
#' @param session  Shiny server session object
#' @param proxy Leaflet proxy which manages a connected leaflet map.
#' @param polygons Reactive value which contains a set of polygons which should be displayed on the leaflet map
#' @param ... Additional parameters of \code{\link[leaflet]{addPolygons}} function.
#'
#' @return None. Invoked for the side-effect of creating an observer
#'         which manages current set of polygons on a leaflet map using leaflet proxy.
#'
#' @importFrom leaflet addPolygons
#' @importFrom shiny observeEvent
#'
#' @author Mateusz Wyszynski
#'
#' @rdname polygonsUpdater
#'
#' @export
polygonsUpdater <- function(input, output, session, proxy, polygons, ...) {
  observeEvent(polygons(), {
    proxy %>%
      addPolygons(data = polygons(),
                  ...)
  })
}
