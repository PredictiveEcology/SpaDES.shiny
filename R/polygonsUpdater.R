#' Polygons Updater Module
#'
#' @description This creates a shiny module which uses leaflet proxy (see \code{?leafletProxy}) to update polygons displayed on a leaflet map.
#'
#' @param input    Shiny server input object
#' @param output   Shiny server output object
#' @param session  Shiny server session object
#' @param proxy Leaflet proxy which manages a connected leaflet map.
#' @param polygonsInput Reactive value which contains a set of polygons which should be displayed on the leaflet map
#' @param ... Additional parameters of \code{leaflet::addPolygons} function. See \code{?leaflet::addPolygons} for reference.
#'
#' @author Mateusz Wyszynski
#'
#' @importFrom leaflet addPolygons
#' @importFrom shiny observeEvent
#'
#' @rdname polygonsUpdater
#'
#' @export
polygonsUpdater <- function(input, output, session, proxy, polygonsInput, ...) {
  observeEvent(polygonsInput(), {
    proxy %>%
      addPolygons(data = polygonsInput()$polygon,
                  ...)
  })
}
