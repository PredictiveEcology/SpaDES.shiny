#' Visualize \code{SpatialPolygons*} (shiny module)
#'
#' Display a \code{SpatialPolygons*} objects on a leaflet map.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @param ... Additional parameters passed to \code{link[leaflet]{leafletOutput}}.
#'
#' @return None. Invoked for the side-effect of generating UI for a leaflet map.
#'
#' @export
#' @importFrom shiny NS
#' @importFrom leaflet leafletOutput
#' @rdname visualizePolygons
visualizePolygonsUI <- function(id, ...) {
  ns <- NS(id)

  leafletOutput(ns("map"), ...)
}

#' @param input     Shiny server input object.
#' @param output    Shiny server output object.
#' @param session   Shiny server session object.
#' @param polys     Reactive value with polygons to visualize.
#' @param proxy     Proxy to a leaflet map on which polygons should be displayed.
#'                  See \code{\link[leaflet]{leafletProxy}}
#'
#' @return None. Invoked for the side-effect of rendering leaflet map.
#'
#' @export
#' @importFrom shiny renderPlot is.reactive
#' @importFrom leaflet leaflet addTiles addPolygons
#' @rdname visualizePolygons
visualizePolygons <- function(input, output, session, polys, proxy = NULL) {
  ns <- session$ns

  if (is.null(proxy)) {
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = polys(), group = ns("group"))
    })

    proxy <- leafletProxy("map")
    callModule(polygonsUpdater, "updater", proxy, polys, group = ns("group"))
  } else {
    callModule(polygonsUpdater, "updater", proxy, polys)
  }
}
