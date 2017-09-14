library(leaflet)

#' tilesUpdater Module
#'
#' @description This creates a shiny module which uses leaflet proxy (see \code{?leafletProxy}) to update tiles displayed on a leaflet map.
#'
#' @param input    Shiny server input object
#' @param output   Shiny server output object
#' @param session  Shiny server session object
#' @param proxy Leaflet proxy which manages a connected leaflet map.
#' @param urlTemplate Reactive value which contains an url with the tiles that should be added to the leaflet map as a result.
#' @param tilesGroup Group of rasters/tiles we want to update
#' @param addTilesParameters Additional parameters of \code{leaflet::addTiles} function. See \code{?addTiles} for reference. Parameters: \code{proxy}/\code{map}, code{urlTemplate} and \code{group} should be provided directly as \code{tilesUpdater} arguments using \code{proxy}, \code{urlTemplate} and \code{tilesGroup} parameter respectively. If passed within \code{addTilesParameters}, these arguments will be surpressed by the values passed directly to \code{tilesUpdater}.
#' @param addLayersControlParameters Additional parameters of \code{leaflet::addLayersControl} function. See \code{?addLayersControl} for reference.
#'
#' @author Mateusz Wyszynski
#'
#' @importFrom leaflet clearGroup addTiles addLayersControl
#'
#' @export
tilesUpdater <- function(input, output, session, proxy, urlTemplate, tilesGroup, addTilesParameters = NULL, addLayersControlParameters = NULL) {
  observeEvent(urlTemplate(), {
    addTilesNecessaryParameters <- list(map = clearGroup(proxy, tilesGroup), urlTemplate = urlTemplate(), group = tilesGroup)
    addTilesParameters[names(addTilesNecessaryParameters)] = NULL
    addTilesParametersCombined <- c(list(map = clearGroup(proxy, tilesGroup), urlTemplate = urlTemplate(), group = tilesGroup), addTilesParameters)
    addLayersControlParametersCombined <- c(list(map = do.call(addTiles, addTilesParametersCombined)), addLayersControlParameters)
    do.call(addLayersControl, addLayersControlParametersCombined)
  })
}

# tilesUpdater <- function(input, output, session, proxy, urlTemplate, tilesGroup, addTilesParameters) {
#   observeEvent(urlTemplate(), {
#     proxy %>%
#       clearGroup(tilesGroup) %>%
#       addTiles(urlTemplate=urlTemplate(),
#                group = tilesGroup,
#                option = tileOptions(tms = TRUE, minZoom = 1, maxZoom = 10, opacity = 0.8)) %>%
#       addLayersControl(options = layersControlOptions(autoZIndex = TRUE,
#                                                       collapsed = FALSE),
#                        baseGroups = c("Open Cycle Map", "ESRI World Imagery"),
#                        overlayGroups = c("Time since fire", "Fire return interval"))
#   })
# }
