#' Tiles Updater Module
#'
#' @description This creates a shiny module which uses leaflet proxy (see \code{\link[leaflet]{leafletProxy}})
#'              to update tiles displayed on a leaflet map.
#'
#' @param input    Shiny server input object
#'
#' @param output   Shiny server output object
#'
#' @param session  Shiny server session object
#'
#' @param proxy Leaflet proxy which manages a connected leaflet map.
#' @param urlTemplate Reactive value which contains an url with the tiles
#'                    that should be added to the leaflet map as a result.
#' @param tilesGroup Group of rasters/tiles we want to update
#' @param addTilesParameters Additional parameters of \code{\link[leaflet]{addTiles}} function.
#'                           Parameters: \code{proxy}/\code{map}, code{urlTemplate} and \code{group}
#'                           should be provided directly as \code{tilesUpdater} arguments using
#'                           \code{proxy}, \code{urlTemplate} and \code{tilesGroup} parameter respectively.
#'                           If passed within \code{addTilesParameters}, these arguments will be surpressed
#'                           by the values passed directly to \code{tilesUpdater}.
#' @param addLayersControlParameters Additional parameters of \code{\link[leaflet]{addLayersControl}} function.
#'
#' @return None. Invoked for the side-effect of sending information to leaflet proxy.
#'
#' @importFrom leaflet addLayersControl addTiles clearGroup
#' @importFrom shiny observeEvent
#'
#' @author Mateusz Wyszynski
#'
#' @rdname tilesUpdater
#'
#' @export
tilesUpdater <- function(input, output, session, proxy, urlTemplate, tilesGroup,
                         addTilesParameters = NULL, addLayersControlParameters = NULL) {
  observeEvent(urlTemplate(), {
    addTilesNecessaryParameters <- list(map = clearGroup(proxy, tilesGroup),
                                        urlTemplate = urlTemplate(),
                                        group = tilesGroup)
    addTilesParameters[names(addTilesNecessaryParameters)] <- NULL
    addTilesParametersCombined <- c(list(map = clearGroup(proxy, tilesGroup),
                                         urlTemplate = urlTemplate(),
                                         group = tilesGroup), addTilesParameters)
    addLayersControlParametersCombined <- c(
      list(map = do.call(addTiles, addTilesParametersCombined)),
      addLayersControlParameters
    )
    do.call(addLayersControl, addLayersControlParametersCombined)
  })
}