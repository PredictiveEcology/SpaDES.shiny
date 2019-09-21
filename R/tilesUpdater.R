#' Tiles updater (shiny module)
#'
#' Creates a shiny module which uses \code{\link[leaflet]{leafletProxy}}
#' to update tiles displayed on a leaflet map.
#'
#' @note This is a server-only module with no UI component.
#'
#' @template input
#'
#' @template output
#'
#' @template session
#'
#' @param proxy    leaflet proxy which manages a connected leaflet map.
#'
#' @param rctUrlTemplateSingleRaster Reactive value which contains the URL template with the tiles
#'                    (for a single raster) that should be added to the leaflet map as a result.
#'
#' @param tilesGroup  Group of rasters/tiles we want to update.
#'
#' @param addTilesParameters Additional parameters of \code{\link[leaflet]{addTiles}} function.
#'                           Parameters: \code{proxy}/\code{map},
#'                           \code{urlTemplate} and \code{group} should be provided
#'                           directly as \code{tilesUpdater} arguments using
#'                           \code{proxy}, \code{urlTemplate} and \code{tilesGroup}
#'                           parameter respectively.
#'                           If passed within \code{addTilesParameters}, these
#'                           arguments will be suppressed by the values passed
#'                           directly to \code{tilesUpdater}.
#'
#' @param addLayersControlParameters Additional parameters passed to
#'                                   \code{\link[leaflet]{addLayersControl}}.
#'
#' @return None. Invoked for the side-effect of sending information to leaflet proxy.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom leaflet addLayersControl addTiles clearGroup
#' @importFrom shiny observeEvent
#' @rdname tilesUpdater
tilesUpdater <- function(input, output, session, proxy, rctUrlTemplateSingleRaster, tilesGroup,
                         addTilesParameters = NULL, addLayersControlParameters = NULL) {
  observeEvent(rctUrlTemplateSingleRaster(), {
    addTilesNecessaryParameters <- list(map = proxy, #clearGroup(proxy, tilesGroup),
                                        urlTemplate = rctUrlTemplateSingleRaster(),
                                        group = tilesGroup)
    addTilesParameters[names(addTilesNecessaryParameters)] <- NULL
    addTilesParametersCombined <- c(addTilesNecessaryParameters, addTilesParameters)
    layersControlParamsCombined <- c(
      list(map = do.call(addTiles, addTilesParametersCombined)),
      addLayersControlParameters
    )
    do.call(addLayersControl, layersControlParamsCombined)
  })
}
