#' Clump module
#'
#' @description Shiny module used ...
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#'
#' @return Shiny module UI.
#'
#' @author Damian Rodziewicz
#' @export
#' @importFrom shiny NS tagList numericInput
#' @rdname clumpMod2
clumpMod2UI <- function(id) {
  ns <- NS(id)

  tagList(
    numericInput(ns("PatchSize33"), value = 500, min = 100, max = NA,
                 label = paste0("Type patch size in hectares that defines 'Large', ",
                                "(numbers below 100 will not work)")
    )
  )
}

#' Clump Module
#'
#' @description Shiny module used to ... # TODO: documentation needed
#'
#' @param input Shiny server input object.
#' @param output Shiny server output object.
#' @param session Shiny server session object.
#' @param tsf ... reactive
#' @param vtm ... reactive
#' @param currentPolygon ...
#' @param cl ...
#' @param ageClasses ...
#' @param patchSize ...
#' @param sizeInHa ...
#' @param cacheRepo ... reactive
#' @param largePatchesFn ...
#' @param countNumPatches ...
#'
#' @return Shiny module UI.
#'
#' @importFrom archivist addTagsRepo
#' @importFrom shiny withProgress setProgress
#' @author Damian Rodziewicz
#' @export
#' @rdname clumpMod2
clumpMod2 <- function(input, output, session, tsf, vtm, currentPolygon, cl,
                      ageClasses, patchSize, sizeInHa, cacheRepo,
                      largePatchesFn, countNumPatches) {
  clumps <- reactive({
    patchSize <- as.integer(input$PatchSize33)

    message(paste("Running largePatchesFn"))
    shiny::withProgress(message = "Calculation in progress",
                        detail = "This may take a while...", value = 0, {
                          args <- list(largePatchesFn, timeSinceFireFiles = tsf,
                                       vegTypeMapFiles = vtm,
                                       cl = if (tryCatch(is(cl, "cluster"),
                                                         error = function(x) FALSE)) cl,
                                       polygonToSummarizeBy = currentPolygon,
                                       ageClasses = ageClasses,
                                       countNumPatches = countNumPatches,
                                       cacheRepo = cacheRepo,
                                       debugCache = "complete",
                                       omitArgs = "cl")
                          args <- args[!unlist(lapply(args, is.null))]
                          largePatches <- do.call(Cache, args)
                          shiny::setProgress(1)
    })
    message(paste("  Finished largePatchesFn"))

    return(list(Clumps = largePatches[sizeInHa > patchSize], patchSize = patchSize))
  })

  return(clumps)
}
