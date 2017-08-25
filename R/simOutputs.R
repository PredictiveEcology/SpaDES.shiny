#' Simulation outputs (graphs and figures) module
#'
#' TO DO: needs description
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny fluidRow NS tabPanel
#' @importFrom shinydashboard tabBox
#' @rdname simOutputs
simOutputsUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    tabBox(width = 12,
      tabPanel("Growth curve", plotOutput(ns("growthCurve")))
    )
  )
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param sim      A \code{simList} object
#'
#' @export
#' @importFrom graphics plot
#' @importFrom shiny renderPlot
#' @rdname simOutputs
simOutputs <- function(input, output, session, sim) {
  output$growthCurve <- renderPlot({
    plot(sim$mpbRedTopGrowthPlotGG)
  })
}

#' Initial map module
#'
#' TO DO: need description
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny fluidRow NS plotOutput
#' @rdname initialMap
initialMapUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    plotOutput(ns("map_init"))
  )
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param sim      A \code{simList} object
#' @param mapID    Character string indicating the name of the map in the
#'                 \code{simList} object.
#'
#' @export
#' @importFrom quickPlot clearPlot Plot setColors<-
#' @importFrom RColorBrewer brewer.pal
#' @rdname initialMap
initialMap <- function(input, output, session, sim, mapID) {
  output$map_init <- renderPlot({
    switch(
      mapID,
      "massAttacksMap" = {
        map <- sim[[mapID]][[paste0("X", start(sim))]]
        setColors(map) <- brewer.pal(9, "Reds")
        map_title <- paste0("MPB red attacks (", start(sim), ")")
      },
      "pineMap" = {
        map <- sim[[mapID]][[1]]
        setColors(map) <- brewer.pal(9, "Greens")
        map_title <- paste0("Percent pine (", start(sim), ")")
      },
      "climateSuitabilityMap" = {
        map <- sim[[mapID]]
        setColors(map) <- rev(brewer.pal(9, "RdBu"))
        map_title <- paste0("MPB climatic suitability (", start(sim), ")")
      }
    )

    clearPlot()
    Plot(map, title = map_title)
    Plot(demoArea, addTo = "map") ## TO DO: use studyArea fom inside simList
  })
}
