#' Simulation outputs (graphs and figures) module
#'
#' TO DO: needs description
#'
#' @template id
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

#' @template input
#' @template output
#' @template session
#' @template sim
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
#' @template id
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny fluidRow NS plotOutput
#' @rdname initialMap
initialMapUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    plotOutput(ns("mapInit"))
  )
}

#' @template input
#' @template output
#' @template session
#' @template sim
#' @param mapID    Character string indicating the name of the map in the `simList` object.
#'
#' @export
#' @importFrom quickPlot clearPlot Plot setColors<-
#' @importFrom RColorBrewer brewer.pal
#' @importFrom SpaDES.core start
#' @rdname initialMap
initialMap <- function(input, output, session, sim, mapID) {
  output$mapInit <- renderPlot({
    switch(
      mapID,
      "massAttacksMap" = {
        map <- sim[[mapID]][[paste0("X", start(sim))]]
        setColors(map) <- brewer.pal(9, "Reds")
        mapTitle <- paste0("MPB red attacks (", start(sim), ")")
      },
      "pineMap" = {
        map <- sim[[mapID]][[1]]
        setColors(map) <- brewer.pal(9, "Greens")
        mapTitle <- paste0("Percent pine (", start(sim), ")")
      },
      "climateSuitabilityMap" = {
        map <- sim[[mapID]]
        setColors(map) <- rev(brewer.pal(9, "RdBu"))
        mapTitle <- paste0("MPB climatic suitability (", start(sim), ")")
      }
    )

    clearPlot()
    Plot(map, title = mapTitle)
    #Plot(demoArea, addTo = "map") ## TO DO: use study area from inside simList
  })
}
