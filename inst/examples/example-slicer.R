#' \dontrun{
library(data.table)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(SpaDES.shiny)

DT <- reactive({
  data.table(
    Alliance = c("Last Alliance of Elves and Men",
                 "Last Alliance of Elves and Men",
                 "Last Alliance of Elves and Men",
                 "Mordor",
                 "Mordor",
                 "Mordor",
                 "Saruman"),
    Race = c("Elves", "Men", "Men", "Orcs", "Orcs", "Nazg\u00FBl", "Uruk-hai"),
    City = c("Rivendell", "Rohan", "Gondor", "Mordor", "Moria", "Mordor", "Isengard"),
    Forces = 22:28
  )
})

## EXAMPLE 1
uiSequence <- data.table(category = c("Alliance", "Race"), uiType = c("tab", "box"))

server <- function(input, output, session) {
  callModule(slicer, "slicer", datatable = DT, uiSequence = uiSequence,
             serverFunction = function(datatable, id, .dtFull) {
               callModule(slider, id,
                          min = .dtFull[, min(Forces)],
                          max = .dtFull[, sum(Forces)],
                          value = datatable[, min(Forces)],
                          step = 1,
                          label = "Forces")
             },
             uiFunction = function(id) {
               withSpinner(sliderUI(id))
             })
}

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lord of the Rings", tabName = "LOTR")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "LOTR", slicerUI("slicer"))
    )
  )
)

shinyApp(ui, server)
#' }
#'
#' \dontrun{
## EXAMPLE 2
uiSequence2 <- data.table(category = c("Alliance", "Race"), uiType = c("box", "tab"))

server2 <- function(input, output, session) {
  callModule(slicer, "slicer", datatable = DT, uiSequence = uiSequence2,
             serverFunction = function(datatable, id, .dtFull) {
               callModule(histogram, id, datatable[, Forces])
             },
             uiFunction = function(ns) {
               histogramUI(id, height = 300)
             })
}

ui2 <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lord of the Rings", tabName = "LOTR")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "LOTR", slicerUI("slicer"))
    )
  )
)

shinyApp(ui2, server2)
#' }
