#' \dontrun{
library(data.table)
library(shiny)
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
  callModule(slicer, "slicer", datatable = DT, categoryValue = "LOTR", ## ?categoryvalue?
             uiSequence = uiSequence,
             serverFunction = function(datatable, chosenCategories, chosenValues) {
               callModule(slider, "slider",
                          min = datatable[, min(Forces)],
                          max = datatable[, sum(Forces)],
                          value = datatable[, min(Forces)],
                          step = 1,
                          label = "Forces")
             },
             uiFunction = function(ns) {
               sliderUI(ns("slider"))
             })
}

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    slicerUI("slicer")
  )
)

shinyApp(ui, server)
#' }
#'
#' \dontrun{
## EXAMPLE 2
uiSequence2 <- data.table(category = c("Alliance", "Race"), uiType = c("box", "tab"))

server2 <- function(input, output, session) {
  callModule(slicer, "slicer", datatable = DT, uiSequence = uiSequence,
             serverFunction = function(datatable, chosenCategories, chosenValues) {
               callModule(histogram, "histogram", datatable[, Forces])
             },
             uiFunction = function(ns) {
               histogramUI(ns("histogram"), height = 300)
             })
}

ui2 <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    slicerUI("slicer")
  )
)

shinyApp(ui2, server2)
#' }
