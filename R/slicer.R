filterDataTable <- function(categoryValue, categoryName, dataTable) {
  dataTable[get(categoryName) == categoryValue][, !categoryName, with = FALSE]
}

getTableOfSubtables <- function(dataTable, categoryName) {
  categories <- dataTable[, get(categoryName)] %>% unique()

  dataTables <- categories %>%
    map(function(category) filterDataTable(category, categoryName, dataTable))

  data.table(category = categories, dataTable = dataTables)
}

#' Slicer Module
#'
#' @description Shiny module used to: \cr
#'              1. extract an m-dimensional data table
#'              from an n-dimensional data table, where n > m \cr
#'              2. simultanously, for each n-m dimension, create desired UI (e.g. tabs) \cr
#'              3. Do a summary (e.g. create a plot) based on extracted m-dimensional data table
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#'
#' @param data Data in form of a \code{data.table}. For each row from \code{uiSequence} argument.
#'             Both server function and ui function should receive the same data table.
#'
#' @param categoryValue Each time the data table is sliced (one dimension is cut off),
#'                      concrete value of the category is set. This argument stores this value.
#'
#' @param uiSequence A \code{data.table} of the form
#'                   \code{data.table(category = list_of_categories, ui = list_of_ui_actions)}.
#'                   Both lists should contain elements of type character.
#'                   Both server function and ui function should receive the same data table.
#'                   Currently there are two possible actions to perform: "tab" and "box".
#'                   Action "box": should be used only together with \pkg{shinydashboard}.
#'                   An example of proper \code{uiSequence} is
#'                   \code{data.table(category = c("Alliance", "Kingdom"), ui = c("tab", "box"))}
#'
#' @param uiFunction A summary module function UI. This function will be applied to
#'                   extracted m-dimensional data table. Should correspond to \code{serverFunction}
#'                   in \code{slicer}.
#'
#' @return Shiny module UI.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom shiny mainPanel tabPanel tabsetPanel
#' @importFrom shinydashboard box
#' @importFrom purrr pmap map
#' @importFrom magrittr %>%
#' @importFrom data.table data.table
#' @rdname slicer
slicerUI <- function(id, data, categoryValue, uiSequence,
                     uiFunction) {
  ns <- NS(id)

  if(nrow(uiSequence) == 0) {
    uiFunction(ns, data, categoryValue)
  } else {
    category <- uiSequence$category[[1]]
    subtables <- getTableOfSubtables(data, category)

    ui <- uiSequence$ui[[1]]

    switch(ui,
           "tab" = {
             tabPanelWithSlicerContent <- function(category, dataTable) {
               tabPanel(category,
                        slicerUI(ns(category), data = dataTable, categoryValue = category,
                                 uiSequence[-1, ], uiFunction))
             }

             tabPanels <-
               pmap(subtables, function(category, dataTable) tabPanelWithSlicerContent(category, dataTable))

             mainPanel(
               do.call(tabsetPanel, tabPanels)
             )
           },
           "box" = {
             boxWithSlicerContent <- function(category, dataTable) {
               shinydashboard::box(
                 width = 6,
                 background = "light-blue",
                 slicerUI(ns(category), data = dataTable, categoryValue = category, uiSequence[-1, ], uiFunction)
               )
             }

             pmap(subtables, function(category, dataTable) boxWithSlicerContent(category, dataTable))
           }
    )
  }
}

#' @param input    Shiny server input object.
#'
#' @param output   Shiny server output object.
#'
#' @param session  Shiny server session object.
#'
#' @param data Data in form of a \code{data.table}. For each row from \code{uiSequence} argument.
#'             Both server function and ui function should receive the same data table.
#'
#' @param uiSequence A \code{data.table} of the form
#'                   \code{data.table(category = list_of_categories, ui = list_of_ui_actions)}.
#'                   Both lists should contain elements of type character.
#'                   Both server function and ui function should receive the same data table.
#'                   Currently there are two possible actions to perform: "tab" and "box".
#'                   Action "box": should be used only together with \pkg{shinydashboard}.
#'                   An example of proper \code{uiSequence} is
#'                   \code{data.table(category = c("Alliance", "Kingdom"), ui = c("tab", "box"))}
#'
#' @param serverFunction A summary module server function. This function will be applied to
#'                       extracted m-dimensional data table. Should correspond to \code{uiFunction}
#'                       in \code{slicerUI}.
#'
#' @return Shiny module server function.
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'   library(shinydashboard)
#'   library(SpaDES.shiny)
#'   library(data.table)
#'
#'   DT <- data.table(Alliance = c("Last Alliance of Elves and Men",
#'                                 "Last Alliance of Elves and Men",
#'                                 "Last Alliance of Elves and Men",
#'                                 "Mordor",
#'                                 "Mordor",
#'                                 "Mordor",
#'                                 "Saruman"),
#'                    Race = c("Elves", "Men", "Men", "Orcs", "Orcs", "Nasguls", "Uruk-hai"),
#'                    City = c("Rivendel", "Rohan", "Gondor", "Mordor", "Moria", "Mordor", "Isengard"),
#'                    Forces = 22: 28)
#'
#'   uiSequence <- data.table(category = c("Alliance", "Race"), ui = c("tab", "box"))
#'
#'   server <-
#'     function(input, output, session) {
#'       callModule(slicer, "slicer", DT, uiSequence,
#'                  serverFunction = function() {
#'                    callModule(slider, "slider")
#'                  })
#'     }
#'
#'   ui <-
#'     dashboardPage(
#'       dashboardHeader(),
#'       dashboardSidebar(),
#'       dashboardBody(
#'         slicerUI("slicer", DT, "LOTR", uiSequence,
#'                  uiFunction = function(ns, data, categoryValue) {
#'                    sliderUI(ns("slider"), min = data[,min(Forces)], max = data[,sum(Forces)],
#'                             value = data[,min(Forces)], step = 1, label = categoryValue)
#'                  })
#'       )
#'     )
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
#' @importFrom purrr pmap map
#' @importFrom magrittr %>%
#' @rdname slicer
slicer <- function(input, output, session, data, uiSequence, serverFunction) {
  if(nrow(uiSequence) == 0) {
    serverFunction()
  }
  else {
    category <- uiSequence$category[[1]]
    subtables <- getTableOfSubtables(data, category)

    pmap(subtables, function(category, dataTable) {
      callModule(slicer, category, dataTable, uiSequence[-1, ], serverFunction)
    })
  }
}