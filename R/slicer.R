#' Get subtable from a \code{data.table}
#'
#' @param datatable         A reactive \code{data.table} object.
#' @param chosenCategories  ...
#' @param chosenValues      ...
#'
#' @return A \code{data.table} object
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom data.table is.data.table setkeyv
#' @rdname getSubTable
getSubtable <- function(datatable, chosenCategories, chosenValues) {
  assert_that(is.data.table(datatable))

  if (NROW(chosenValues) == 0) {
    return(datatable)
  } else {
    #ids <- which(datatable[[chosenCategories[[1]]]] %in% chosenValues[[1]])
    #subtable <- datatable[ids]
    subtable <- setkeyv(datatable, chosenCategories[[1]])[chosenValues[[1]]]

    getSubtable(subtable, chosenCategories[-1], chosenValues[-1])
  }
}

#' Generate UI (internal)
#'
#' @param uiType  Character indicating the UI type; currently one of \code{"box"} or \code{"tab"}.
#'
#' @param categoriesValue  Single column \code{data.table} containing the names of the categories.
#'
#' @param ns  Namespace function.
#'
#' @importFrom purrr map
#' @importFrom shiny mainPanel tabPanel
#' @importFrom shinydashboard box
#' @keywords internal
#' @rdname generateUI
.generateUI <- function(uiType, categoriesValues, ns) {
  categoriesValues <- unlist(categoriesValues) %>% unname() %>% as.list()
  switch(uiType,
         "tab" = {
           tabPanelWithSlicerContent <- function(category) {
             tabPanel(category, slicerUI(ns(category)))
           }

           tabPanels <- map(categoriesValues, tabPanelWithSlicerContent)

           mainPanel(width = 12, do.call(tabsetPanel, tabPanels))
         },
         "box" = {
           boxWithSlicerContent <- function(category) {
             shinydashboard::box(
               width = 4, solidHeader = TRUE, collapsible = TRUE,
               title = category, slicerUI(ns(category))
             )
           }

           map(categoriesValues, boxWithSlicerContent)
         }
  )
}

#' Slicer shiny module
#'
#' One can imagine behaviour of this module in the following way:
#' A tree of height \code{m} is created.
#' We begin at the top of the tree with the entire \code{data.table}.
#' A category (column) from a \code{data.table} is chosen via \code{uiSequence}.
#' Each value of this fixed category determines a subtable of the \code{data.table}.
#' For each value choice a child node is created and receives the value choice.
#' Therefore, every child node implicitly receives a subtable corresponding to the
#' received value choice.
#' Child node also receives an information from user which category should be fixed next.
#' It then performs the same calculations as the top node, but based on the implicitly
#' received subtable.
#' Simultaneously, for each node desired UI (e.g., tabs) is created.
#' If no information about next category to fix is provided for a node, it assumes it is a leaf.
#' At the end, at each leaf, a summary function is applied.
#' Note that each leaf (and each node) receives the entire \code{data.table} and
#' the information about chosen values leading to this leaf (node).
#' Hence, summary can be based on implicitly determined subtable, but also based
#' on entire \code{data.table} or some subtable determined by a subset of chosen values.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @return Shiny module UI.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname slicer
slicerUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("recursiveUI"))
}

#' @param input      Shiny server input object.
#' @param output     Shiny server output object.
#' @param session    Shiny server session object.
#' @param datatable  Reactive value containing data in form of a \code{data.table}.
#'                   This \code{data.table} is not changed; its subtables are accessed
#'                   using \code{chosenCategories} and \code{chosenValues} arguments.
#'                   This is helpful, because the end summary function might require
#'                   information about entire \code{data.table}.
#'
#' @param categoryValue Each time the \code{data.table} is sliced (one dimension is cut off),
#'                      concrete value of the category is set. This argument stores this value.
#'
#' @param nSimTimes   Number of simulation timestamps there are.
#'
#' @param uiSequence  A \code{data.table} of the form
#'                    \code{data.table(category = list_of_categories, uiType = list_of_ui_actions)}.
#'                    Both lists should contain elements of type character.
#'                    The \code{category} column should contain names of the categories
#'                    which will be subsequently fixed. The \code{uiType} column
#'                    should contain corresponding UI which should be applied for each
#'                    category choice. Currently there are two possible UI
#'                    types to perform: "tab" and "box".
#'                    Type "box": should be used only together with \pkg{shinydashboard}.
#'                    An example of proper \code{uiSequence} is
#'                    \code{data.table(category = c("Alliance", "Kingdom"), uiType = c("tab", "box"))}.
#'
#' @param serverFunction A summary module server function. This function should take
#'                       three arguments: \code{datatable}, \code{chosenCategories} and
#'                       \code{chosenValues}. Inside the function there should be
#'                       a call to shiny module server function. See example section
#'                       and compare with \code{link[shiny]{callModule}}).
#'
#' @param uiFunction A summary module function UI. This function should take
#'                   one argument: \code{ns}. Inside the function there should be
#'                   a call to shiny module UI function. See example section.
#'
#' @param chosenCategories A list with categories names that were already chosen.
#'                         Default \code{NULL}.
#'
#' @param chosenValues A list with categories values that were already chosen.
#'                     Default \code{NULL}.
#'
#' @return Shiny module server function.
#'
#' @author Mateusz Wyszynski
#' @author Alex Chubaty
#' @export
#' @importFrom assertthat assert_that
#' @importFrom data.table data.table is.data.table
#' @importFrom magrittr %>%
#' @importFrom shiny callModule is.reactive mainPanel NS observeEvent renderUI tabPanel tabsetPanel
#' @importFrom shinydashboard box
#' @importFrom purrr map
#' @rdname slicer
slicer <- function(input, output, session, datatable, categoryValue, nSimTimes,
                   uiSequence, serverFunction, uiFunction, chosenCategories = NULL,
                   chosenValues = NULL) {
  assert_that(is.reactive(datatable), msg = "slicer(): datatable is not reactive")

  ns <- session$ns

  observeEvent({
    datatable
  }, {
    assert_that(is.data.table(datatable()),
                msg = "slicer(): observeEvent: datatable() is not a data.table")

    if (nrow(uiSequence) == 0) {
      serverFunction(datatable, chosenCategories, chosenValues, nSimTimes)

      output$recursiveUI <- renderUI(uiFunction(ns))
    } else {
      categoryName <- uiSequence$category[[1]]

      currentSubtable <- getSubtable(datatable(), chosenCategories, chosenValues)

      categoriesValues <- currentSubtable[, categoryName, with = FALSE] %>% unique()

      categoriesValues %>% map(function(value) {
        callModule(slicer, value, datatable, value, nSimTimes, uiSequence[-1, ],
                   serverFunction, uiFunction,
                   c(chosenCategories, list(categoryName)),
                   c(chosenValues, list(value)))
      })

      uiType <- uiSequence$uiType[[1]]

      output$recursiveUI <- renderUI(.generateUI(uiType, categoriesValues, ns))
    }
  })
}
