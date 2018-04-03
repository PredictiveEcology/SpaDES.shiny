#' Get subtable from a \code{data.table}
#'
#' @param datatable         A \code{data.table} object.
#' @param chosenCategories  ...
#' @param chosenValues      ...
#'
#' @return A \code{data.table} object.
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
    setkeyv(datatable, chosenCategories[[length(chosenCategories)]])
    subtable <- na.omit(datatable[chosenValues[[length(chosenValues)]]])

    getSubtable(subtable, chosenCategories[-1], chosenValues[-1])
  }
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
#' @param datatable  A reactive \code{data.table} whose subtables are accessed (but not modified)
#'                   using \code{chosenCategories} and \code{chosenValues} arguments.
#'                   This is helpful, because the end summary function might require
#'                   information about the entire \code{data.table}.
#'
#' @param categoryValue Each time the \code{data.table} is sliced (one dimension is cut off),
#'                      concrete value of the category is set. This argument stores this value.
#'
#' @param uiSequence  A \code{data.table} with columns \code{category}, \code{uiType},
#'                    and (optionally) \code{possibleValues}.
#'                    Both lists should contain elements of type character.
#'                    The \code{category} column should contain names of the categories
#'                    which will be subsequently fixed.
#'                    The \code{uiType} column should contain corresponding UI
#'                    which should be applied for each category choice.
#'                    The \code{possibleValues} column should contain a list of
#'                    the possible values for \code{category}.
#'                    Currently there are two possible UI types to perform: "tab" and "box".
#'                    Type "box": should be used only together with \pkg{shinydashboard}.
#'
#' @param serverFunction A summary module server function.
#'                       This function should take, at minimum, the following arguments:
#'                       \code{datatable}, \code{chosenCategories}, \code{chosenValues}.
#'                       Additonal named arguments are passed via \code{...}.
#'                       Inside the function there should be a call to a shiny
#'                       module server function.
#'                       See example section and compare with \code{link[shiny]{callModule}}).
#'
#' @param uiFunction     A summary module function UI. This function should take
#'                       one argument: \code{ns}. Inside the function there should be
#'                       a call to shiny module UI function. See example section.
#'
#' @param chosenCategories A list with categories names that were already chosen.
#'                         Default \code{NULL}.
#'
#' @param chosenValues A list with categories values that were already chosen.
#'                     Default \code{NULL}.
#'
#' @param ...          Additional arguments passed to \code{serverFunction}.
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
#'
slicer <- function(input, output, session, datatable, categoryValue, uiSequence,
                   serverFunction, uiFunction, chosenCategories = NULL,
                   chosenValues = NULL, ...) {
  observeEvent({
    datatable()
  }, {
    assertthat::assert_that(is.data.table(datatable()))

    if (nrow(uiSequence) == 0) {
      serverFunction(datatable(), chosenCategories, chosenValues, ...)

      output$recursiveUI <- renderUI(uiFunction(session$ns)) ## don't change the ns!
    } else {
      categoryName <- uiSequence$category[[1]]

      currentSubtable <- reactive(getSubtable(datatable(), chosenCategories, chosenValues))

      categoriesValues <- reactive({
        possVals <- unlist(uiSequence[1]$possibleValues)
        if (!is.null(possVals)) {
          out <- possVals
        } else {
          cst <- currentSubtable()
          out <- cst[, categoryName, with = FALSE] %>% unique() %>% unlist() %>% unname() # nolint
        }
        return(out)
      })

      map(categoriesValues(), function(value) {
        callModule(slicer, id = value,
                   datatable = currentSubtable,
                   categoryValue = value,
                   uiSequence = uiSequence[-1, ],
                   serverFunction = serverFunction,
                   uiFunction = uiFunction,
                   chosenCategories = c(chosenCategories, list(categoryName)),
                   chosenValues = c(chosenValues, list(value)), ...)
      })

      uiType <- uiSequence$uiType[[1]]

      output$recursiveUI <- renderUI({
        ns <- session$ns
        switch(uiType,
               "tab" = {
                 tabPanelWithSlicerContent <- function(category) {
                   tabPanel(category, slicerUI(ns(category)))
                 }

                 tabPanels <- map(categoriesValues(), tabPanelWithSlicerContent)

                 mainPanel(width = 12, do.call(tabsetPanel, tabPanels))
               },
               "box" = {
                 boxWithSlicerContent <- function(category) {
                   shinydashboard::box(
                     width = 4, solidHeader = TRUE, collapsible = TRUE,
                     title = category, slicerUI(ns(category))
                   )
                 }

                 map(categoriesValues(), boxWithSlicerContent)
               }
        )
      })
    }
  })
}
