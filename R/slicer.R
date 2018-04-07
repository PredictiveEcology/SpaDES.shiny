#' Get subtable from a \code{data.table}
#'
#' \code{getSubtableMem} provides a memoised version of \code{getSubtable}.
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
#' @importFrom stats na.omit
#' @rdname getSubtable
getSubtable <- function(datatable, chosenCategories, chosenValues) {
  # assert_that(is.data.table(datatable),
  #             all(vapply(chosenCategories, function(x) is.character(datatable[[x]]), logical(1))))

  if (NROW(chosenValues) == 0) {
    return(datatable)
  } else {
    len <- length(chosenCategories)
      # setkeyv(datatable, chosenCategories[[len]])
      # if (NROW(datatable) <= 1)
      #   subtable <- na.omit(datatable[chosenValues[[len]]])
    subtable <- datatable[datatable[[chosenCategories[[len]]]] == chosenValues[[len]]]
    if (NROW(subtable) == 1)
      subtable <- na.omit(subtable)

    getSubtable(subtable, chosenCategories[-1], chosenValues[-1])
  }
}

#' @export
#' @importFrom memoise memoise
#' @rdname getSubtable
getSubtableMem <- memoise::memoise(getSubtable)

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
#' @importFrom shinycssloaders withSpinner
#' @rdname slicer
slicerUI <- function(id) {
  ns <- NS(id)

  shinycssloaders::withSpinner(uiOutput(ns("slicedUI")))
}

#' @param input          Shiny server input object.
#'
#' @param output         Shiny server output object.
#'
#' @param session        Shiny server session object.
#'
#' @param datatable      A reactive \code{data.table}.
#'
#' @param uiSequence     A \code{data.table} with columns \code{category}, \code{uiType},
#'                       and (optionally) \code{possibleValues}.
#'                       Both lists should contain elements of type character.
#'                       The \code{category} column should contain names of the categories
#'                       which will be subsequently fixed.
#'                       The \code{uiType} column should contain corresponding UI
#'                       which should be applied for each category choice.
#'                       The \code{possibleValues} column should contain a list of
#'                       the possible values for \code{category}.
#'                       If not supplied, possible values for each level of the list
#'                       will be determined based on the data.
#'                       Currently there are two possible UI types to perform: "tab" and "box",
#'                       which both make use of \pkg{shinydashboard}.
#'
#' @param serverFunction A summary module server function.
#'                       This function should take, at minimum, the following arguments:
#'                       \code{datatable} and \code{id}.
#'                       Additonal named arguments are passed via \code{...}.
#'                       Users have access to the full data.table if they need it
#'                       (e.g., to calculate histogram breaks) via \code{.dtFull}.
#'                       Inside the function there should be a call to a shiny
#'                       module server function using the \code{id}.
#'                       See example section and compare with \code{link[shiny]{callModule}}).
#'
#' @param uiFunction     A summary module function UI taking one argument: \code{id}.
#'                       Note: the \code{id} value is generated internally.
#'                       Inside the function there should be a call to shiny module UI function.
#'                       See example section.
#'
#' @param ...            Additional arguments passed to \code{serverFunction}.
#'
#' @return Shiny module server function.
#'
#' @author Mateusz Wyszynski
#' @author Alex Chubaty
#' @export
#' @importFrom assertthat assert_that
#' @importFrom data.table data.table is.data.table
#' @importFrom magrittr %>%
#' @importFrom shiny callModule fluidRow is.reactive mainPanel NS observeEvent renderUI tabPanel
#' @importFrom shinydashboard box tabBox
#' @importFrom purrr map
#' @rdname slicer
#'
slicer <- function(input, output, session, datatable, uiSequence,
                   serverFunction, uiFunction, ...) {

  observeEvent(datatable(), {
    #assertthat::assert_that(is.data.table(datatable()))

    categories <- uiSequence$category
    possibleValues <- uiSequence$possibleValues

    dtFull <- datatable()
    dtList <- split(dtFull, by = categories, flatten = FALSE) ## nested list

    ## TODO: this is currently fixed at 3 levels but needs to be made general WITHOUT using recursion!!!
    ##       because of this, the examples currently do not work because they have 2 levels
#browser()
    ## server elements
    level1names <- if (is.null(possibleValues[[1]])) {
      names(dtList)
    } else {
      possibleValues[[1]]
    }

    for (x in seq_along(level1names)) {
      level2names <- if (is.null(possibleValues[[2]])) {
        names(dtList[[x]])
      } else {
        possibleValues[[2]]
      }
      for (y in seq_along(level2names)) {
        level3names <- if (is.null(possibleValues[[3]])) {
          names(dtList[[x]][[y]])
        } else {
          categoriesValues[[3]]
        }
        for (z in seq_along(level3names)) {
          getID <- function(x, y, z) {
            paste("slicedUI", level1names[x], level2names[y], level3names[z], sep = "-")
          }

          serverFunction(datatable = dtList[[x]][[y]][[z]], id = getID(x, y, z), ..., .dtFull = dtFull)
        }
      }
    }

    ## UI elements
    output$slicedUI <- renderUI({
      ns <- session$ns

      level1names <- if (is.null(possibleValues[[1]])) {
        names(dtList)
      } else {
        possibleValues[[1]]
      }
      outerTabPanels <- lapply(seq_along(level1names), function(x) {
        level2names <- if (is.null(possibleValues[[2]])) {
          names(dtList[[x]])
        } else {
          possibleValues[[2]]
        }
        innerTabPanels <- lapply(seq_along(level2names), function(y) {
          level3names <- if (is.null(possibleValues[[3]])) {
            names(dtList[[x]][[y]])
          } else {
            categoriesValues[[3]]
          }

          getID <- function(x, y, z) {
            paste("slicedUI", level1names[x], level2names[y], level3names[z], sep = "-")
          }

          tabPanel(level2names[y],
            fluidRow(
              lapply(seq_along(level3names), function(z) {
                shinydashboard::box(
                  width = 4, solidHeader = TRUE, collapsible = TRUE,
                  title = level3names[[z]], uiFunction(session$ns(getID(x, y, z)))
                )
              })
            )
          )
        })

        tabPanel(level1names[x],
          fluidRow(width = 12, do.call(tabBox, append(innerTabPanels, list(width = 12))))
        )
      })
      fluidRow(width = 12, do.call(tabBox, append(outerTabPanels, list(width = 12))))
    })
  })
}

