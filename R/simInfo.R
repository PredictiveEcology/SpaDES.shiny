#' \code{SpaDES} simulation information (parent) module
#'
#' @template id
#'
#' @author Olivia Sung, Alex Chubaty, Greyson Wang
#' @export
#' @importFrom shiny fluidRow NS tabPanel
#' @importFrom shinydashboard tabBox
#' @rdname simInfo
simInfoUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    tabBox(
      width = 12,
      tabPanel("Module Diagram", simModuleDiagramUI(ns("moduleDiagram"))),
      tabPanel("Object Diagram", simObjectDiagramUI(ns("objectDiagram"))),
      tabPanel("Event Diagram", simEventDiagramUI(ns("eventDiagram")))
    )
  )
}

#' @template input
#' @template output
#' @template session
#' @template sim
#'
#' @export
#' @importFrom shiny callModule
#' @rdname simInfo
simInfo <- function(input, output, session, sim) {
  callModule(simModuleDiagram, "moduleDiagram", sim)
  callModule(simObjectDiagram, "objectDiagram", sim)
  callModule(simEventDiagram, "eventDiagram", sim)
}

#' \code{SpaDES} module diagram module
#'
#' @template id
#'
#' @author Alex Chubaty and Greyson Wang (module)
#' @export
#' @importFrom shiny h3 imageOutput NS p tagList
#' @rdname simModuleDiagram
simModuleDiagramUI <- function(id) {
  ns <- NS(id)

  uiOut <- tagList()

  uiOut$diagramTitle <- h3("Dependency graph (simplified)")

  uiOut$diagramDescription <- p(paste(
    "A network diagram illustrating the simplified module",
    "dependencies of a simulation.",
    "Arrows between modules indicate at least one data object",
    "passed from one module to the other."))

  uiOut$diagram <- imageOutput(ns("modDiag"), height = 750)

  return(uiOut)
}

#' @template input
#' @template output
#' @template session
#' @template sim
#'
#' @export
#' @importFrom shiny renderPlot
#' @importFrom igraph graph_from_data_frame layout_in_circle
#' @importFrom SpaDES.core moduleDiagram
#' @rdname simModuleDiagram
simModuleDiagram <- function(input, output, session, sim) {
  output$modDiag <- renderPlot({
    clearPlot()
    moduleDiagram(sim)
  })
}

#' \code{SpaDES} object diagram
#'
#' @template id
#'
#' @author Alex Chubaty and Greyson Wang (module)
#' @export
#' @importFrom DiagrammeR DiagrammeROutput
#' @importFrom shiny h3 NS p tagList
#' @rdname simObjectDiagram
simObjectDiagramUI <- function(id) {
  ns <- NS(id)

  uiOut <- tagList()

  uiOut$title <- h3("Summary of the data objects shared among modules.")

  uiOut$description <- p(paste(
    "A sequence diagram illustrating the data object dependencies",
    "of a simulation.",
    "Arrows between modules indicate at least one data object",
    "passed from one module to the other."
  ))

  uiOut$diagram <- DiagrammeR::DiagrammeROutput(ns("objectDiagram"), height = 1500)

  return(uiOut)
}

#' @template input
#' @template output
#' @template session
#' @template sim
#'
#' @export
#' @importFrom DiagrammeR renderDiagrammeR
#' @importFrom SpaDES.core objectDiagram
#' @rdname simObjectDiagram
simObjectDiagram <- function(input, output, session, sim) {
  output$objectDiagram <- DiagrammeR::renderDiagrammeR({
    objectDiagram(sim)
  })
}

#' Simulation event diagram shiny module
#'
#' @template id
#'
#' @author Alex Chubaty and Greyson Wang (module)
#' @export
#' @importFrom DiagrammeR DiagrammeROutput
#' @importFrom shiny h3 NS p tagList
#' @rdname simEventDiagram
simEventDiagramUI <- function(id) {
  ns <- NS(id)

  out <- tagList()

  out$title <-  h3("Summary of the simulation event sequence.")

  out$description <- p(paste(
    "Simulation time is presented on the x-axis.",
    "Each module appears in a color-coded row,",
    "within which each event for that module is displayed",
    "corresponding to the sequence of events for that module.",
    "Note that only the start time of the event is meaningful is",
    "this figure:",
    "the width of the bar associated with a particular module's",
    "event DOES NOT correspond to an event's 'duration'."
  ))

  out$diagram <- DiagrammeROutput(ns("eventDiagram"), height = 1500)

  return(out)
}

#' @template input
#' @template output
#' @template session
#' @template sim
#'
#' @export
#' @importFrom DiagrammeR renderDiagrammeR
#' @rdname simEventDiagram
simEventDiagram <- function(input, output, session, sim) {
  output$eventDiagram <- DiagrammeR::renderDiagrammeR({
    eventDiagram(sim)
  })
}

#' Detailed \code{SpaDES} module info module
#'
#' TODO: needs documentation
#'
#' @template id
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname moduleInfo
moduleInfoUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("allModuleInfo"))
}

#' @template input
#' @template output
#' @template session
#' @template sim
#' @param data     Logical. \code{TRUE} indicates to use the Rmd file for the
#'                 module's data (\file{moduleName/moduleName_data.Rmd}), if present.
#'                 \code{FALSE} uses the module's Rmd file (\file{moduleName/moduleName.Rmd}).
#'
#' @export
#' @importFrom shiny actionLink div fluidRow includeMarkdown p renderUI tagList
#' @importFrom shinyBS bsModal
#' @importFrom shinydashboard box
#' @importFrom SpaDES.core depends modulePath modules
#' @rdname moduleInfo
moduleInfo <- function(input, output, session, sim, data = FALSE) {
  output$allModuleInfo <- renderUI({
    fluidRow(
      tagList(lapply(modules(sim), function(module) {
        m <- slot(depends(sim), "dependencies")[[module]]
        rmdFileName <- if (data) {
          paste0(module, "_data.Rmd")
        } else {
          paste0(module, ".Rmd")
        }
        rmdFile <- file.path(modulePath(sim), module, rmdFileName)
        box(title = module, width = 12, status = "success", collapsible = TRUE,
            div(
              p(paste("Description:", slot(m, "description"))),
              p(paste("Keywords:", paste(slot(m, "keywords"), collapse = ", "))),
              p(paste("Authors:", paste(slot(m, "authors"), collapse = "; "))),
              p(paste("Version:", slot(m, "version"))),
              p("Documentation:", actionLink(paste0(module, "_Rmd"), "Rmd"))
              ## TO DO: add more metadata as required
            ),
            bsModal(module, basename(rmdFile),
                    trigger = paste0(module, "_Rmd"), size = "large",
                    tryCatch(suppressWarnings(shiny::includeMarkdown(rmdFile)),
                             error = function(e) "NOTE: no Rmd file supplied for this module."))
        )
      }))
    )
  })
}

#' \code{SpaDES} module parameter values module
#'
#' TODO: needs documentation
#'
#' @template id
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname moduleParams
moduleParamsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("moduleParamValues"))
}

#' @template input
#' @template output
#' @template session
#' @template sim
#'
#' @export
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny fluidRow tagList
#' @importFrom shinydashboard box
#' @rdname moduleParams
moduleParams <- function(input, output, session, sim) {
  output$moduleParamValues <- renderUI({
    fluidRow(
      tagList(lapply(modules(sim), function(module) {
        box(title = module, width = 12, status = "success", collapsible = TRUE,
            paramList <- params(sim)[[module]],
            paramsDF <- data.frame(
              Parameter = as.character(names(paramList)),
              Value = as.character(unname(unlist(paramList))),
              stringsAsFactors = FALSE),
            renderDataTable(dataTableOutput(paramsDF))
        )
      }))
    )
  })
}

#' Module data sources information module
#'
#' Display the rendered Rmarkdown content of the module's data description file
#' (\file{moduleName/moduleName_data.Rmd}).
#'
#' @template id
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny fluidRow h4 NS p tags
#' @importFrom shinydashboard box
#' @rdname dataInfo
dataInfoUI <- function(id) {
  ns <- NS(id)

  moduleInfoUI(ns("allDataInfo"))
}

#' @template input
#' @template output
#' @template session
#' @template sim
#'
#' @export
#' @importFrom shiny callModule
#' @rdname dataInfo
dataInfo <- function(input, output, session, sim) {
  callModule(moduleInfo, "allDataInfo", sim, data = TRUE)
}
