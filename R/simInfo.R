#' \code{SpaDES} simulation information (parent) module
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
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

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param sim      A \code{simList} object
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
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#'
#' @author Alex Chubaty and Greyson Wang (module)
#' @export
#' @importFrom shiny h3 imageOutput NS p tagList
#' @rdname simModuleDiagram
simModuleDiagramUI <- function(id) {
  ns <- NS(id)

  ui_output <- tagList()

  ui_output$diagramTitle <- h3("Dependency graph (simplified)")

  ui_output$diagramDescription <- p(paste(
    "A network diagram illustrating the simplified module",
    "dependencies of a simulation.",
    "Arrows between modules indicate at least one data object",
    "passed from one module to the other."))

  ui_output$diagram <- imageOutput(ns("modDiag"), height = 750)

  return(ui_output)
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param sim      A \code{simList} object
#'
#' @export
#' @importFrom shiny renderPlot
#' @importFrom SpaDES.core moduleDiagram
#' @rdname simModuleDiagram
simModuleDiagram <- function(input, output, session, sim) {
  output$modDiag <- renderPlot({
    moduleDiagram(sim, vertex.size = 30)
  })
}

#' \code{SpaDES} object diagram
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#'
#' @author Alex Chubaty and Greyson Wang (module)
#' @export
#' @importFrom DiagrammeR DiagrammeROutput
#' @importFrom shiny h3 NS p tagList
#' @rdname simObjectDiagram
simObjectDiagramUI <- function(id) {
  ns <- NS(id)

  ui_output <- tagList()

  ui_output$title <- h3("Summary of the data objects shared among modules.")

  ui_output$description <- p(paste(
    "A sequence diagram illustrating the data object dependencies",
    "of a simulation.",
    "Arrows between modules indicate at least one data object",
    "passed from one module to the other."
  ))

  ui_output$diagram <- DiagrammeR::DiagrammeROutput(ns("objectDiagram"), height = 1500)

  return(ui_output)
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param sim      A \code{simList} object
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

#' Simulation event diagram module
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
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

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param sim      A \code{simList} object
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
#' TO DO: needs documentation
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname moduleInfo
moduleInfoUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("allModuleInfo"))
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param sim      A \code{simList} object
#'
#' @export
#' @importFrom shiny actionLink div fluidRow includeMarkdown p renderUI tagList
#' @importFrom shinyBS bsModal
#' @importFrom shinydashboard box
#' @importFrom SpaDES.core depends modulePath
#' @rdname moduleInfo
moduleInfo <- function(input, output, session, sim) {
  output$allModuleInfo <- renderUI({
    fluidRow(
      tagList(lapply(modules(sim), function(module) {
        m <- slot(depends(sim), "dependencies")[[module]]
        rmdFile <- file.path(modulePath(sim), module, paste0(module, ".Rmd"))
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
                    includeMarkdown(rmdFile))
        )
      }))
    )
  })
}

#' \code{SpaDES} module parameter values module
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#'
#' TO DO: needs documentation
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname moduleParams
moduleParamsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("moduleParamValues"))
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param sim      A \code{simList} object
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
#' TO DO: needs to be generalized
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny fluidRow h4 NS p tags
#' @importFrom shinydashboard box
#' @rdname dataInfo
dataInfoUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(title = "Climate Suitability", width = 12, status = "success",
        collapsible = TRUE, collapsed = TRUE,
        h4("Summary"),
        p("This module imports several MPB climate suitability scenarios (using four different indices: `S`, `L`, `R`, `G`) for use as model drivers.",
          "The Logan suitability index (`L`) is based on summer temperatures (Logan et al. 2003).",
          "The Regniere suitability index (`R`) is based on MPB cold tolerance (i.e., winter survival) (Regniere et al. 2007).",
          "The Safranyik suitability index (`S`) is based on aspects of both summer temperatures and winter survival (Safranyik et al. 1975).",
          "Finally, the composite SLR index (`G`) takes the geometric mean of the `S`, `L`, and `R` models.",
          "These are described in further detail in Nealis et al. (2008 and 2014) and in their respective publications cited above."),
        p("BioSim was used to generate the maps (see Bentz et al. 2010; Logan et al. 2003; Safranyik et al. 2010)."),

        h4("References"),
        p("Bentz, B J, J R\u00e9gni\u00e8re, C J Fettig, E M Hansen, J L Hayes, J A Hicke, R. G. Kelsey, J. F. Negr\u00f3n, and S. J. Seybold. 2010. \"Climate Change and Bark Beetles of the Western United States and Canada: Direct and Indirect Effects.\" BioScience 60(8):602-13. doi:10.1525/bio.2010.60.8.6."),
        p("Logan, Jesse A, Jacques R\u00e9gni\u00e8re, and James A Powell. 2003. \"Assessing the impacts of global warming on forest pest dynamics.\" Frontiers in Ecology and the Environment 1(3):130-37. doi:10.1890/1540-9295(2003)001[0130:ATIOGW]2.0.CO;2."),
        p("Nealis, Vince G, and Barry J Cooke. 2014. \"Risk assessment of the threat of mountain pine beetle to Canada's boreal and eastern pine forests.\" Ottawa, ON: Canadian Council of Forest Ministers. http://cfs.nrcan.gc.ca/publications?id=35406."),
        p("Nealis, Vince G, and Brian Peter. 2008. \"Risk assessment of the threat of mountain pine beetle to Canada's boreal and eastern pine forests.\" Infromation Report. Victoria, BC: Natural Resources Canada, Canadian Forest Service, Pacific Forestry Centre."),
        p("R\u00e9gni\u00e8re, Jacques, and Barbara Bentz. 2007. \"Modeling cold tolerance in the mountain pine beetle, Dendroctonus ponderosae.\" Journal of Insect Physiology 53(6): 559-72. doi:10.1016/j.jinsphys.2007.02.007."),
        p("Safranyik, Les, Allan L Carroll, Jacques R\u00e9gni\u00e8re, David W Langor, William G Riel, Terry L Shore, Brian Peter, Barry J Cooke, Vince G Nealis, and Stephen W Taylor. 2010. \"Potential for range expansion of mountain pine beetle into the boreal forest of North America.\" The Canadian Entomologist 142(5):415-42. doi:10.4039/n08-CPA01."),
        p("Safranyik, L.; Shrimpton, D.M.; Whitney, H.S. 1975. \"An interpretation of the interaction between lodgepole pine, the mountain pine beetle, and its associated blue stain fungi in western Canada.\" In Management of Lodgepole Pine Ecosystems Symposium Proceedings, edited by D M Baumgartner, 406-28. Pullman, WA: Washington State University Coop. Extension Service. http://wfiwc.org/sites/default/files/Safranyik_Shrimpton_Whitney_1975.pdf.")
    ),
    box(title = "Mountain Pine Beetle Attack", width = 12, status = "success",
        collapsible = TRUE, collapsed = TRUE,
        h4("Summary"),
        p("MPB aerial survey data for BC and AB are provided by those provinces as point and polygon data."),
        p("Cooke & Carroll (in press) describes the methodology by which MPB attack data for AB (derived from spatial points) and BC (derived from polygons) have been rasterized and merged into a single `RasterLayer`:"),
        tags$blockquote("To provide an unbiased perspective on MPB damage in the two provinces, the two data sets were rasterized to a common resolution, allowing impact to be expressed in common units (hectares attacked per 4 km2 cell).",
                        "For BC data, raster cells were weighted according to polygon-wide average infestation rates and areas calculated from the proportion of a polygon infested within each raster cell, assuming an average stem density of 1125 mature stems per hectare, which is typical for beetle-prone lodgepole pine stands (Whitehead and Russo, 2005).",
                        "Data were weighted and calculated for each year in the sequence, and then summed. For Alberta data, the number of attacked trees per point attack cluster was determined from provincial government records, based on a ground survey, and summed across years within each 4 km2 raster cell."),
        p("NOTE: We produced finer-resolusion rasters (250m x 250m) to match the resolution of the kNN pine maps."),
        p("NOTE: currently, only the data up to and including 2011 are being used, as more recent data for BC have not yet been acquired (we have AB aerial survey data up to and including 2016)."),

        h4("References"),
        p("Cooke, Barry J, and Allan L Carroll. in press. \"The risk of mountain pine beetle spread to eastern pine forests: what can we predict?\""),
        p("Whitehead, R J, Russo, G L. 2005. \"\'Beetle-proofed\' lodgepole pine stands in interior British Columbia have less damage from mountain pine beetle\". Info. Rep. No. BC-X-402. Natural Resources Canada, Canadian Forest Service, Pacific Forestry Centre, Victoria, B.C.")
    ),
    box(title = "Pine Availability", width = 12, status = "success",
        collapsible = TRUE, collapsed = TRUE,
        h4("Summary"),
        p("Beaudoin et al. (2014) produced estimates of percent cover for several major tree species across Canada, including lodgepole pine and jack pine, at 250m resolution.",
          "These estimates are used to provide a base map of the proportion pine available to MPB in each pixel."),

        h4("References"),
        p("Beaudoin, A, P Y Bernier, L Guindon, P Villemaire, X J Guo, G Stinson, T Bergeron, S Magnussen, and R J Hall. 2014. \"Mapping attributes of Canada's forests at moderate resolution through kNN and MODIS imagery.\" Canadian Journal of Forest Research 44: 521-32. doi:10.1139/cjfr-2013-0401.")
    )
  )
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param sim      A \code{simList} object
#'
#' @export
#' @rdname dataInfo
dataInfo <- function(input, output, session, sim) {
  ##
}
