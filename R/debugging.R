#' Get simulation-specific log filepath
#'
#' @template config
#' @template session
#' @param simID    The simulation ID
#'
#' @author Alex Chubaty
#' @export
#' @rdname simLogFile
simLogFile <- function(config, session, simID) {
  file.path(config$OUTPUT_DIR, simID, "error.log")
}

#' Generate Debugging Tab
#'
#' View app-specific logs at a specific path (default is \file{/var/log/shiny-server}).
#'
#' @details Generates a tab consisting of the text output from the log file.
#'
#' @template id
#'
#' @author Olivia Sung, Alex Chubaty, Alex Tso
#' @export
#' @importFrom shiny NS verbatimTextOutput
#' @rdname debugging
debuggingUI <- function(id) {
  ns <- NS(id)

  verbatimTextOutput(ns("simLogText"))
}

#' @template input
#' @template output
#' @template session
#' @template config
#' @param simID   The simulation number
#'
#' @export
#' @importFrom shiny reactiveFileReader
#' @rdname debugging
debugging <- function(input, output, session, config, simID) {
  output$simLogText <- renderText({
    file <- simLogFile(config, session, simID)
    if (file.exists(file)) {
      simLogFileData <- reactiveFileReader(1000, session, file, readLines)
      paste(simLogFileData(), collapse = "\n")
    } else {
      paste("You must start a simulation.")
    }
  })
}
