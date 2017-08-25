#' Generate Admin Log File Viewer
#'
#' View app-specific logs at a specific path (default is \file{/var/log/shiny-server}).
#'
#' @details Generates a fluidRow consisting of two columns:
#' one containing a file selector dropdown menu and the other displaying the
#' text output from the selected log file.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny column fluidRow h4 NS uiOutput verbatimTextOutput wellPanel
#' @rdname adminLogs
adminLogsUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(width = 4,
      wellPanel(
        h4("Admin Log Files"),
        uiOutput(ns("selectAdminLog"))
      )
    ),
    column(width = 8, div(style = 'overflow-y: scroll'),
      verbatimTextOutput(ns("adminLogText"))
    )
  )
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param config   inSpaDES app configuration (e.g., from \code{\link{readConfig}})
#' @param path     file path of the server log files (usually \file{/var/log/shiny-server})
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny isolate reactiveFileReader renderText renderUI req selectInput
#' @rdname adminLogs
adminLogs <- function(input, output, session, config, path = "/var/log/shiny-server") {

  adminLogSelector <- function(config, path) {
    renderUI({
      ns <- session$ns
      selectInput(ns("adminLogFileSelector"), label = "Select Log File", {
        list.files(path, pattern = "[.]log$") %>%
          grep(basename(config$APP_DIR), ., value = TRUE) %>%
          c("", .) # prepend empty string so by default no file selected
      })
    })
  }

  output$selectAdminLog <- adminLogSelector(config, path)

  adminLogFile <- function(path) {
    req(input$adminLogFileSelector)
    file.path(path, isolate(input$adminLogFileSelector))
  }

  output$adminLogText <- renderText({
    f <- adminLogFile(path)
    adminLogFileData <- reactiveFileReader(1000, session, f, readLines)
    paste(adminLogFileData(), collapse = '\n')
  })
}
