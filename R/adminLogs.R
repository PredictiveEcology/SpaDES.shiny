#' Admin log file viewer shiny module
#'
#' View app-specific logs at a specific path (default is \file{/var/log/shiny-server}).
#'
#' Generates a `fluidRow` consisting of two columns: one containing a file selector
#' dropdown menu and the other displaying the text output from the selected log file.
#'
#' @template id
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
    column(width = 8, div(style = 'overflow-y: scroll'), # nolint
      verbatimTextOutput(ns("adminLogText"))
    )
  )
}

#' @template input
#' @template output
#' @template session
#' @template config
#' @param path     file path of the server log files (usually \file{/var/log/shiny-server})
#'
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
    paste(adminLogFileData(), collapse = "\n")
  })
}
