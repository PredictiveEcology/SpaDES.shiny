#' Generate a page displaying your app's \file{NEWS} file
#'
#' Renders a markdown file containing your app's \file{NEWS} file, which can be used as
#' an updates feed for users to see what has changed in the app.
#'
#' @template id
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname appNews
appNewsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("appNewsUI"))
}

#' @template input
#' @template output
#' @template session
#' @param file  Path to a markdown file containing the NEWS (default: `"NEWS.md"`).
#' @template status
#'
#' @export
#' @importFrom future future
#' @importFrom shiny fluidRow includeMarkdown renderUI tagList
#' @importFrom shinydashboard box
#' @rdname appNews
appNews <- function(input, output, session, file = "NEWS.md", status = NULL) {

  if (!file.exists(file)) stop("Cannot find file '", file, "'.")

  output$appNewsUI <- renderUI({
    ns <- session$ns

    future({
      tagList(
        fluidRow(
          shinydashboard::box(
            title = "News & Updates", status = status,
            solidHeader = TRUE, collapsible = TRUE, width = 12,

            includeMarkdown(file)
          )
        )
      )
    })
  })
}
