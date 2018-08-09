#' Generate a page displaying your app's NEWS file
#'
#' Renders a markdown file containing your app's NEWS file, which can be used as
#' an updates feed for users to see what has changed in the app.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname appNews
appNewsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("appNewsUI"))
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param file     Path to a markdown file containing the NEWS
#'                 (default: \code{"NEWS.md"}).
#' @param status   The status (color) of the box in which the \code{file} is rendered.
#'                 See \code{\link[shinydashboard]{box}}.
#'
#' @export
#' @importFrom shiny fluidRow includeMarkdown renderUI tagList
#' @importFrom shinydashboard box
#' @rdname appNews
appNews <- function(input, output, session, file = "NEWS.md", status = NULL) {

  if (!file.exists(file)) stop("Cannot find file '", file, "'.")

  output$appNewsUI <- renderUI({
    ns <- session$ns

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
}
