#' Generate a page displaying your app's Privacy Statement
#'
#' Renders a markdown file containing your app's Privacy Statement.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname privacyStatement
privacyStatementUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("appPrivacyUI"))
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param file     Path to a markdown file containing the Privacy Statement
#'                 (default: \code{"PRIVACY.md"}).
#' @param status   The status (color) of the box in which the \code{file} is rendered.
#'                 See \code{\link[shinydashboard]{box}}.
#'
#' @export
#' @importFrom shiny fluidRow includeMarkdown renderUI tagList
#' @importFrom shinydashboard box
#' @rdname privacyStatement
privacyStatement <- function(input, output, session, file = "PRIVACY.md", status = NULL) {

  if (!file.exists(file)) stop("Cannot find file '", file, "'.")

  output$appPrivacyUI <- renderUI({
    ns <- session$ns

    tagList(
      fluidRow(
        shinydashboard::box(
          title = "Privacy Statement", status = status,
          solidHeader = TRUE, collapsible = TRUE, width = 12,

          includeMarkdown(file)
        )
      )
    )
  })
}

#' Generate a page displaying your app's Terms of Service
#'
#' Renders a markdown file containing your app's Terms of Service.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname termsOfService
termsOfServiceUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("appToSUI"))
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param file     Path to a markdown file containing the Terms of Service
#'                 (default: \code{"TERMS.md"})
#' @param status   The status (color) of the box in which the \code{file} is rendered.
#'                 See \code{\link[shinydashboard]{box}}.
#'
#' @export
#' @importFrom shiny fluidRow includeMarkdown renderUI tagList
#' @importFrom shinydashboard box
#' @rdname termsOfService
termsOfService <- function(input, output, session, file = "TERMS.md", status = NULL) {
  output$appToSUI <- renderUI({
    ns <- session$ns

    tagList(
      fluidRow(
        shinydashboard::box(
          title = "Terms of Service", status = status,
          solidHeader = TRUE, collapsible = TRUE, width = 12,

          includeMarkdown(file)
        )
      )
    )
  })
}
