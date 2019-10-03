#' Generate a page displaying your app's Privacy Statement
#'
#' Renders a markdown file containing your app's Privacy Statement.
#'
#' @template id
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname privacyStatement
privacyStatementUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("appPrivacyUI"))
}

#' @template input
#' @template output
#' @template session
#' @param file     Path to a markdown file containing the Privacy Statement
#'                 (default: \code{"PRIVACY.md"}).
#' @template status
#'
#' @export
#' @importFrom future future
#' @importFrom shiny fluidRow includeMarkdown renderUI tagList
#' @importFrom shinydashboard box
#' @rdname privacyStatement
privacyStatement <- function(input, output, session, file = "PRIVACY.md", status = NULL) {

  if (!file.exists(file)) stop("Cannot find file '", file, "'.")

  output$appPrivacyUI <- renderUI({
    ns <- session$ns

    future({
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
  })
}

#' Generate a page displaying your app's Terms of Service
#'
#' Renders a markdown file containing your app's Terms of Service.
#'
#' @template id
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname termsOfService
termsOfServiceUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("appToSUI"))
}

#' @template input
#' @template output
#' @template session
#' @param file     Path to a markdown file containing the Terms of Service
#'                 (default: \code{"TERMS.md"})
#' @template status
#'
#' @export
#' @importFrom future future
#' @importFrom shiny fluidRow includeMarkdown renderUI tagList
#' @importFrom shinydashboard box
#' @rdname termsOfService
termsOfService <- function(input, output, session, file = "TERMS.md", status = NULL) {
  output$appToSUI <- renderUI({
    ns <- session$ns

    future({
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
  })
}
