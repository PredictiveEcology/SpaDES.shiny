#' Generate HTML footers for body or sidebar
#'
#' App copyright info can be inserted into a footer in the dashboard body using
#' `copyrightFooter` module.
#'
#' @template id
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname footers
copyrightFooterUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("copyrightInfo"))
}

#' @template input
#' @template output
#' @template session
#' @param cph      Character string indicating the copyright holder name.
#'                 This is automatically generated from the `copyright` item
#'                 in the app metadata sued my `newApp`.
#' @param year     Character string indicating the copyright date.
#'                 Defaults to the current year.
#'
#' @export
#' @importFrom shiny HTML icon renderUI
#' @rdname footers
copyrightFooter <- function(input, output, session, cph = "Author Name",
                            year = format(Sys.time(), "%Y")) {
  output$copyrightInfo <- renderUI({
    HTML(paste(
      "<footer>", "<div id=\"copyright\">",
      shiny::icon("copyright",  lib = "font-awesome"),
      "Copyright ", year, cph,
      "</div>", "</footer>"
    ))
  })
}

#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname footers
sidebarFooterUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("sidebarInfo"))
}

#' @param footer Custom text to appear in footer above the 'SpaDES' and 'Appsilon' logos.
#' @export
#' @importFrom shiny br HTML
#' @rdname footers
sidebarFooter <- function(input, output, session, footer = NULL) {
  customFooter <- if (length(footer) == 0) "" else footer

  output$sidebarInfo <- renderUI({
    HTML(paste(
      "<footer>",
      "<div id=\"sidebar\">",
      paste(customFooter, br()),
      " Built with ",
      paste("<a href=\"http://SpaDES.PredictiveEcology.org\">",
            "<img src=\"http://predictiveecology.org/img/avatar.png\", height=25px>",
            "SpaDES,",
            "</a> "),
      "<a href=\"http://shiny.rstudio.com/\", target=\"_blank\">shiny</a> ", "and ",
      paste("<a href=\"https://appsilondatascience.com/\">",
            "<img src=\"http://d3u4jj2f3q2139.cloudfront.net/logo-appsilon-data-science-transparent.png\", height=20px>", # nolint
            "</a>"),
      "</div>",
      "</footer>"
    ))
  })
}
