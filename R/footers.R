#' Generate HTML footers for body or sidebar
#'
#' App copyright info can be inserted into a footer in the dashboard body using
#' \code{copyrightFooter} module.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname footers
copyrightFooterUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("copyrightInfo"))
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param cph      Character string indicating the copyright holder name.
#'                 This is automatically generated from the \code{copyright} item
#'                 in the app metadata sued my \code{newApp}.
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

#' @export
#' @importFrom shiny br HTML
#' @rdname footers
sidebarFooter <- function(input, output, session, footer = NULL) {
  customFooter <- if (length(footer) == 0) "" else footer

  output$sidebarInfo <- renderUI({
    HTML(paste(
      "<footer>",
      "<div id=\"sidebar\" style='position: absolute; bottom: 5px; margin: 15px'>",
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
