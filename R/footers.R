#' Generate HTML footers for body or sidebar
#'
#' App copyright info can be inserted into a footer in the dashboard body using
#' \code{copyrightFooter} module.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS renderText
#' @rdname footers
#'
#' @examples
#' \dontrun{
#' # define this in global.R (or global_file.R)
#' cph <- paste("Author Name")
#'
#' # The following line is in the global.R.template:
#' callModule(copyrightFooter, "copyright", cph)
#' }
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
#' @importFrom shiny HTML icon
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
#' @importFrom shiny HTML
#' @rdname footers
sidebarFooter <- function() {
  HTML(paste(
    "<footer>",
    "<div id=\"sidebar\" style='position: absolute; bottom: 5px; margin: 15px'>",
    " Built with ",
    "<a href=\"http://SpaDES.PredictiveEcology.org\">  <img src=\"http://predictiveecology.org/img/avatar.png\", height=25px>SpaDES, </a>", # nolint
    "<a href=\"http://shiny.rstudio.com/\", target=\"_blank\">shiny</a> ",
    "and <a href=\"https://appsilondatascience.com/\">  <img src=\"http://d3u4jj2f3q2139.cloudfront.net/logo-appsilon-data-science-transparent.png\", height=20px> </a>", # nolint
    "</div>",
    "</footer>"
  ))
}
