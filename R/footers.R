#' Generate HTML footers for body or sidebar
#'
#' App copyright info can be inserted into a footer in the dashboard body using
#' \code{copyrightFooter}.
#'
#' @param copyrightInfo  Character string containing your app's copyright info.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny HTML
#' @rdname footers
copyrightFooter <- function(copyrightInfo) {
  HTML(paste(
    "<footer>", "<div id=\"copyright\">", copyrightInfo, "</div>", "</footer>"
  ))
}

#' @author Alex Chubaty
#' @export
#' @importFrom shiny HTML
#' @rdname footers
sidebarFooter <- function() {
  HTML(paste(
    "<footer>",
    "<div id=\"sidebar\">",
    "Powered by <a href=\"http://SpaDES.PredictiveEcology.org\", target=\"_blank\">\u2660 SpaDES</a> ", # nolint
    "and <a href=\"http://shiny.rstudio.com/\", target=\"_blank\">shiny</a>",
    "</div>",
    "</footer>"
  ))
}
