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
#' @examples
#' copyrightInfo <- paste(
#'   shiny::icon("copyright",  lib = "font-awesome"), "Copyright ",
#'   format(Sys.time(), "%Y"),
#'   paste("Her Majesty the Queen in Right of Canada,",
#'         "as represented by the Minister of Natural Resources Canada.")
#' )
#' 
#' # The following line is in the ui.R.template and will be run if 
#' # global.R has a copyrightInfo object
#' copyrightFooter(copyrightInfo)
copyrightFooter <- function(copyrightInfo) {
	if (!missing(copyrightInfo))
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
    "<div id=\"sidebar\" style='position: absolute; bottom: 5px; margin: 15px'>",
    " Built with ",
    "<a href=\"http://SpaDES.PredictiveEcology.org\">  <img src=\"http://predictiveecology.org/img/avatar.png\", height=25px>SpaDES, </a>", # nolint
    "<a href=\"http://shiny.rstudio.com/\", target=\"_blank\">shiny</a> ",
    "and <a href=\"https://appsilondatascience.com/\">  <img src=\"http://d3u4jj2f3q2139.cloudfront.net/logo-appsilon-data-science-transparent.png\", height=20px> </a>", # nolint
    "</div>",
    "</footer>"
  ))
}
