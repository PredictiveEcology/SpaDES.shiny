#' Unsuspend hidden modules
#'
#' Modified from \url{https://groups.google.com/forum/#!topic/shiny-discuss/F-EwQGELS1c}.
#'
#' @note "by default, \code{renderPlot} will try to set the plot output size to
#' the same width/height of the \code{plotOutput}, which will be 0 by 0 pixels
#' when it's not actually visible. You can work around this if necessary by
#' providing explicit \code{width} and \code{height} arguments to the
#' \code{renderPlot()} function call itself (NOT the call to \code{plotOutput}
#' and not any function calls inside the \code{renderPlot} code block).
#' For example, \code{renderPlot({...}, width=400, height=300)}"
#'
#' @param id       Module id.
#' @param session  The current shiny server session.
#'
#' @author Andrew Sali and Joe Cheng
#' @export
#' @importFrom shiny getDefaultReactiveDomain NS observe outputOptions
unsuspendModule <- function(id, session = getDefaultReactiveDomain()) {
  observe({
    output_names <- names(session$clientData) %>%
      {.[grepl(NS(session$ns(id))(""), ., fixed = TRUE)]} %>%
      {gsub("^output[_]", "", ., fixed = FALSE)} %>%
      {gsub("[_]hidden$", "", ., fixed = FALSE)}

    for (output_id in output_names) {
      #print(output_id)
      try({
        outputOptions(session$output, output_id, suspendWhenHidden = FALSE)
      }, silent = TRUE)
    }
  })
}
