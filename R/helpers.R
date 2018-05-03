#' Unsuspend hidden modules
#'
#' Modified from \url{https://groups.google.com/forum/#!topic/shiny-discuss/F-EwQGELS1c}.
#'
#' @param
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
