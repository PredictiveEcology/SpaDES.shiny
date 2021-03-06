#' Slider Module
#'
#' @description Function \code{sliderUI} creates a shiny module UI.
#'
#' @template id
#'
#' @return None. Invoked for the side-effect of creating a shiny UI.
#'
#' @export
#' @importFrom shiny NS sliderInput
#' @rdname slider
#' @seealso \code{\link{sliderInput}}
sliderUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("sliderOut"))
}

#' Slider Module
#'
#' @description Function \code{slider} creates a slider module server function.
#'
#' @template input
#' @template output
#' @template session
#' @param ...      Additional arguments passed to \code{\link{sliderInput}}.
#'
#' @return Reactive value with the current value on the slider.
#'
#' @importFrom shiny reactive
#'
#' @author Mateusz Wyszynski
#'
#' @rdname slider
#'
#' @export
slider <- function(input, output, session, ...) {
  output$sliderOut <- renderUI({
    ns <- session$ns

    sliderInput(ns("slider"), ...)
  })

  return(reactive(input$slider))
}
