#' Slider Module
#'
#' @description Function \code{sliderUI} creates a shiny module UI.
#'
#' @param id   An ID string that corresponds with the ID used to call the module's UI function.
#' @param ...  Additonal arguments passed to \code{sliderInput}.
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
#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
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
