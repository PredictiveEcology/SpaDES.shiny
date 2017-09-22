#' Slider Module
#'
#' @description Function \code{sliderUI} creates a shiny module UI.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param ... sliderInput arguments. See ?sliderInput for reference.
#'
#' @return None. Invoked for the side-effect of creating a shiny UI.
#'
#' @importFrom shiny NS sliderInput
#'
#' @rdname slider
#'
#' @export
sliderUI <- function(id, ...) {
  ns <- NS(id)

  sliderInput(ns("slider"), ...)
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
slider <- function(input, output, session) {
  return(reactive(input$slider))
}
