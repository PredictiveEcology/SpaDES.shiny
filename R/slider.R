#' Slider Module
#'
#' @description This creates a slider module UI.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param ... sliderInput arguments. See ?sliderInput for reference.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom shiny NS sliderInput
#' @rdname slider
sliderUI <- function(id, ...) {
  ns <- NS(id)

  sliderInput(ns("slider"), ...)
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#'
#' @export
#' @importFrom shiny reactive
#' @rdname slider
slider <- function(input, output, session) {
  return(reactive(input$slider))
}
