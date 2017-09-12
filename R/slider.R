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
sliderUI <- function(id, ...) {
  ns <- NS(id)
  
  sliderInput(ns("slider"), ...)
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#'
#' @export
slider <- function(input, output, session) {
  return(reactive(input$slider))
}
