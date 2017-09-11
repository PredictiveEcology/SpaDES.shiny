#' Slider Module
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param label Character string with which the slider will be labelled. Default is NULL - no label.
#' @param min The minimum value that can be selected
#' @param max The maximum value that can be selected
#' @param value The initial value of the slider
#' 
#' @author Mateusz Wyszynski
#' @export
sliderUI <- function(id, label = NULL, min, max, value) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("slider"), label = label, min, max, value)
  )
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#'
#' @export
slider <- function(input, output, session) {
  observeEvent(input$slider, {
    cat("Slider: current slider value is ", input$slider, "\n")
  })
  slider_value <- reactive({input$slider})
  return(slider_value)
}