#' Slider Module
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param label Character string with which the slider will be labelled. Default is NULL - no label.
#' @param min The minimum value that can be selected
#' @param max The maximum value that can be selected
#' @param value The initial value of the slider
#' @param interval Number specifying how fast animation should progress
#' @param loop Should the animation be a loop. Default is FALSE - no loop.
#' 
#' @author Mateusz Wyszynski
#' @export
sliderUI <- function(id, label = NULL, min, max, value, interval, loop = FALSE) {
  ns <- NS(id)
  
  if(missing(min)) {
    stop("Slider Module requires a minimum value.")
  }
  if(missing(max)) {
    stop("Slider Module requires a maximum value.")
  }
  if(missing(value)) {
    stop("Slider Module requires an initial value.")
  }
  if(missing(interval)){
    stop("Slider Module requires an interval argument.")
  }
  
  tagList(
    sliderInput(ns("slider"), label = label, min, max, value, animate = animationOptions(interval = interval, loop = loop))
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