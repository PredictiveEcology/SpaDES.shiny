sliderUI <- function(id, label = NULL, min, max, value) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("slider"), label = label, min, max, value)
  )
}

slider <- function(input, output, session) {
  observeEvent(input$slider, {
    cat("Slider: current slider value is ", input$slider, "\n")
  })
  slider_value <- reactive({input$slider})
  return(slider_value)
}