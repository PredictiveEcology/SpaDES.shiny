sliderUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("slider"), label = "Slider", min = 1, max = 10, value = 5)
  )
}

slider <- function(input, output, session) {
  observeEvent(input$slider, {
    cat("Slider: current slider value is ", input$slider, "\n")
  })
  slider_value <- reactive({input$slider})
  return(slider_value)
}