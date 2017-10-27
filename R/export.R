#' Export module
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param availableTypes list of types that user can select from and export to
#'
#' @importFrom shiny downloadButton NS selectInput tagList
#' @rdname export
#'
#' @export
exportUI <- function(id,
                     availableTypes = c("csv", "txt", "xls", "png", "pdf", "tiff", "grd", "rds")) {
  ns <- NS(id)

  tagList(
    selectInput(ns("selectedFileType"), "File type", availableTypes, multiple = FALSE),
    downloadButton(ns("downloadButton"), "Download")
  )
}

#' Export to file
#'
#' @param file    shiny server input object
#' @param data    shiny server output object
#' @param device  shiny server session object
#'
#' @importFrom R.devices devEval
#' @importFrom ggplot2 ggsave
#' @importFrom rasterVis levelplot
#' @importFrom quickPlot Plot
#' @importFrom xlsx write.xlsx
#' @importFrom graphics plot.new mtext
exportToFile <- function(file, data, device) {
  if ("ggplot" %in% class(data)) {
    ggplot2::ggsave(file, data, device)
  } else {
    R.devices::devEval(device, filename = file, {
      if ("RasterLayer" %in% class(data)) {
        print(rasterVis::levelplot(data))
      } else if ("Spatial" %in% substr(class(data), 0, 7)) {
        print(Plot(data))
      } else {
        plot.new()
        mtext(paste0("No export handler defined for data of class: ", class(data), "!"))
      }
    })
  }
}

#' Export module server
#'
#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param data     data to be downloaded
#' @param filename name of the downloaded file
#'
#' @importFrom shiny observeEvent
#' @importFrom utils write.table write.csv
#' @importFrom xlsx write.xlsx
#' @importFrom raster writeRaster
#' @rdname export
#'
#' @export
export <- function(input, output, session, data, filename = "download") {
  output$downloadButton <- downloadHandler(
    filename = function() {
      paste0(filename, ".", input$selectedFileType)
    },
    content = function(file) {
      if (is.reactive(data)) {
        data <- data()
      }
      switch(input$selectedFileType,
             csv = utils::write.csv(data, file),
             txt = utils::write.table(data, file),
             xls = xlsx::write.xlsx(data, file),
             png = exportToFile(file, data, device = "png"),
             tiff = exportToFile(file, data, device = "tiff"),
             pdf = exportToFile(file, data, device = "pdf"),
             rds = saveRDS(data, file),
             grd = raster::writeRaster(data, file, format = "raster"))
    }
  )
}
