#' Rasters-over-time shiny module
#'
#' A shiny module showing the values from a raster in time on a predefined map.
#' Allows changing polygons and rasters via slider.
#' Additionally, a histogram summary for each raster choice is shown.
#'
#' @param id An ID string that corresponds with the ID used to call the module server function.
#'
#' @return None. Invoked for the side-effect of creating a shiny UI.
#'
#' @author Damian Rodziewicz
#' @author Alex Chubaty
#' @export
#' @importFrom leaflet leafletOutput
#' @importFrom shiny fluidRow br htmlOutput NS uiOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinydashboard box
#' @rdname rasterOverTime
rastersOverTimeUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(width = 8, solidHeader = TRUE, collapsible = TRUE,
        htmlOutput(ns("title")),
        shinycssloaders::withSpinner(leaflet::leafletOutput(ns("map"), height = 600)),
        sliderUI(ns("rastersSlider")),
        polygonChooserUI(ns("polyDropdown"))
    ),
    uiOutput(ns("histUI"))
  )
}

#' @param input           Shiny server input object.
#' @param output          Shiny server output object.
#' @param session         Shiny server session object.
#' @param rctRasterList   A reactive that gives a list of rasters to be displayed.
#' @param rctUrlTemplate  The reactive url template for leaflet map tiles
#' @param rctPolygonList  Reactive list with sets of polygons to be displayed on a leaflet map.
#'                        # TODO: decribe the format of the list!
#' @param defaultPolyName Name of the polygon to use as the default for mapping.
#' @param map             Leaflet map to show raster and polygons on.
#' @param colorPalette    Colour palette to use.
#' @param histTitle       Title to be shown above the histogram.
#' @param mapTitle        Title to be shown above the map.
#' @param sliderTitle     Title to be shown above the slider.
#' @param nPolygons       The number of available polygons.
#' @param nRasters        The number of available rasters.
#' @param rasterStepSize  Size of step in the raster slider.
#'
#' @return Reactive polygon selected by the user with the \code{polygonChooser} module.
#'          Invoked for the side-effect of creating shiny server and ui components. # TODO: reword
#'
#' @export
#' @importFrom leaflet JS layersControlOptions leaflet leafletOptions leafletOutput leafletProxy
#' @importFrom leaflet providerTileOptions renderLeaflet setView tileOptions
#' @importFrom raster cellFromXY crop crs extent extract filename hist maxValue ncell
#' @importFrom raster sampleRegular res rowColFromCell xmax xmin ymax ymin
#' @importFrom reproducible asPath Cache
#' @importFrom shiny animationOptions callModule h4 reactive
#' @importFrom sp bbox CRS SpatialPoints spTransform
#' @importFrom SpaDES.core cachePath outputPath paddedFloatToChar
#' @rdname rasterOverTime
rastersOverTime <- function(input, output, session, rctRasterList, rctUrlTemplate,
                            rctPolygonList,
                            defaultPolyName = NULL,
                            map = leaflet(),
                            colorPalette,
                            histTitle = "",
                            sliderTitle = "",
                            mapTitle = "",
                            nPolygons, nRasters, rasterStepSize = 10) {

  output$map <- renderLeaflet(map)
  mapProxy <- leafletProxy("map")

  rctChosenPolyName <- callModule(polygonChooser, "polyDropdown", rctPolygonList, defaultPolyName) ## reactive character

  rctPoly4Map <- reactive({
    polyList <- rctPolygonList()
    polyList[[rctChosenPolyName()]][["crsLFLT"]][["shpSubStudyRegion"]]
  })

  rctRasterIndexValue <- callModule(slider, "rastersSlider", label = sliderTitle,
                                    min = 0, max = (nRasters - 1) * rasterStepSize,
                                    value = 0, step = rasterStepSize,
                                    animate = animationOptions(interval = 2500, loop = FALSE))

  rasts <- reactive({
    rasterIndex <- rasterIndex <- if (is.null(rctRasterIndexValue())) {
      1
    } else {
      rctRasterIndexValue() / rasterStepSize + 1
    }

    rst <- lapply(rctRasterList(), function(x) x[[rasterIndex]]) # get both crs

    return(rst);
  })

  sampledRasterVals <- reactive({
    mb <- input$map_bounds

    ras <- if (is.null(mb) ) {
      rasts()$crsSR
    } else {
      mapBoundsAsExtent <- raster::extent(x = mb$west, xmax = mb$east,
                                          ymin = mb$south, ymax = mb$north)
      sp1 <- SpatialPoints(t(bbox(mapBoundsAsExtent)), proj4string = CRS(proj4stringLFLT))
      sp2 <- spTransform(sp1, crs(rasts()$crsSR))
      tryCatch(crop(rasts()$crsSR, sp2), error = function(x) NULL)
      #tryCatch(Cache(crop, rasts()$crsSR, sp2), error = function(x) NULL)
    }

    ret <- if (!is.null(ras)) {
      # Cache(.sampleRasterToRAM, ras)
      .sampleRasterToRAM(ras)
    } else {
      NULL
    }
    ret
  })

  xAxisBreaks <- reactive({
    c(0, seq.int(ceiling(maxValue(rasts()$crsSR) / 10))  * 10)
  })

  addAxisParams <- reactive({
    return(list(side = 1, at = xAxisBreaks() / 10, labels = xAxisBreaks()))
  })

  rasterScale <- reactive({
    prod(raster::res(rasts()$crsSR)) / 1e4 / 1e3 # 1000s of hectares
  })

  addTilesParameters <- list(
    option = tileOptions(tms = TRUE, minZoom = 1, maxZoom = 10, opacity = 1)
  )

  click <- reactive(input$map_shape_click)

  rctUrlTemplateSingleFile <- reactive({
    rasterFilename <- strsplit(basename(filename(rasts()$crsLFLT)), "\\.")[[1]][[1]]
    grep(rasterFilename, gsub("www/", "", rctUrlTemplate()), value = TRUE)
  })

  ## TODO: fix the raster control layer tile swiicher in top right corner of map
  callModule(tilesUpdater, "tilesUpdater", mapProxy, rctUrlTemplateSingleFile, session$ns("tiles"), ## don't change ns
             addTilesParameters = addTilesParameters, addLayersControlParameters = NULL)

  callModule(summaryPopups, "popups", mapProxy, click, reactive(rasts()$crsLFLT), rctPoly4Map)

  callModule(polygonsUpdater, "polygonsUpdater", mapProxy, rctPoly4Map,
             fillOpacity = 0.0, weight = 0.5)

  callModule(histogramForRaster, "histogram", sampledRasterVals,
             rctHistogramBreaks = xAxisBreaks,
             scale = rasterScale(), addAxisParams = addAxisParams,
             col = colorPalette, width = 1, space = 0,
             main = "Approximate area in each age class",
             xlab = "Time since fire, years",
             ylab = "Area in visible window (1000s hectares)",
             cex.names = 2, cex.lab = 1.3, cex.main = 1.5, cex.axis = 1.5)

  output$title <- renderUI(h4(mapTitle))

  output$histUI <- renderUI({
    ns <- session$ns

    histogramForRasterUI(ns("histogram"), title = h4(histTitle),
                         plotParameters = list(height = 600), solidHeader = TRUE,
                         collapsible = TRUE, width = 4)
  })

  return(rctChosenPolyName) ## the reactive polygon selected by the user
}

.sampleRasterToRAM <- function(ras) {
  if (ncell(ras) > 1e7) {
    sampledRasterVals <- raster::sampleRegular(ras, size = 5e5, asRaster = FALSE)
  } else {
    sampledRasterVals <- ras[]
  }
  sampledRasterVals[sampledRasterVals == 0] <- NA
  sampledRasterVals
}
