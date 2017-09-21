#' Time Since Fire Shiny Module UI
#'
#' @description This creates a shiny module UI for Time Since Fire shiny module. Compare with \code{?timeSinceFire}
#'
#' @param id An ID string that corresponds with the ID used to call the module server function
#' @param rastersNumber How many rasters can we choose from
#'
#' @author Mateusz Wyszynski
#'
#' @export
timeSinceFireUI <- function(id, rastersNumber) {
  ns <- NS(id)
  tagList(
    box(width = 8, solidHeader = TRUE, collapsible = TRUE,
        h4(paste("Below are a sequence of snapshots of the landscape, showing the natural range of",
                 "variation in time since fire. Click on the 'play' button at the bottom right to animate")),
        withSpinner(leaflet::leafletOutput(ns("timeSinceFire2"), height = 600)),
        sliderUI(ns("slider"),
                 "Individual snapshots of time since fire maps. Use play button (bottom right) to animate.",
                 min = 0, max = (rastersNumber-1)*10, value = 0, step = 10,
                 animate = animationOptions(interval = 2500, loop = FALSE))
    ),
    histogramForRasterUI(ns("histogram"), title = h4(paste("Current time since distribution distribution")), plotParameters = list(height = 600),
                         width = 4, solidHeader = TRUE, collapsible = TRUE))
}

#' Time Since Fire Shiny Module
#'
#' @description This creates a shiny module which displays rasters changing in time on a predefined map. A slider is also created on which you can choose which raster should be currently displayed. Moreover, a histogram summary for each raster choice is shown.
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny server session object
#' @param rasters Set of rasters to be displayed
#'
#' @author Mateusz Wyszynski
#'
#' @export
timeSinceFire <- function(input, output, session, rasters) {

  output$timeSinceFire2 <- renderLeaflet({
    leafZoom <- leafletZoomInit
    rasInp <- isolate(rasterInput())
    polyNum <- polygonInput()
    polyFull <- polygons[[polyNum + (length(polygons)/4)*2]] # leaflet projection, Full scale

    pol <- polygons[[(length(polygons)/4)*4]]
    shpStudyRegionFullLFLT <- spTransform(shpStudyRegionFull, crs(polyFull))
    leafMap <- leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10)) %>%
      addProviderTiles("Thunderforest.OpenCycleMap", group="Open Cycle Map",
                       options=providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery",
                       options=providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
      addPolygons(data = shpStudyRegionFullLFLT, color = "blue",
                  group = "Fire return interval",
                  fillOpacity = 0.3, weight = 1,
                  fillColor = ~colorFactor("Spectral", fireReturnInterval)(fireReturnInterval)) %>%
      addLegend(position = "bottomright", pal = timeSinceFirePalette,
                values = 1:maxAge,
                title = paste0("Time since fire",br(),"(years)")) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit = "hectares",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      addEasyButton(easyButton(
        icon="fa-map", title="Zoom to Demonstration Area",
        onClick=JS(paste0("function(btn, map){ map.fitBounds([[",ymin(pol),", ",xmin(pol),"], ["
                          ,ymax(pol),", ",xmax(pol) ,"]])}")))) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom out to LandWeb study area",
        onClick=JS(paste0("function(btn, map){ map.setView([",mean(c(ymin(shpStudyRegionFullLFLT),
                                                                     ymax(shpStudyRegionFullLFLT))),
                          ", ",mean(c(xmin(shpStudyRegionFullLFLT),
                                      xmax(shpStudyRegionFullLFLT))) ,"], 5)}")))) %>%
      addMiniMap(
        tiles = providers$OpenStreetMap,
        toggleDisplay = TRUE) %>%
      setView(mean(c(xmin(shpStudyRegionFullLFLT),xmax(shpStudyRegionFullLFLT))),
              mean(c(ymin(shpStudyRegionFullLFLT),ymax(shpStudyRegionFullLFLT))),
              zoom = leafZoom
      )
    leafMap
  })

  proxy <- leafletProxy("timeSinceFire2")

  urlTemplate <- reactive(file.path(studyArea,
                                    paste0("outrstTimeSinceFire_year",
                                           paddedFloatToChar(rasterInput()$sliderVal+summaryPeriod[1],
                                                             nchar(end(mySim))),
                                           "LFLT/{z}/{x}/{y}.png")))
  addTilesParameters <- list(option = tileOptions(tms = TRUE, minZoom = 1, maxZoom = 10, opacity = 0.8), urlTemplate = "error")
  addLayersControlParameters <- list(options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE),
                                     baseGroups = c("Open Cycle Map", "ESRI World Imagery"),
                                     overlayGroups = c("Time since fire", "Fire return interval"))

  callModule(tilesUpdater, "rasterUpdater", proxy, urlTemplate, "Time since fire", addTilesParameters, addLayersControlParameters)

  raster <- reactive(rasterInput()$r)

  breaks <- reactive(ceiling(maxValue(raster())/10))

  addAxisParams <- reactive({
    numberOfBreaks <- ceiling(maxValue(raster())/10)
    return(list(side = 1, at = 0:numberOfBreaks, labels = 0:numberOfBreaks*10))
  })

  callModule(histogramForRaster, "histogram", raster, scale = prod(rasterResolution)/1e4, histogramBreaks = breaks,
             addAxisParams = addAxisParams, xlab = "Time since fire \n(Years)", col = timeSinceFirePalette(1:(maxAge/10)),
             width = 1, space = 0, ylab = "Area (ha)")

  rasterInput <- reactive({
    sliderVal <- callModule(slider, "slider")
    sliderValue <-
      if(is.null(sliderVal())) {
        1
      } else {
        sliderVal()
      }
    currentRaster <-sliderValue/10 + 1
    r <- rasters[[currentRaster]] # slider units are 10, starting at 0; index here is 1 to length (tsf)

    if(useGdal2Tiles) {
      message("Running gdal2TilesFn for layer ", currentRaster, " of ", length(rasters))
      Cache(gdal2TilesFn, r, filename=asPath(filename(r)), #notOlderThan = Sys.time(),
            zoomRange=5:10, color_text_file = asPath(colorTableFile),
            cacheRepo = paths$cachePath, digestPathContent = TRUE)
    }
    if(TRUE) {
      #if(Sys.info()["nodename"]=="W-VIC-A105388") stopApp()
      if (ncell(r) > 3e5) {
        r <- Cache(sampleRegular, r, size = 4e5, #notOlderThan = Sys.time(),
                   asRaster = TRUE, cacheRepo = paths$cachePath)
        r[r[]>401] <- maxAge
        r[r[]==0] <- NA
      }

    }
    list(r=r, sliderVal=sliderValue)
  })

  polygonInput <- reactive({
    1
  })

  observe({
    #Observer to show Popups on click
    click <- input$timeSinceFire2_shape_click
    if (!is.null(click)) {
      showpos(x=click$lng, y=click$lat)
    }
  })

  showpos <- function(x=NULL, y=NULL) {
    #Show popup on clicks
    #Translate Lat-Lon to cell number using the unprojected raster
    #This is because the projected raster is not in degrees, we cannot use it!
    colNam <- names(polygons)[[(length(polygons)/4)*4]]
    pol <- polygons[[(length(polygons)/4)*3]]
    friPoly <- shpStudyRegion

    sp <- SpatialPoints(cbind(x,y), proj4string = crs(pol))
    ras1 <- rasterInput()$r
    cell <- cellFromXY(ras1, c(x, y))
    #if (!is.na(cell)) {#If the click is inside the raster...
    #Get row and column, to print later
    rc <- rowColFromCell(ras1, cell)

    #Get values from raster and polygon
    polyVal <- sp %>%
      extract(pol, .) %>%
      .[polygonIndivIdsColum[[colNam]]]
    friVal <- sp %>%
      spTransform(crs(shpStudyRegionFull)) %>%
      extract(shpStudyRegionFull, .) %>%
      .["fireReturnInterval"]

    val = ras1[][cell]

    firstPart <- if(!is.na(val)) {
      paste0("Time Since Fire=", round(val, 1), " years <br>")
    } else {
      ""
    }
    content <- paste0(firstPart,
                      polygonIndivIdsColum[[colNam]],": ",polyVal,"<br>",
                      "Fire Return Interval: ", friVal, "<br>",
                      "Lat/Long: ", round(y,4),", ", round(x,4))
    proxy <- leafletProxy("timeSinceFire2")
    proxy %>% clearPopups() %>% addPopups(x, y, popup = content)
  }
}
