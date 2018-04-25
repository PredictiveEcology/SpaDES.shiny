#' Upload a shapefile
#'
#' @param id An ID string that corresponds with the ID used to call the module server function.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny uiOutput
#' @rdname uploadPolygon
uploadPolygonUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("uploader"))
}

#' @param input          shiny server input object
#' @param output         shiny server output object
#' @param session        shiny server session object
#' @param authStatus     Logical indicating whether a user can upload files.
#' @param userDir        User-specific directory in which to store uploaded files.
#' @param studyArea      A \code{Spatial} object used as a template for postprocessing
#'                       the uploaded polygon, which is cropped, reprojected, etc.
#'                       to match \code{studyArea}. See \code{\link[SpaDES.tools]{postProcess}}.
#'
#' @return               Reactive object containing the uploaded polygon.
#'
#' @export
#' @importFrom raster extension shapefile
#' @importFrom rgeos gBuffer
#' @importFrom shiny fileInput modalDialog p renderUI showModal tagList textInput
#' @importFrom SpaDES.core updateList
#' @importFrom SpaDES.tools postProcess
#' @importFrom tools file_path_sans_ext
#' @importFrom utils unzip
#' @rdname uploadPolygon
uploadPolygon <- function(input, output, session, authStatus, userDir, studyArea) {

  output$uploader <- renderUI({
    ns <- session$ns

    if (isTRUE(authStatus)) {
      tagList(
        tags$hr(),
        actionButton(ns("showUploadModal"), "Upload...")
      )
    }
  })

  observeEvent(input$showUploadModal, {
    ns <- session$ns

    showModal(modalDialog(
      title = "Upload a custom polygon",

      tagList(
        p("Upload a shapefile by selecting .shp and its associated files (or upload a single .zip file)."),
        p("Each polygon should have a \"LABEL\" attribute, otherwise a generic default will be used."),
        fileInput(ns("shpFiles"), "Upload shapefile:", multiple = TRUE,
                  accept = c(".dbf", ".prj", ".sbn", ".sbx", ".shp", ".shx", ".zip"))
      )
    ))
  })

  if (!dir.exists(userDir)) dir.create(userDir, recursive = TRUE)

  polyName <- format(Sys.time(), "%Y-%m-%d-%Hh%Mm%S")

  # do GIS checks etc.
  rctUserPoly <- reactive({
    if (is.null(input$shpFiles)) {
      NULL
    } else {
      filenames <- input$shpFiles$datapath

      tmpDir <- unique(dirname(filenames))
      filenames <- vapply(filenames, function(x) {
        fname <- "shp_upload"
        fext <- raster::extension(x)
        fullname <- file.path(tmpDir, paste0(fname, fext))
        file.rename(x, fullname)
        unname(fullname)
      }, character(1))

      zipFile <- filenames[which(raster::extension(filenames) == ".zip")]
      shpFile <- filenames[which(raster::extension(filenames) == ".shp")]

      # save polygon to the USERNAME dir with timestamp
      if (length(zipFile)) {
        ## TODO: use SpaDES.tools::prepInputs
        utils::unzip(zipFile, exdir = userDir, overwrite = TRUE, junkpaths = TRUE)
        fname <- list.files(userDir, pattern = ".shp", full.names = TRUE)
        raster::shapefile(fname)
      } else if (length(shpFile)) {
        userPoly <- raster::shapefile(shpFile)
        userPolySR <- SpaDES.tools::postProcess(userPoly, studyArea = studyArea, useSAcrs = TRUE)

        ## TODO: check that attribute 'LABEL' exists for use as shinyLabel, if not, create it.

        if (!is.null(studyArea)) {
          studyAreaLFLT <- spTransform(studyArea, proj4stringLFLT)
          userPolyLFLT <- SpaDES.tools::postProcess(userPoly, studyArea = studyAreaLFLT, useSAcrs = TRUE)
        }

        fname <- file.path(userDir, paste0(polyName, ".shp"))
        raster::shapefile(userPoly, filename = fname)
        raster::shapefile(fname)
      } else {
        warning("Invalid or missing shopefile (.shp).")
      }
    }
  })

  rctUserPolyList <- reactive({
    ## TODO: allow a user to remove old uploaded polygons
    userShpFiles <- list.files(userDir, pattern = ".shp", full.names = TRUE)
    userPolyList <- lapply(userShpFiles, raster::shapefile)
    userPolyNames <- vapply(userShpFiles, function(x) {
      basename(x) %>% tools::file_path_sans_ext() %>% paste0("uploaded_", .)
    }, character(1))
    names(userPolyList) <- userPolyNames

    newUploadPoly <- if (is.null(rctUserPoly())) {
      NULL
    } else {
      out <- list(rctUserPoly())
      names(out) <- paste0("uploaded_", polyName)
      out
    }

    SpaDES.core::updateList(userPolyList, newUploadPoly)
  })

  # return the cleaned-up/verified polygon [outside the module: add this poly to the polygonList]
  return(rctUserPolyList)
}
