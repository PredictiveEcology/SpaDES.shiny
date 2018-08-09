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
#'                       to match \code{studyArea}. See \code{\link[reproducible]{postProcess}}.
#'
#' @return               Reactive object containing the uploaded polygon.
#'
#' @export
#' @include polygonList.R
#' @importFrom raster extension shapefile
#' @importFrom reproducible checkPath postProcess
#' @importFrom rgeos gBuffer
#' @importFrom shiny fileInput modalDialog p renderUI showModal tagList textInput
#' @importFrom SpaDES.core updateList
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
    } else {
      NULL
    }
  })

  observeEvent(input$showUploadModal, {
    ns <- session$ns

    showModal(modalDialog(
      title = "Upload a custom polygon",

      tagList(
        p("Upload a shapefile by selecting .shp and its associated files (or upload a single .zip file)."),
        p("Each polygon should have a \"shinyLabel\" attribute, otherwise a generic default will be used."),
        fileInput(ns("shpFiles"), "Upload shapefile:", multiple = TRUE,
                  accept = c(".dbf", ".prj", ".sbn", ".sbx", ".shp", ".shx", ".zip"))
      )
    ))
  })

  reproducible::checkPath(userDir, create = TRUE)

  polyName <- format(Sys.time(), "%Y-%m-%d-%Hh%Mm%S")
  polyFilename <- file.path(userDir, paste0(polyName, ".shp"))

  # do GIS checks etc.
  rctUserPoly <- reactive({
    if (is.null(input$shpFiles)) {
      NULL
    } else {
      filenames <- input$shpFiles$datapath

      tmpdir <- unique(dirname(filenames))
      filenames <- vapply(filenames, function(x) {
        fname <- "shp_upload"
        fext <- raster::extension(x)
        fullname <- file.path(tmpdir, paste0(fname, fext))
        file.rename(x, fullname)
        unname(fullname)
      }, character(1))

      zipFile <- filenames[which(raster::extension(filenames) == ".zip")]
      shpFile <- filenames[which(raster::extension(filenames) == ".shp")]

      ## perform basic checks on the user-uploaded polygon
      checkPoly <- function(shpFile, studyArea, polyFilename) {
        userPoly <- raster::shapefile(shpFile)

        ## check that attribute 'shinyLabel' exists; if not, create it.
        if (is.null(userPoly@data[["shinyLabel"]])) {
          label <- colnames(userPoly@data) %>%
            grep("name", ., ignore.case = TRUE, value = TRUE)

          if (length(label) == 0)
            label <- colnames(userPoly@data) %>%
              grep("id", ., ignore.case = TRUE, value = TRUE)

          userPoly@data[["shinyLabel"]] <- if (length(label) == 0) {
            row.names(userPoly)
          } else {
            userPoly@data[[label[1]]]
          }
        }

        ## TODO: capture warnings/messages from postProcess and tell user if
        ## something went wrong (#29). Currently, if e.g. userPoly doesn't
        ## intersect with studyArea, user gets no feedback but the userPoly isn't
        ## added to the list of selectable polygons.
        reproducible::postProcess(userPoly, filename2 = polyFilename,
                                  studyArea = studyArea, useSAcrs = TRUE)
      }

      if (length(zipFile)) {
        tmpUnzipDir <- file.path(userDir, "unzip") %>% checkPath(., create = TRUE)
        utils::unzip(zipFile, exdir = tmpUnzipDir, overwrite = TRUE, junkpaths = TRUE)
        on.exit(unlink(tmpUnzipDir, recursive = TRUE), add = TRUE)

        shpfilez <- list.files(tmpUnzipDir, pattern = ".shp", full.names = TRUE)

        if (length(shpfilez) == 0) warning("No shapefile found in uploaded zip archive.")
        if (length(shpfilez) > 1) warning("Multiple shapefiles found in uploaded zip archive.\n",
                                         "Only the first one wll be used.")

        checkPoly(shpfilez[1], studyArea, polyFilename)
      } else if (length(shpFile)) {
        if (length(shpFile) > 1) warning("Multiple shapefiles uploaded.\n",
                                         "Only the first one wll be used.")
        checkPoly(shpFile[1], studyArea, polyFilename)
      } else {
        warning("Invalid or missing shopefile (.shp).")
      }
    }
  })

  rctUserPolyList <- reactive({
    if (isTRUE(authStatus)) {
      ## TODO: allow a user to remove old uploaded polygons
      userShpFiles <- list.files(userDir, pattern = ".shp", full.names = TRUE)
      userPolyList <- lapply(userShpFiles, raster::shapefile)
      userPolyNames <- vapply(userShpFiles, function(x) {
        basename(x) %>% tools::file_path_sans_ext() %>% paste0("uploaded_", .) #nolint
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
    } else {
      list()
    }
  })

  # return the cleaned-up/verified polygon [outside the module: add this poly to the polygonList]
  return(rctUserPolyList)
}
