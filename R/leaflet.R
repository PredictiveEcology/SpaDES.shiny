#' Leaflet \code{proj.4} string
#'
#' This is the projection needed to plot on \pkg{leaflet} maps.
#'
#' @export
proj4stringLFLT <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#' Convert a raster to tile images
#'
#' @description Important: \code{gdalUtils_gdalPath} environment variable must point to
#'              path where GDAL is installed.
#'
#' @note Requires GDAL (>=2) and python are found on the system \code{PATH}.
#'
#' @param raster Raster to generate tiles from.
#' @param outputPath Output folder path.
#' @param zoomRange List of numbers representing zoom value to generate tiles for.
#' @param colorTableFile Table file that contains colour values for tiles.
#'
#' @return Leaflet map url template path. # TODO: improve description
#'
#' @author Damian Rodziewicz
#' @author Alex Chubaty
#' @export
#' @importFrom gdalUtils gdal_translate gdaldem gdal_setInstallation
#' @importFrom raster minValue writeRaster
#' @importFrom reproducible checkPath
#' @rdname gdal2Tiles
gdal2Tiles <- function(raster, outputPath, zoomRange, colorTableFile) {
  message("  Making leaflet tiles...")
  filePaths <- unlist(lapply(raster, function(rstInner) {
    filename1 <- filename(rstInner)
    message("    ", filename1)
    reproducible::checkPath(outputPath, create = TRUE)
    filename2 <- file.path(outputPath, paste0("out", basename(filename1)))
    filename3 <- file.path(outputPath, paste0("out2", basename(filename1)))
    filename4 <- file.path(outputPath, paste0("out3", basename(filename1)))
    filename5 <- file.path(outputPath, paste0("out4", basename(filename1)))
    filename5 <- gsub(pattern = "tif", x = filename5, replacement = "vrt")
    foldername <- gsub(pattern = ".tif", filename2, replacement = "")

    if (anyNA(rstInner[])) {
      rstInner[is.na(rstInner[])] <- -1
    }
    if (raster::minValue(rstInner) < 0) {
      rstInner <- rstInner - raster::minValue(rstInner)
    }
    rstInner <- raster::writeRaster(x = rstInner, filename = filename2,
                                  overwrite = TRUE, datatype = "INT2S")

    # TODO: temporary workaround for https://github.com/eliotmcintire/LandWeb/issues/13
    if (is.null(getOption("gdalUtils_gdalPath")) ||
        getOption("gdalUtils_gdalPath") == "NULL")
      gdalUtils::gdal_setInstallation(rescan = TRUE)
    # end workaround

    gdalUtils::gdaldem(mode = "color-relief", filename2,
                       color_text_file = as.character(colorTableFile), filename3)
    system(paste0("python ",
                  file.path(getOption("gdalUtils_gdalPath")[[1]]$path, "rgb2pct.py "),
                  filename3, " ", filename4))
    gdalUtils::gdal_translate(of = "VRT", expand = "rgb", filename4, filename5)
    system(paste0("python ",
                  file.path(getOption("gdalUtils_gdalPath")[[1]]$path, "gdal2tiles.py "),
                  "--s_srs=EPSG:4326 ", " --zoom=", min(zoomRange), "-", max(zoomRange),
                  " ", "--srcnodata=0 ", filename5, " ", foldername),
           wait = TRUE)
    unlink(filename5)
    unlink(filename4)
    unlink(filename3)
    unlink(filename2)
    return(file.path(foldername, "{z}", "{x}", "{y}.png"))
  }))
}
