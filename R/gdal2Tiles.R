#' Convert a raster to tile images
#'
#' @description Important: `gdalUtils_gdalPath` environment variable must point to
#'              path where GDAL is installed.
#'
#' @note Requires GDAL (>=2) and python are found on the system PATH.
#'
#' @param raster Raster to generate tiles from.
#' @param outputPath Output folder path.
#' @param zoomRange List of numbers representing zoom value to generate tiles for.
#' @param colorTableFile Table file that contains color values for tiles.
#'
#' @return None. Invoked for the side-effect of creating tile images.
#'
#' @author Damian Rodziewicz
#' @export
#' @importFrom gdalUtils gdal_translate gdaldem
##' @importFrom reproducible checkPath
#' @rdname gdal2Tiles
gdal2Tiles <- function(raster, outputPath, zoomRange, colorTableFile) {
  filename1 <- filename(raster)
  reproducible::checkPath(outputPath, create = TRUE)
  filename2 <- file.path(outputPath, paste0("out", basename(filename1)))
  filename3 <- file.path(outputPath, paste0("out2", basename(filename1)))
  filename4 <- file.path(outputPath, paste0("out3", basename(filename1)))
  filename5 <- file.path(outputPath, paste0("out4", basename(filename1)))
  filename5 <- gsub(pattern = "tif", x = filename5, replacement = "vrt")
  foldername <- gsub(pattern = ".tif", filename2, replacement = "")

  if (anyNA(raster[])) {
    raster[is.na(raster[])] <- -1
  }
  if (raster::minValue(raster) < 0) {
    raster <- raster - raster::minValue(raster)
  }
  raster <- raster::writeRaster(x = raster, filename = filename2,
                                overwrite = TRUE, datatype = "INT2S")
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

  return(invisible(NULL))
}
