# Tests for the browser-independent internals of shine(): file discovery,
# time-series grouping, snapshot selection, and COG generation. The Shiny UI /
# leaflet rendering is exercised manually (it needs a real browser).

# Build a small outputs tree: two single-band continuous map time-series, one
# 2-band map, one categorical map, plus static and time-series figures.
make_outputs <- function(dir, area = "areaX") {
  terra::terraOptions(progress = 0)
  tmpl <- terra::rast(nrows = 8, ncols = 8, xmin = 0, xmax = 80,
                      ymin = 0, ymax = 80, crs = "EPSG:3857")

  cont <- function(seed) { r <- terra::setValues(tmpl, (seq_len(64) + seed) / 64); r }
  for (yr in c(2000, 2010, 2020)) {
    terra::writeRaster(cont(yr), file.path(dir, sprintf("rsf_%s_year%d.tif", area, yr)),
                       overwrite = TRUE)
  }

  # 2-band raster (one object per band expected)
  for (yr in c(2000, 2010)) {
    rr <- c(cont(yr), cont(yr + 5)); names(rr) <- c("bandA", "bandB")
    terra::writeRaster(rr, file.path(dir, sprintf("multi_%s_year%d.tif", area, yr)),
                       overwrite = TRUE)
  }

  # categorical raster
  rc <- terra::setValues(tmpl, rep(0:3, length.out = 64))
  levels(rc) <- data.frame(id = 0:3, class = c("a", "b", "c", "d"))
  terra::writeRaster(rc, file.path(dir, sprintf("classMap_%s_year2000.tif", area)),
                     overwrite = TRUE)

  # figures: one static, one 2-step time series (content irrelevant; scan only reads names)
  writeLines("x", file.path(dir, "summary_static.png"))
  writeLines("x", file.path(dir, sprintf("burn_%s_year2000.png", area)))
  writeLines("x", file.path(dir, sprintf("burn_%s_year2010.png", area)))

  # sidecar that must be ignored
  writeLines("x", file.path(dir, sprintf("rsf_%s_year2000.tif.aux.xml", area)))
}

test_that(".shineScan groups files into time-series objects and ignores sidecars", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  make_outputs(d)

  objs <- .shineScan(d)

  # 1 continuous map (rsf) + 2 bands (multi) + 1 categorical = 4 map objects
  maps <- Filter(function(o) o$kind == "map", objs)
  figs <- Filter(function(o) o$kind == "figure", objs)
  expect_setequal(vapply(maps, `[[`, character(1), "id"),
                  c("rsf_areaX", "multi_areaX: bandA", "multi_areaX: bandB", "classMap_areaX"))
  expect_setequal(vapply(figs, `[[`, character(1), "id"), c("summary_static", "burn_areaX"))

  # rsf is a 3-step continuous series, sorted, with no NA times
  rsf <- objs[["rsf_areaX"]]
  expect_false(rsf$categorical)
  expect_equal(rsf$times$time, c(2000, 2010, 2020))

  # multi-band -> one object per band, both pointing at the same files
  expect_equal(objs[["multi_areaX: bandA"]]$band, 1L)
  expect_equal(objs[["multi_areaX: bandB"]]$band, 2L)

  # categorical detected
  expect_true(objs[["classMap_areaX"]]$categorical)

  # figures: static has NA time, burn is a series
  expect_true(all(is.na(objs[["summary_static"]]$times$time)))
  expect_equal(objs[["burn_areaX"]]$times$time, c(2000, 2010))

  # the .aux.xml sidecar is not its own object
  expect_false(any(grepl("aux", names(objs))))
})

test_that(".shineTimes returns the sorted union of real timestamps", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  make_outputs(d)
  maps <- Filter(function(o) o$kind == "map", .shineScan(d))
  expect_equal(.shineTimes(maps), c(2000, 2010, 2020))
})

test_that(".shineFileAt picks the nearest timestep and handles static layers", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  make_outputs(d)
  objs <- .shineScan(d)

  rsf <- objs[["rsf_areaX"]]
  expect_match(.shineFileAt(rsf, 2010), "year2010")
  expect_match(.shineFileAt(rsf, 2008), "year2010")   # nearest
  expect_match(.shineFileAt(rsf, 1900), "year2000")   # clamped to nearest

  stat <- objs[["summary_static"]]
  expect_equal(.shineFileAt(stat, NA), stat$times$file[1])  # always the one file
})

test_that(".san makes ids safe for layer/control identifiers", {
  expect_equal(.san("multi_areaX: bandA"), "multi_areaX_bandA")
  expect_equal(.san("B - A"), "B_A")
  expect_false(grepl("[^A-Za-z0-9_]", .san("a b:c-d.e")))
})

test_that(".shineMakeCog writes a web-mercator COG and reports its value range", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100,
                   crs = "EPSG:3857")
  r <- terra::setValues(r, seq_len(100) / 100)
  info <- .shineMakeCog(r, "test_makecog.tif")
  expect_true(file.exists(file.path(.shineCogDir(), info$file)))
  expect_length(info$range, 2L)
  expect_true(all(is.finite(info$range)))
  expect_lt(info$range[1], info$range[2])
})

test_that("static continuous maps have < 2 timesteps (excluded from Differences)", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  tmpl <- terra::rast(nrows = 6, ncols = 6, xmin = 0, xmax = 60, ymin = 0, ymax = 60,
                      crs = "EPSG:3857")
  terra::writeRaster(terra::setValues(tmpl, runif(36)), file.path(d, "rsf_a_year2000.tif"))
  terra::writeRaster(terra::setValues(tmpl, runif(36)), file.path(d, "rsf_a_year2010.tif"))
  terra::writeRaster(terra::setValues(tmpl, runif(36)), file.path(d, "speciesLayers_static.tif"))
  objs <- .shineScan(d)
  has2 <- function(o) sum(!is.na(o$times$time)) >= 2L   # the Differences-tab predicate
  expect_true(has2(objs[["rsf_a"]]))
  expect_false(has2(objs[["speciesLayers_static"]]))
})

test_that(".shineSnapshots enumerates (object @ time) snapshots for custom diffs", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  make_outputs(d)
  contMaps <- Filter(function(o) o$kind == "map" && !o$categorical, .shineScan(d))
  snaps <- .shineSnapshots(contMaps)
  expect_equal(length(snaps), 7L)            # rsf(3) + multi bandA(2) + bandB(2)
  s <- snaps[[1]]
  expect_true(all(c("o", "t", "label") %in% names(s)))
  expect_true(file.exists(.shineFileAt(s$o, s$t)))
  expect_match(s$label, " @ ")
})

test_that(".shineResolvePath dispatches on path string, NULL and bad input", {
  # a length-1 path string passes through
  expect_equal(.shineResolvePath("some/dir"), "some/dir")

  # NULL/missing falls back to the spades.outputPath option
  withr::local_options(spades.outputPath = "/opt/out")
  expect_equal(.shineResolvePath(NULL), "/opt/out")

  # NULL with no option set is an error
  withr::local_options(spades.outputPath = NULL)
  expect_error(.shineResolvePath(NULL), "spades.outputPath")

  # an unsupported type is an error
  expect_error(.shineResolvePath(1:3), "simList")
})

test_that(".shineResolvePath uses outputPath() for a simList", {
  skip_if_not_installed("SpaDES.core")
  local_mocked_bindings(outputPath = function(sim) "/sim/outputs", .package = "SpaDES.core")
  fake <- structure(list(), class = "simList")
  expect_equal(.shineResolvePath(fake), "/sim/outputs")
})

test_that(".shineMakeCog downsamples when maxCells is set", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 200, ncols = 200, xmin = 0, xmax = 100, ymin = 0, ymax = 100,
                   crs = "EPSG:3857")
  r <- terra::setValues(r, runif(200 * 200))
  info <- .shineMakeCog(r, "test_downsample.tif", maxCells = 50)
  out <- terra::rast(file.path(.shineCogDir(), info$file))
  expect_lte(max(dim(out)[1:2]), 60L)   # ~50, allowing for aggregation rounding
})
