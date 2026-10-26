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
  maps <- .shineById(Filter(function(o) o$kind == "map", objs))
  figs <- .shineById(Filter(function(o) o$kind == "figure", objs))
  expect_setequal(names(maps),
                  c("rsf_areaX", "multi_areaX: bandA", "multi_areaX: bandB", "classMap_areaX"))
  expect_setequal(names(figs), c("summary_static", "burn_areaX"))

  # rsf is a 3-step continuous series, sorted, with no NA times
  expect_false(maps[["rsf_areaX"]]$categorical)
  expect_equal(maps[["rsf_areaX"]]$times$time, c(2000, 2010, 2020))

  # multi-band -> one object per band, both pointing at the same files
  expect_equal(maps[["multi_areaX: bandA"]]$band, 1L)
  expect_equal(maps[["multi_areaX: bandB"]]$band, 2L)

  # categorical detected
  expect_true(maps[["classMap_areaX"]]$categorical)

  # figures: static has NA time, burn is a series
  expect_true(all(is.na(figs[["summary_static"]]$times$time)))
  expect_equal(figs[["burn_areaX"]]$times$time, c(2000, 2010))

  # the .aux.xml sidecar is not its own object
  expect_false(any(grepl("aux", vapply(objs, `[[`, character(1), "id"))))
})

test_that("a name shared by a .tif and a .png yields BOTH a map and a figure", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  tmpl <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 40, ymin = 0, ymax = 40,
                      crs = "EPSG:3857")
  terra::writeRaster(terra::setValues(tmpl, runif(16)), file.path(d, "rsfBinMap_year2030.tif"))
  writeLines("x", file.path(d, "rsfBinMap_year2030.png"))
  objs <- .shineScan(d)
  kinds <- vapply(objs, `[[`, character(1), "kind")
  ids <- vapply(objs, `[[`, character(1), "id")
  expect_true("map" %in% kinds && "figure" %in% kinds)   # neither overwrote the other
  expect_equal(sum(ids == "rsfBinMap"), 2L)
})

test_that(".shineScan records the subfolder (module) of each object", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "moduleA"))
  writeLines("x", file.path(d, "moduleA", "fig_one.png"))
  writeLines("x", file.path(d, "fig_root.png"))
  figs <- .shineById(Filter(function(o) o$kind == "figure", .shineScan(d)))
  expect_equal(figs[["fig_one"]]$folder, "moduleA")
  expect_equal(figs[["fig_root"]]$folder, "")
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
  maps <- .shineById(Filter(function(o) o$kind == "map", objs))
  figs <- .shineById(Filter(function(o) o$kind == "figure", objs))

  rsf <- maps[["rsf_areaX"]]
  expect_match(.shineFileAt(rsf, 2010), "year2010")
  expect_match(.shineFileAt(rsf, 2008), "year2010")   # nearest
  expect_match(.shineFileAt(rsf, 1900), "year2000")   # clamped to nearest

  stat <- figs[["summary_static"]]
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
  maps <- .shineById(Filter(function(o) o$kind == "map", .shineScan(d)))
  has2 <- function(o) sum(!is.na(o$times$time)) >= 2L   # the Differences-tab predicate
  expect_true(has2(maps[["rsf_a"]]))
  expect_false(has2(maps[["speciesLayers_static"]]))
})

test_that(".shineIsOrdinal detects numeric-range (binned) factor labels", {
  expect_true(.shineIsOrdinal(c("[0.02 - 0.08]", "(0.08 - 0.20]", "(0.20 - 0.38]")))
  expect_true(.shineIsOrdinal(c("-1.5 - 0", "0 - 1.5")))      # unbracketed, negative
  expect_false(.shineIsOrdinal(c("forest", "water", "urban"))) # nominal
  expect_false(.shineIsOrdinal("[0 - 1]"))                     # single level
})

test_that(".shineScan flags an ordinal factor map (binned raster)", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  tmpl <- terra::rast(nrows = 6, ncols = 6, xmin = 0, xmax = 60, ymin = 0, ymax = 60,
                      crs = "EPSG:3857")
  mk <- function(yr) {
    r <- terra::setValues(tmpl, rep(0:3, length.out = 36))
    levels(r) <- data.frame(id = 0:3,
      lab = c("[0 - 0.25]", "(0.25 - 0.5]", "(0.5 - 0.75]", "(0.75 - 1]"))
    terra::writeRaster(r, file.path(d, sprintf("binMap_year%d.tif", yr)), overwrite = TRUE)
  }
  mk(2000); mk(2010)
  o <- .shineById(Filter(function(x) x$kind == "map", .shineScan(d)))[["binMap"]]
  expect_true(o$categorical)
  expect_true(o$ordinal)

  # level-index difference is plain integers (codes), not the factor values
  rb <- terra::rast(.shineFileAt(o, 2010), lyrs = o$band)
  ra <- terra::rast(.shineFileAt(o, 2000), lyrs = o$band)
  levels(rb) <- NULL; levels(ra) <- NULL
  d2 <- rb - ra
  expect_false(terra::is.factor(d2)[1])
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

test_that("a categorical map's legend shows its category names, not codes", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leafem")
  d <- withr::local_tempdir()
  make_outputs(d)
  o <- .shineById(Filter(function(x) x$kind == "map", .shineScan(d)))[["classMap_areaX"]]
  expect_equal(o$labels, data.frame(value = 0:3, label = c("a", "b", "c", "d")))

  m <- .shineAddRaster(leaflet::leaflet(), list(range = c(0, 3)), o$id, TRUE, "x.tif", "l1",
                       replaceLegend = FALSE, labels = o$labels)
  lgd <- Filter(function(cl) cl$method == "addLegend", m$x$calls)[[1]]$args[[1]]
  expect_equal(as.character(lgd$labels), c("d", "c", "b", "a"))
})

test_that("exclude drops matching files from the scan", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  make_outputs(d)
  ids <- vapply(.shineScan(d, exclude = c("burn_.*png$", "^.*multi_")), `[[`, character(1), "id")
  expect_false(any(grepl("burn|multi", ids)))
  expect_true("rsf_areaX" %in% ids)
})

test_that("transparent values become NoData in the map's COG", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 40, ymin = 0, ymax = 40,
                   crs = "EPSG:3857", vals = rep(0:3, 4))
  o <- list(times = data.frame(time = 1, file = tempfile(fileext = ".tif")), band = 1L,
            categorical = FALSE)
  terra::writeRaster(r, o$times$file)
  plain <- .shineCogForObject(o, 1)
  clear <- .shineCogForObject(o, 1, transparent = 0)
  expect_false(identical(plain$file, clear$file))   # cached separately
  expect_equal(plain$range, c(0, 3))
  expect_equal(clear$range, c(1, 3))
})

test_that("a single file is static even if its name has digits, so the slider spans real series", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  make_outputs(d)
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 40, ymin = 0, ymax = 40, crs = "EPSG:3857", vals = 1)
  terra::writeRaster(r, file.path(d, "SCANFI_2020_v2_647220px.tif"))
  maps <- .shineById(Filter(function(o) o$kind == "map", .shineScan(d)))
  o <- maps[["SCANFI_2020_v2_647220px"]]
  expect_false(is.null(o))
  expect_true(is.na(o$times$time))
  times <- unlist(lapply(maps, function(m) m$times$time))
  expect_equal(range(times, na.rm = TRUE), c(2000, 2020))   # rsf/multi series only
})

test_that("a map of whole numbers stays whole, with whole-number legend ticks", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leafem")
  ## a count (times harvested): bilinear reprojection made values like 3.2
  r <- terra::rast(nrows = 40, ncols = 40, xmin = 0, xmax = 4000, ymin = 0, ymax = 4000,
                   crs = "EPSG:3005")
  r <- terra::setValues(r, rep(0:5, length.out = 1600))
  info <- .shineMakeCog(r, "test_whole.tif", transparent = 0)
  expect_true(info$whole)
  v <- terra::values(terra::rast(file.path(.shineCogDir(), info$file)))[, 1]
  expect_true(all(v[!is.na(v)] %in% 1:5))

  legendLabels <- function(info) {
    m <- .shineAddRaster(leaflet::leaflet(), info, "count", FALSE, "x.tif", "l1",
                         replaceLegend = FALSE)
    lgd <- Filter(function(cl) cl$method == "addLegend", m$x$calls)[[1]]$args[[1]]
    as.numeric(gsub("[^0-9.-]", "", lgd$labels))
  }
  labs <- legendLabels(list(range = c(1, 5), whole = TRUE))
  expect_true(all(labs == round(labs)))
  expect_setequal(labs, 1:5)
  expect_identical(legendLabels(list(range = c(1, 1), whole = TRUE)), 1)  # one value, one entry

  f <- terra::setValues(r, seq_len(1600) / 1600)
  expect_false(.shineMakeCog(f, "test_fraction.tif")$whole)
})

test_that("a single-value map is painted the colour its legend shows", {
  skip_if_not_installed("leafem")
  m <- .shineAddRaster(leaflet::leaflet(), list(range = c(1, 1), whole = TRUE), "count", FALSE,
                       "x.tif", "l1", replaceLegend = FALSE)
  calls <- m$x$calls
  lgd <- Filter(function(cl) cl$method == "addLegend", calls)[[1]]$args[[1]]
  geo <- unlist(Filter(function(cl) cl$method == "addGeotiff", calls)[[1]]$args)
  mapCols <- unname(unique(toupper(geo[grepl("^#[0-9A-Fa-f]{6}", geo)])))
  expect_length(mapCols, 1L)
  expect_identical(mapCols, toupper(substr(as.character(lgd$colors), 1, 7)))
})

test_that("on RStudio Server, shine() prints the address instead of opening it", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  terra::writeRaster(terra::rast(nrows = 4, ncols = 4, vals = 1:16), file.path(d, "a.tif"))
  got <- NULL
  local_mocked_bindings(runApp = function(appDir, ...) got <<- list(...), .package = "shiny")

  local_mocked_bindings(.shineOnRStudioServer = function() TRUE)
  shine(d, port = 1)
  expect_identical(got$launch.browser, .shinePrintUrl)
  expect_identical(got$port, 1)
  shine(d, launch.browser = FALSE)   # the caller's choice wins
  expect_false(got$launch.browser)

  local_mocked_bindings(.shineOnRStudioServer = function() FALSE)
  shine(d)
  expect_null(got$launch.browser)    # elsewhere, runApp's default
})
