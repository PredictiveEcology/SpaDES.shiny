#' Interactive Shiny visualizer for SpaDES outputs
#'
#' Recursively scans a folder for raster (`.tif`) and image (`.png`) outputs,
#' groups files that share a base name but differ by an embedded timestamp into
#' time-series "objects", and launches a Shiny app to explore them. Maps (`.tif`)
#' are drawn on a chooseable web basemap; figures (`.png`) are shown as images.
#' Two further tabs show precomputed (`last - first`) and user-defined differences
#' between map snapshots.
#'
#' Rasters are rendered as leafem's [leafem::addGeotiff()] does, through the
#' tiled `georaster-layer-for-leaflet` canvas layer: pan/zoom redraw per-tile on
#' the client at native resolution (fast, no server round-trip per interaction).
#' Each raster is reprojected once to a web-mercator Cloud-Optimized GeoTIFF (with
#' overviews), cached on disk, and served same-origin via [shiny::addResourcePath()].
#'
#' Multi-band rasters contribute one object per band, except that layers whose names differ
#' only by a year (e.g. `Tmax_sm_future_2011` ... `Tmax_sm_future_2100`) are one time series.
#' Digits after "year" or "yr" (as SpaDES names saved outputs, e.g. `_year2030`) are always a
#' time. Otherwise the time is the run of digits that, once removed, leaves a name shared by
#' the most files (so `Tmax04_future_2031_t_25-26.tif` is 2031, not 04 or 26). A file whose
#' name no other file shares, such as `speciesLayers_2020_647220px.tif`, is a static
#' (always-shown) layer with no position on the time slider, as is a name with no digits.
#'
#' When `x` is a `simList`, maps listed in `outputs(x)` are shown in an "Output maps" tab and
#' all other maps (e.g. climate or species layers a module prepared) in an "Input maps" tab,
#' each with its own time slider.
#'
#' @param x What to visualize, resolved to a folder to scan recursively:
#'   a `simList` (its [SpaDES.core::outputPath()] is used), a single path string,
#'   or missing (uses `getOption("spades.outputPath")`).
#' @param timePattern Regex matching the timestamp token in a file name. The
#'   **last** match in the (extension-stripped) base name is used as the numeric
#'   time. Default `"[0-9]+"`.
#' @param maxCells Optional cap on pixels per side; if set, rasters larger than
#'   this are aggregated down before rendering. Default `NULL` (full resolution).
#' @param interval Initial seconds between automatic time-slider steps; also the
#'   starting value of the in-app "seconds per step" slider. Default `2`.
#' @param opacity Opacity of the raster layers (NoData stays transparent so the
#'   basemap shows through). Default `1`. Opaque layers animate seamlessly
#'   (double-buffered); values `< 1` use a single layer (a brief redraw per step,
#'   but no ghosting between semi-transparent frames).
#' @param launch If `TRUE` (default), run the app with [shiny::runApp()]. If
#'   `FALSE`, return the [shiny::shinyApp()] object (useful for testing/embedding).
#' @param transparent Numeric values drawn transparent on the maps, like NoData. Default `0`,
#'   "none" in most SpaDES maps (e.g. never harvested, never burned); `NULL` for NoData only.
#' @param exclude Optional regular expressions; files whose path matches any of them are
#'   not shown (e.g. `"_year_.*png$"` to drop per-year map images from the figures).
#' @param ... Passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#'
#' @return Invisibly, the `shinyApp` object (also when `launch = TRUE`).
#' @examples
#' \dontrun{
#' shine("outputs")        # a path
#' shine(mySim)            # a simList -> uses outputPath(mySim)
#' shine()                 # uses getOption("spades.outputPath")
#' shine("outputs", exclude = "_year_.*png$")
#' shine("outputs", transparent = NULL)   # draw 0 values too
#' }
#' @export
shine <- function(x, timePattern = "[0-9]+", maxCells = NULL, interval = 2,
                  opacity = 1, launch = TRUE, transparent = 0, exclude = NULL, ...) {
  for (p in c("shiny", "leaflet", "leafem", "terra")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' is required by shine(). Please install it.", call. = FALSE)
    }
  }
  if (missing(x)) x <- NULL
  path <- .shineResolvePath(x)
  if (!dir.exists(path)) stop("Folder not found: ", path, call. = FALSE)

  # with a simList, maps not in outputs(sim) go to an "Input maps" tab
  outputFiles <- if (inherits(x, "simList")) SpaDES.core::outputs(x)$file
  objects <- .shineScan(path, timePattern, exclude, outputFiles)
  if (length(objects) == 0L) stop("No .tif or .png files found under: ", path, call. = FALSE)

  app <- shiny::shinyApp(ui = .shineUI(objects, interval),
                         server = .shineServer(objects, path, maxCells, interval, opacity,
                                               transparent))
  if (isTRUE(launch)) shiny::runApp(app, ...)
  invisible(app)
}

# Resolve shine()'s first argument to a directory: a simList -> its outputPath(),
# a length-1 character -> itself, NULL/missing -> getOption("spades.outputPath").
.shineResolvePath <- function(x) {
  if (is.null(x)) {
    path <- getOption("spades.outputPath")
    if (is.null(path) || !nzchar(path)) {
      stop("No `x` supplied and getOption('spades.outputPath') is not set.", call. = FALSE)
    }
    return(path)
  }
  if (inherits(x, "simList")) {
    if (!requireNamespace("SpaDES.core", quietly = TRUE)) {
      stop("A simList was supplied but 'SpaDES.core' is not installed.", call. = FALSE)
    }
    return(SpaDES.core::outputPath(x))
  }
  if (is.character(x) && length(x) == 1L) return(x)
  stop("`x` must be a simList, a single path string, or missing.", call. = FALSE)
}

# ---- discovery / grouping -------------------------------------------------

# Scan `path` and return a list of "objects". Each object is a list:
#   id          character, label shown in the legend (unique within a kind)
#   kind        "map" (.tif) or "figure" (.png)
#   band        integer band index within `file` (1 for png / single-band)
#   categorical logical, TRUE for factor rasters (maps only)
#   folder      relative subfolder under `path` ("" if in the root)
#   times       data.frame(time = numeric, file = character), sorted by time;
#               a single row with time = NA means a static layer. A series of layers
#               in one file also has `band`, the layer to read at each time.
#   input       maps only: TRUE when `outputFiles` is given and none of the object's
#               files is in it (e.g. climate or species layers a module prepared)
# The list is keyed by "<kind>\r<id>" so a map and a figure that share a name
# (e.g. foo.tif and foo.png) do not collide; use .shineById() to re-key by id
# within a single kind.
.shineScan <- function(path, timePattern = "[0-9]+", exclude = NULL, outputFiles = NULL) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE,
                      pattern = "\\.(tif|png)$", ignore.case = TRUE)
  files <- files[!grepl("\\.aux\\.xml$", files, ignore.case = TRUE)]
  if (length(exclude)) files <- files[!grepl(paste(exclude, collapse = "|"), files)]
  if (length(files) == 0L) return(list())
  root <- normalizePath(path, mustWork = FALSE)
  if (length(outputFiles)) outputFiles <- normalizePath(outputFiles, mustWork = FALSE)

  kinds <- ifelse(grepl("\\.tif$", files, ignore.case = TRUE), "map", "figure")
  tk <- .shineTimeKeys(tools::file_path_sans_ext(basename(files)), kinds, timePattern)
  parsed <- Map(function(f, kind, key, time) {
    reldir <- sub(root, "", normalizePath(dirname(f), mustWork = FALSE), fixed = TRUE)
    folder <- gsub("\\\\", "/", sub("^[/\\\\]+", "", reldir))    # "" if file is in root
    list(key = key, time = time, file = f, kind = kind, folder = folder)
  }, files, kinds, tk$key, tk$time, USE.NAMES = FALSE)

  # Group by (kind, key); one band per map object is expanded below
  groupKey <- vapply(parsed, function(p) paste(p$kind, p$key, sep = "\r"), character(1))
  objects <- list()
  for (gk in unique(groupKey)) {
    members <- parsed[groupKey == gk]
    kind <- members[[1]]$kind
    key  <- members[[1]]$key
    folder <- members[[1]]$folder
    df <- data.frame(
      time = vapply(members, `[[`, numeric(1), "time"),
      file = vapply(members, `[[`, character(1), "file"),
      stringsAsFactors = FALSE
    )
    df <- df[order(df$time, na.last = TRUE), , drop = FALSE]

    if (kind == "figure") {
      objects[[paste("figure", key, sep = "\r")]] <- list(id = key, kind = "figure",
        band = 1L, categorical = FALSE, folder = folder, times = df)
      next
    }

    # map: inspect the first raster for band count / band names / factor-ness
    r <- terra::rast(df$file[1])
    isInput <- length(outputFiles) > 0L &&
      !any(normalizePath(df$file, mustWork = FALSE) %in% outputFiles)
    nb <- terra::nlyr(r)
    bnames <- names(r)
    isFac <- terra::is.factor(r)
    # In a single file, layers whose names differ by a time (e.g. Tmax_sm_future_2011 ...
    # Tmax_sm_future_2100) are one series, stepping through the layers.
    bk <- if (nb > 1L && nrow(df) == 1L) .shineTimeKeys(bnames, timePattern = timePattern)
          else list(key = bnames, time = rep(NA_real_, nb))
    for (b in seq_len(nb)) {
      bSeries <- !is.na(bk$time[b])
      if (bSeries && b != match(bk$key[b], bk$key)) next   # added with its series' 1st layer
      id <- if (nb > 1L) paste0(key, ": ", bk$key[b]) else key
      cat_b <- isTRUE(isFac[b])
      times <- df
      if (bSeries) {
        bs <- which(bk$key == bk$key[b])
        times <- data.frame(time = bk$time[bs], file = df$file[1], band = bs)
        times <- times[order(times$time), , drop = FALSE]
      }
      objects[[paste("map", id, sep = "\r")]] <- list(
        id = id, kind = "map", band = b,
        categorical = cat_b,
        ordinal = cat_b && .shineIsOrdinal(.shineFactorLabels(r, b)),
        labels = if (cat_b) .shineFactorTable(r, b),
        folder = folder, times = times, input = isInput
      )
    }
  }
  objects
}

# Split each name into a series key and a time. Each run of digits is a candidate time.
# Digits after "year" or "yr" (as SpaDES names saved outputs) are the time. Otherwise the
# time is the run whose removal leaves a key shared by the most names of the same group
# (ties: the last run): in Tmax04_future_2031_t_25-26 that is 2031, in
# speciesLayers_1985_054bb70ccd4f20e0 it is 1985. A name whose key no other name shares has
# no time (key = the whole name, time = NA): a pixel count or a version, not a time.
.shineTimeKeys <- function(stems, groups = "", timePattern = "[0-9]+") {
  groups <- rep_len(groups, length(stems))
  yrRe <- "(?i)(year|yr)[ _-]*$"
  cands <- lapply(stems, function(stem) {
    m <- gregexpr(timePattern, stem)[[1]]
    if (m[1] == -1L) return(NULL)
    lapply(seq_along(m), function(i) {
      start <- m[i]; len <- attr(m, "match.length")[i]
      left <- substr(stem, 1L, start - 1L)
      isYear <- grepl(yrRe, left, perl = TRUE)
      left <- sub("[ _-]+$", "", sub(yrRe, "", left, perl = TRUE))
      right <- sub("^[ _-]+", "", substr(stem, start + len, nchar(stem)))
      list(key = paste(c(left[nzchar(left)], right[nzchar(right)]), collapse = "_"),
           time = as.numeric(substr(stem, start, start + len - 1L)), isYear = isYear)
    })
  })
  ck <- function(cc, g) paste(g, vapply(cc, `[[`, character(1), "key"), sep = "\r")
  nShared <- table(unlist(Map(function(cc, g) if (length(cc)) unique(ck(cc, g)), cands, groups)))
  key <- stems
  time <- rep(NA_real_, length(stems))
  for (j in seq_along(stems)) {
    cc <- cands[[j]]
    if (!length(cc)) next
    yr <- which(vapply(cc, `[[`, logical(1), "isYear"))
    n <- as.integer(nShared[ck(cc, groups[j])])
    i <- if (length(yr)) yr[length(yr)] else if (max(n) > 1L) max(which(n == max(n)))
    if (length(i)) { key[j] <- cc[[i]]$key; time[j] <- cc[[i]]$time }
  }
  list(key = key, time = time)
}

# The category labels of band `b` of factor SpatRaster `r` (the active label col).
.shineFactorLabels <- function(r, b) {
  ct <- terra::cats(r)[[b]]
  if (is.null(ct) || ncol(ct) < 2L) return(character(0))
  ac <- tryCatch(terra::activeCat(r, layer = b), error = function(e) 1L)
  col <- if (length(ac) == 1L && !is.na(ac)) ac + 1L else 2L   # +1: skip the id column
  if (col > ncol(ct)) col <- ncol(ct)
  as.character(ct[[col]])
}

# The categories of band `b` of factor SpatRaster `r`: data.frame(value, label).
.shineFactorTable <- function(r, b) {
  labs <- .shineFactorLabels(r, b)
  if (length(labs) == 0L) return(NULL)
  data.frame(value = terra::cats(r)[[b]][[1]], label = labs)
}

# An "ordinal" factor is one whose every label is a numeric range, e.g.
# "[0.023542 - 0.082522]" -- a binned continuous variable. Such factors can be
# differenced on their (ordered) level index.
.shineIsOrdinal <- function(labs) {
  if (length(labs) < 2L) return(FALSE)
  num <- "-?[0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?"
  # a numeric range, optionally bracketed with [ ( on the left and ] ) on the right
  pat <- paste0("^\\s*[\\[(]?\\s*", num, "\\s*-\\s*", num, "\\s*[\\])]?\\s*$")
  all(grepl(pat, labs, perl = TRUE))
}

# Re-key a (kind-filtered) object list by id, for name-based lookup within a kind.
.shineById <- function(objs) {
  if (length(objs)) names(objs) <- vapply(objs, `[[`, character(1), "id")
  objs
}

# A downloadHandler that serves the file referenced by reactiveVal `rv`, which
# holds list(path, name) for the difference raster currently shown (or NULL).
.shineDownload <- function(rv) {
  shiny::downloadHandler(
    filename = function() { d <- rv(); if (is.null(d)) "shine_difference.tif" else d$name },
    content = function(file) {
      d <- rv()
      if (is.null(d) || !file.exists(d$path)) {
        stop("Select a difference to display before downloading.", call. = FALSE)
      }
      file.copy(d$path, file, overwrite = TRUE)
    }
  )
}

# Sorted union of real timestamps across the given objects.
.shineTimes <- function(objects) {
  ts <- unlist(lapply(objects, function(o) o$times$time), use.names = FALSE)
  sort(unique(ts[!is.na(ts)]))
}

# The single file for object `o` at (nearest to) time `t`. Static layers
# (only NA time) always return their single file.
.shineFileAt <- function(o, t) o$times$file[.shineRowAt(o, t)]

# The band of object `o` to read at time `t`: a series of layers in one file has a band
# per time; otherwise it is the object's band.
.shineBandAt <- function(o, t) {
  if (is.null(o$times$band)) o$band else o$times$band[.shineRowAt(o, t)]
}

.shineRowAt <- function(o, t) {
  tt <- o$times$time
  if (all(is.na(tt))) return(1L)
  ok <- which(!is.na(tt))
  ok[which.min(abs(tt[ok] - t))]
}

# Custom differences: maps the A and B pickers offer (continuous, with a time), the
# years each offers, and the snapshot a (map, year) pick names: list(o, t, label), or
# NULL when the year is not one of that map's (e.g. mid-way through switching maps).
.shineCustMaps <- function(contMaps) Filter(function(o) any(!is.na(o$times$time)), contMaps)
.shineYears <- function(o) sort(o$times$time[!is.na(o$times$time)])
.shineSnapAt <- function(custMaps, id, year) {
  if (length(id) != 1L || !id %in% names(custMaps)) return(NULL)
  o <- custMaps[[id]]
  t <- suppressWarnings(as.numeric(year))
  if (length(t) != 1L || !t %in% .shineYears(o)) return(NULL)
  list(o = o, t = t, label = paste0(o$id, " @ ", t))
}

# ---- COG cache (served same-origin for addGeotiff) ------------------------

.shineCogDir <- function() {
  d <- file.path(tempdir(), "shine_cog")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

.shineCogRange <- new.env(parent = emptyenv())   # cog basename -> c(lo, hi)
.shineCogWhole <- new.env(parent = emptyenv())   # cog basename -> only whole numbers?
.san <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

# Write (once) a web-mercator COG named `key` from SpatRaster `r`; return
# list(file = key, range = c(lo, hi), whole). Range via terra::minmax (block-wise).
# A map of whole numbers (e.g. counts) is resampled like a categorical one, so it
# stays whole: bilinear or mean would show 3.2 harvests.
.shineMakeCog <- function(r, key, categorical = FALSE, maxCells = NULL, transparent = NULL) {
  out <- file.path(.shineCogDir(), key)
  if (is.null(.shineCogWhole[[key]]))
    .shineCogWhole[[key]] <- categorical || .shineIsWhole(r)
  whole <- .shineCogWhole[[key]]
  if (!file.exists(out)) {
    if (length(transparent)) r <- terra::classify(r, cbind(transparent, NA))
    if (!is.null(maxCells)) {
      fact <- floor(max(dim(r)[1:2]) / maxCells)
      if (fact > 1L) r <- terra::aggregate(r, fact = fact,
                          fun = if (whole) "modal" else "mean", na.rm = TRUE)
    }
    r <- terra::project(r, "EPSG:3857", method = if (whole) "near" else "bilinear")
    suppressWarnings(
      terra::writeRaster(r, out, filetype = "COG", gdal = c("COMPRESS=DEFLATE"),
                         overwrite = TRUE))
  }
  if (is.null(.shineCogRange[[key]])) {
    .shineCogRange[[key]] <- as.vector(terra::minmax(terra::rast(out), compute = TRUE))
  }
  list(file = key, range = .shineCogRange[[key]], whole = whole)
}

.shineIsWhole <- function(r) {
  isFrac <- terra::global(r != round(r), "max", na.rm = TRUE)[[1]]
  isTRUE(isFrac == 0)
}

# Legend ticks for a gradient: whole numbers only when the map has only whole numbers
.shineBins <- function(rng, whole) {
  if (!isTRUE(whole)) return(7)
  ticks <- unique(round(pretty(rng)))
  ticks <- ticks[ticks >= floor(rng[1]) & ticks <= ceiling(rng[2])]
  if (length(ticks) < 2L) c(floor(rng[1]), ceiling(rng[2])) else ticks
}

# COG for object `o`'s band at time `t` (cache key includes mtime so edits bust it).
.shineCogForObject <- function(o, t, maxCells = NULL, transparent = NULL) {
  f <- .shineFileAt(o, t)
  b <- .shineBandAt(o, t)
  key <- paste0(.san(f), "_b", b, "_", as.integer(file.mtime(f)),
                if (length(transparent)) paste0("_t", .san(paste(transparent, collapse = "_"))), ".tif")
  .shineMakeCog(terra::rast(f, lyrs = b), key, o$categorical, maxCells, transparent)
}

# georaster-layer-for-leaflet JS, primed on the UI so leafletProxy addGeotiff()
# calls have a client handler to invoke (addGeotiff only attaches deps to the map
# object it is called on, which via proxy would never reach the page).
.shineCogDeps <- function() {
  c(utils::getFromNamespace("leafletGeoRasterDependencies", "leafem")(),
    utils::getFromNamespace("chromaJsDependencies", "leafem")())
}

# ---- rendering helpers ----------------------------------------------------

.viridis <- function() grDevices::hcl.colors(64, "viridis")
.shineOneColour <- function() .viridis()[32]  # a map with a single value, and its legend
# reversed so the low end (negative) is red and the high end (positive) is blue
.diverging <- function() rev(grDevices::hcl.colors(64, "Blue-Red 3"))

# Stream COG `info` (from .shineMakeCog) onto a leaflet proxy/map at `url` with a
# matching legend. Re-adding the same `layerId` replaces that layer once the new
# tile has loaded (georaster swaps it in place). `replaceLegend` removes the prior
# legend first (proxy updates); set FALSE when drawing into a fresh map.
.shineAddRaster <- function(map, info, id, categorical, url, layerId,
                            replaceLegend = TRUE, opacity = 1, labels = NULL) {
  rng <- info$range
  if (!all(is.finite(rng))) return(map)
  if (isTRUE(info$whole) && diff(rng) == 0) categorical <- TRUE  # one value: one legend entry
  palette <- .viridis()
  if (diff(rng) == 0) {
    rng <- rng + c(-1, 1) * (abs(rng[1]) + 1) * 1e-6
    ## one value: paint it the legend's colour; placing it in that tiny domain
    ## gave a different colour in the browser
    palette <- rep(.shineOneColour(), 2)
  }
  map <- .shineGeotiff(map, url, layerId, layerId, opacity, palette, rng)
  lgd <- paste0("lgd_", .san(id))
  if (replaceLegend) map <- leaflet::removeControl(map, lgd)   # one legend per object
  if (categorical) {                                           # discrete, highest on top
    vals <- if (is.null(labels)) seq(floor(info$range[1]), ceiling(info$range[2])) else  # names, not codes
      labels$value[labels$value >= info$range[1] & labels$value <= info$range[2]]
    vals <- rev(vals)
    pal <- leaflet::colorFactor(.viridis(), domain = vals, na.color = "transparent")
    cols <- if (length(vals) == 1L) .shineOneColour() else pal(vals)
    labs <- if (is.null(labels)) vals else labels$label[match(vals, labels$value)]
    leaflet::addLegend(map, position = "bottomleft", colors = cols, labels = labs,
                       title = id, group = id, layerId = lgd)
  } else {                                                     # gradient, highest on top
    leaflet::addLegend(map, position = "bottomleft",
      pal = leaflet::colorNumeric(rev(.viridis()), domain = rng, na.color = "transparent"),
      values = rng, title = id, group = id, layerId = lgd, bins = .shineBins(rng, info$whole),
      labFormat = leaflet::labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
  }
}

# Draw the COG at `url` as layer `layerId`, replacing any layer with that id. This is what
# leafem::addGeotiff() does in the browser, without the click and mousemove handlers it adds
# on every call and never removes: each held the whole decoded raster, so a playing time
# slider kept every map it showed and the tab ran out of memory after a few hours
# (leafem 0.2.5, georaster-binding.js; fix proposed in r-spatial/leafem#111). shine() never
# used those handlers (imagequery = FALSE).
.shineGeotiff <- function(map, url, layerId, group, opacity, palette, domain) {
  leaflet::invokeMethod(map, leaflet::getMapData(map), "shineAddGeotiff", url, group,
                        layerId, opacity, list(palette = palette, domain = domain))
}

.shineGeotiffJs <- '
LeafletWidget.methods.shineAddGeotiff = function (url, group, layerId, opacity, colorOptions) {
  var map = this;
  fetch(url)
    .then(function (response) { return response.arrayBuffer(); })
    .then(parseGeoraster)
    .then(function (georaster) {
      var scale = chroma.scale(colorOptions.palette).domain(colorOptions.domain);
      var layer = new GeoRasterLayer({
        georaster: georaster, resolution: 96, opacity: opacity, pane: "tilePane",
        pixelValuesToColorFn: function (values) {
          var v = values[0];
          if (isNaN(v) || v === georaster.noDataValue) return "transparent";
          return scale(v).hex();
        }
      });
      map.layerManager.addLayer(layer, "image", layerId, group);
    });
};'

# Stream a difference COG (diverging palette centered at 0) onto a leaflet proxy.
.shineAddDiff <- function(map, info, id, url, opacity = 1) {
  rng <- info$range
  lim <- max(abs(rng)); if (!is.finite(lim) || lim == 0) lim <- 1
  dom <- c(-lim, lim)
  map <- .shineGeotiff(map, url, .san(id), id, opacity, .diverging(), dom)
  # legend reversed (palette + labels) so the most positive value is on top
  leaflet::addLegend(map, position = "bottomleft",
    pal = leaflet::colorNumeric(rev(.diverging()), domain = dom, na.color = "transparent"),
    values = dom, title = id, group = id, bins = .shineBins(dom, info$whole),
    labFormat = leaflet::labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
}

.basemaps <- c("OpenStreetMap" = "OpenStreetMap",
               "CartoDB Positron" = "CartoDB.Positron",
               "Esri World Imagery" = "Esri.WorldImagery")

# Lon/lat bounding box (c(lng1, lat1, lng2, lat2)) of the first map object, used
# to fit the initial leaflet view. NULL if there are no map objects.
.shineBounds <- function(objects) {
  mapObjs <- Filter(function(o) o$kind == "map", objects)
  if (length(mapObjs) == 0L) return(NULL)
  e <- try(terra::ext(terra::project(terra::rast(mapObjs[[1]]$times$file[1]),
                                      "EPSG:4326")), silent = TRUE)
  if (inherits(e, "try-error")) return(NULL)
  as.numeric(c(e[1], e[3], e[2], e[4]))
}

# A base leaflet map fit to `bounds` (or a default Canada view). zoomSnap < 1 lets
# fitBounds pick a fractional zoom that hugs the extent instead of a farther one.
.shineBaseMap <- function(bounds) {
  m <- leaflet::addProviderTiles(
    leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.25, zoomDelta = 0.25)),
    .basemaps[[1]])
  if (is.null(bounds)) return(leaflet::setView(m, lng = -123, lat = 62, zoom = 5))
  dx <- (bounds[3] - bounds[1]) * 0.06   # inset a little so the view sits closer
  dy <- (bounds[4] - bounds[2]) * 0.06
  leaflet::fitBounds(m, bounds[1] + dx, bounds[2] + dy, bounds[3] - dx, bounds[4] - dy)
}

# The map tabs: list(tab = input-id prefix, title, objs). Maps that are inputs (not listed in
# outputs(sim)) get their own tab; with no inputs flagged there is a single "Maps" tab.
.shineMapTabs <- function(mapObjs) {
  isIn <- vapply(mapObjs, function(o) isTRUE(o$input), logical(1))
  if (!any(isIn)) return(list(list(tab = "map", title = "Maps", objs = mapObjs)))
  Filter(function(x) length(x$objs) > 0L, list(
    list(tab = "map", title = "Output maps", objs = mapObjs[!isIn]),
    list(tab = "inmap", title = "Input maps", objs = mapObjs[isIn])))
}

# ---- UI -------------------------------------------------------------------

.shineUI <- function(objects, interval = 2) {
  mapObjs <- .shineById(Filter(function(o) o$kind == "map", objects))
  figObjs <- .shineById(Filter(function(o) o$kind == "figure", objects))
  contMaps <- Filter(function(o) !o$categorical, mapObjs)
  # last - first works for continuous maps and ordinal factors (>= 2 timestamps)
  diffMaps <- Filter(function(o) (!o$categorical || isTRUE(o$ordinal)) &&
                       sum(!is.na(o$times$time)) >= 2L, mapObjs)

  # custom differences: a map and a year for each of A and B; B starts one year after A
  custMaps <- .shineCustMaps(contMaps)
  custPick <- function(ab, yearIdx) {
    yrs <- if (length(custMaps)) .shineYears(custMaps[[1]]) else numeric(0)
    shiny::tagList(
      shiny::selectInput(paste0("cust_", ab, "_map"), toupper(ab), names(custMaps)),
      shiny::selectInput(paste0("cust_", ab, "_year"), NULL, yrs,
                         selected = if (length(yrs)) yrs[min(yearIdx, length(yrs))]))
  }

  timeSliderUI <- function(id, times) {
    if (length(times) < 2L) return(NULL)
    shiny::absolutePanel(
      bottom = 10, left = 10, right = 10, fixed = TRUE,
      style = "background: rgba(255,255,255,0.85); padding: 8px; border-radius: 6px; z-index: 1000;",
      shiny::fluidRow(
        shiny::column(2, shiny::actionButton(paste0(id, "_play"), "Pause")),
        shiny::column(10,
          shiny::sliderInput(paste0(id, "_time"), NULL, min = min(times), max = max(times),
                             value = min(times), step = NULL, sep = "", width = "100%",
                             ticks = TRUE))
      )
    )
  }

  speedSlider <- function(id) {
    shiny::sliderInput(id, "Seconds per step", min = 0.5, max = 10,
                       value = interval, step = 0.5, width = "100%")
  }

  # small bottom-right caption showing the source file(s) of what's displayed
  fileInfoPanel <- function(id) {
    shiny::absolutePanel(
      bottom = 96, right = 10, fixed = TRUE,   # clear the time slider at the bottom
      style = paste("background: rgba(255,255,255,0.8); padding: 3px 8px;",
                    "border-radius: 4px; z-index: 1000; font-size: 11px;",
                    "max-width: 45vw; word-break: break-all; text-align: right;"),
      shiny::uiOutput(id))
  }

  # A single radio-button group whose items are grouped under bold folder headers
  # (folder = subfolder under the scanned path; items with no folder get no header).
  groupedRadios <- function(inputId, objs, selectedId) {
    item <- function(o) shiny::tags$div(class = "radio", style = "margin: 1px 0;",
      shiny::tags$label(
        shiny::tags$input(type = "radio", name = inputId, value = o$id,
          checked = if (identical(o$id, selectedId)) "checked"),
        shiny::tags$span(style = "margin-left: 4px;", o$id)))
    folders <- vapply(objs, function(o) if (is.null(o$folder)) "" else o$folder, character(1))
    blocks <- lapply(unique(folders), function(fld) {
      items <- lapply(objs[folders == fld], item)
      if (nzchar(fld)) c(list(shiny::tags$div(style = "font-weight: bold; margin-top: 8px;", fld)), items)
      else items
    })
    shiny::tags$div(id = inputId, class = "shiny-input-radiogroup", do.call(c, blocks))
  }

  legendPanel <- function(content) {
    shiny::absolutePanel(
      top = 70, right = 10, width = 280, fixed = TRUE, draggable = TRUE,
      style = "background: rgba(255,255,255,0.9); padding: 10px; border-radius: 6px; z-index: 1000; max-height: 70vh; overflow-y: auto;",
      content
    )
  }

  mapTab <- function(tab, title, objs) {
    times <- .shineTimes(objs)
    shiny::tabPanel(title,
      shiny::div(style = "position: relative;",
        leaflet::leafletOutput(paste0(tab, "_map"), width = "100%", height = "92vh"),
        legendPanel(shiny::tagList(
          shiny::selectInput(paste0(tab, "_basemap"), "Basemap", choices = .basemaps),
          shiny::tags$hr(),
          shiny::radioButtons(paste0(tab, "_objs"), "Layer",
                              choices = vapply(objs, `[[`, character(1), "id"),
                              selected = if (length(objs)) objs[[1]]$id else character(0)),
          if (length(times) >= 2L) shiny::tagList(shiny::tags$hr(), speedSlider(paste0(tab, "_speed")))
        )),
        fileInfoPanel(paste0(tab, "_fileinfo")),
        timeSliderUI(tab, times)
      )
    )
  }
  mapTabs <- lapply(.shineMapTabs(mapObjs), function(x) mapTab(x$tab, x$title, x$objs))

  ui <- do.call(shiny::navbarPage, c(list(
    "shine", id = "tabs", collapsible = TRUE,
    header = shiny::tags$head(shiny::tags$style(shiny::HTML(paste(
      ".leaflet-container { background: #ddd; }",
      # lift the bottom-left color legend up so the time slider doesn't cover it
      ".leaflet-bottom.leaflet-left { margin-bottom: 72px; }"))),
      shiny::tags$script(shiny::HTML(.shineMemoryCheckJs)),
      shiny::tags$script(shiny::HTML(.shineFigJs)),
      shiny::tags$script(shiny::HTML(.shineGeotiffJs)))),
    mapTabs, list(

    # ---- Map tabs (outputs, and inputs when outputs(sim) told them apart) ----
    # ---- Figures tab ----
    shiny::tabPanel("Figures",
      shiny::div(style = "position: relative; min-height: 92vh;",
        # reserve the legend's width on the right so figures scale to fit beside it
        shiny::div(style = paste("height: 92vh; overflow-y: auto; text-align: center;",
                                 "padding-right: 300px; box-sizing: border-box;"),
                   shiny::uiOutput("fig_ui")),
        legendPanel(shiny::tagList(
          shiny::tags$label("Figure"),
          groupedRadios("fig_objs", figObjs,
                        if (length(figObjs)) figObjs[[1]]$id else character(0)),
          shiny::tags$hr(), speedSlider("fig_speed")
        )),
        fileInfoPanel("fig_fileinfo"),
        shiny::uiOutput("fig_slider")   # only shown for time-series figures
      )
    ),

    # ---- Differences tab ----
    shiny::tabPanel("Change from start to end",
      shiny::div(style = "position: relative;",
        leaflet::leafletOutput("diff_map", width = "100%", height = "92vh"),
        legendPanel(shiny::tagList(
          shiny::selectInput("diff_basemap", "Basemap", choices = .basemaps),
          shiny::tags$hr(),
          shiny::helpText("last - first (continuous time-series maps)"),
          shiny::radioButtons("diff_objs", "Map",
                              choices = vapply(diffMaps, `[[`, character(1), "id"),
                              selected = if (length(diffMaps)) diffMaps[[1]]$id else character(0)),
          shiny::tags$hr(),
          shiny::downloadButton("diff_dl", "Download GeoTIFF")
        ))
      )
    ),

    # ---- Custom differences tab ----
    shiny::tabPanel("Custom differences",
      shiny::div(style = "position: relative;",
        leaflet::leafletOutput("cust_map", width = "100%", height = "92vh"),
        legendPanel(shiny::tagList(
          shiny::selectInput("cust_basemap", "Basemap", choices = .basemaps),
          shiny::tags$hr(),
          shiny::helpText("Pick one A and one B -> B - A"),
          custPick("a", 1L), custPick("b", 2L),
          shiny::tags$hr(),
          shiny::downloadButton("cust_dl", "Download GeoTIFF")
        ))
      )
    )
  )))
  # load the tiled raster renderer's JS at page start so proxy addGeotiff() works
  htmltools::attachDependencies(ui, .shineCogDeps(), append = TRUE)
}

# Figure frames: keep the preloaded images referenced so the browser holds them, and show
# a frame only once it has loaded (the latest request wins if steps outpace loading).
.shineFigJs <- '
(function () {
  var held = [], want = null;
  Shiny.addCustomMessageHandler("shine_fig_preload", function (srcs) {
    held = srcs.map(function (s) { var im = new Image(); im.src = s; return im; });
  });
  Shiny.addCustomMessageHandler("shine_fig_src", function (src) {
    want = src;
    var im = new Image();
    im.onload = function () {
      var el = document.getElementById("shine_fig_img");
      if (el && want === src) el.src = src;
    };
    im.src = src;
  });
})();'

# The browser reads each map into memory blocks (ArrayBuffers). A tab that can no longer get
# them leaves every map blank with no message, so the page asks for one block on load and
# every minute, and says so. The cause seen was shine's own leak through leafem::addGeotiff()
# (see .shineGeotiff()). Tabs of one server can share a browser process, and shine may run in
# RStudio's Viewer, so the memory may be held by another tab: hence the second step.
.shineMemoryMessage <- paste(
  "This browser tab can't get the memory it needs to draw maps, so maps will stay blank.",
  "Reload this page. If the maps are still blank, close every tab of this server",
  "(including RStudio) and open it again.")

.shineMemoryCheckJs <- paste0('
(function () {
  var msg = "', .shineMemoryMessage, '";
  function check() {
    try { new ArrayBuffer(64 * 1024 * 1024); return; } catch (e) {}
    if (document.getElementById("shine-memory")) return;
    var d = document.createElement("div");
    d.id = "shine-memory"; d.textContent = msg;
    d.style.cssText = "position: fixed; top: 0; left: 0; right: 0; z-index: 10000; padding: 10px;" +
      "background: #b71c1c; color: #fff; font-weight: bold; text-align: center;";
    document.body.appendChild(d);
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue("shine_memory", true);
  }
  $(document).on("shiny:connected", check);
  setInterval(check, 60000);
})();')

# ---- Server ---------------------------------------------------------------

.shineServer <- function(objects, path, maxCells, interval = 2, opacity = 1,
                         transparent = NULL) {
  mapObjs  <- .shineById(Filter(function(o) o$kind == "map", objects))
  figObjs  <- .shineById(Filter(function(o) o$kind == "figure", objects))
  contMaps <- Filter(function(o) !o$categorical, mapObjs)
  bounds <- .shineBounds(objects)

  # serve figure PNGs and the COG cache to the browser (same-origin)
  root <- normalizePath(path)
  shiny::addResourcePath("shineimg", root)
  shiny::addResourcePath("shinecog", .shineCogDir())
  relPath <- function(f) {   # path relative to the user-supplied scan root
    gsub("\\\\", "/", sub("^[/\\\\]", "", sub(root, "", normalizePath(f), fixed = TRUE)))
  }
  figUrl <- function(f) paste0("shineimg/", relPath(f))
  cogUrl <- function(file) paste0("shinecog/", file)

  function(input, output, session) {
    shiny::observeEvent(input$shine_memory, message(.shineMemoryMessage), once = TRUE)

    # --- auto-advancing time sliders (default playing, only when a layer is on) ---
    # A tab's slider advances only while that tab is shown.
    playing <- list(map = shiny::reactiveVal(TRUE), inmap = shiny::reactiveVal(TRUE))
    advancer <- function(tab, times, selId, title) {
      if (length(times) < 2L) return()
      shiny::observeEvent(input[[paste0(tab, "_play")]], {
        playing[[tab]](!playing[[tab]]())
        shiny::updateActionButton(session, paste0(tab, "_play"),
                                  label = if (playing[[tab]]()) "Pause" else "Play")
      })
      shiny::observe({
        if (!isTRUE(playing[[tab]]())) return()
        if (!identical(input$tabs, title)) return()
        if (length(input[[selId]]) == 0L) return()  # nothing shown -> don't advance
        secs <- input[[paste0(tab, "_speed")]]; if (is.null(secs)) secs <- interval
        shiny::invalidateLater(secs * 1000, session)
        cur <- shiny::isolate(input[[paste0(tab, "_time")]])
        if (is.null(cur)) cur <- times[1]
        idx <- which.min(abs(times - cur))          # snap to nearest listed time
        nxt <- times[(idx %% length(times)) + 1L]   # advance, looping
        shiny::updateSliderInput(session, paste0(tab, "_time"), value = nxt)
      })
    }
    # --- Map tabs ---
    # Each object animates by re-adding a layerId, which makes georaster replace
    # that layer in place (LayerManager.addLayer removes the old same-id layer).
    # Opaque layers (opacity >= 1) double-buffer with two alternating ids so the
    # previous frame stays visible until the new one paints (seamless, and the
    # opaque new frame fully covers the old -> no ghosting). Semi-transparent
    # layers use one id (a brief redraw per step, but stacking two would ghost).
    # Adding/removing layers or changing basemap rebuilds the map (the reliable
    # way to drop a georaster layer), preserving the current view.
    mapTabServer <- function(tab, title, objs) {
      force(title)  # read later by the advancer, after the loop below has moved on
      times <- .shineTimes(objs)
      advancer(tab, times, paste0(tab, "_objs"), title)
      mapView <- shiny::reactiveValues(center = NULL, zoom = NULL)
      shiny::observeEvent(input[[paste0(tab, "_map_center")]],
                          mapView$center <- input[[paste0(tab, "_map_center")]])
      shiny::observeEvent(input[[paste0(tab, "_map_zoom")]],
                          mapView$zoom <- input[[paste0(tab, "_map_zoom")]])
      buf <- new.env(parent = emptyenv())   # object id -> current buffer letter (opaque only)
      animLayerId <- function(id, init = FALSE) {
        if (opacity < 1) return(.san(id))            # single layer
        if (init) { buf[[id]] <- "A"; return(paste0(.san(id), "_A")) }
        nxt <- if (identical(buf[[id]], "A")) "B" else "A"
        buf[[id]] <- nxt
        paste0(.san(id), "_", nxt)
      }
      curMapTime <- function() if (length(times)) {
        v <- input[[paste0(tab, "_time")]]; if (is.null(v)) times[1] else v
      } else NA_real_

      output[[paste0(tab, "_map")]] <- leaflet::renderLeaflet({
        sel <- input[[paste0(tab, "_objs")]]
        bm <- if (is.null(input[[paste0(tab, "_basemap")]])) .basemaps[[1]] else input[[paste0(tab, "_basemap")]]
        t <- shiny::isolate(curMapTime())
        m <- leaflet::addProviderTiles(
          leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.25, zoomDelta = 0.25)), bm)
        cv <- shiny::isolate(mapView$center); zm <- shiny::isolate(mapView$zoom)
        if (!is.null(cv) && !is.null(zm)) {
          m <- leaflet::setView(m, cv$lng, cv$lat, zm)              # keep the user's view
        } else if (!is.null(bounds)) {
          dx <- (bounds[3] - bounds[1]) * 0.06; dy <- (bounds[4] - bounds[2]) * 0.06
          m <- leaflet::fitBounds(m, bounds[1] + dx, bounds[2] + dy, bounds[3] - dx, bounds[4] - dy)
        } else {
          m <- leaflet::setView(m, lng = -123, lat = 62, zoom = 5)
        }
        rm(list = ls(buf), envir = buf)                            # reset buffer state
        for (o in objs[sel]) {
          if (is.null(o)) next
          info <- .shineCogForObject(o, t, maxCells, transparent)
          m <- .shineAddRaster(m, info, o$id, o$categorical, cogUrl(info$file),
                               animLayerId(o$id, init = TRUE), replaceLegend = FALSE,
                               opacity = opacity, labels = o$labels)
        }
        m
      })

      shiny::observeEvent(input[[paste0(tab, "_time")]], {                        # animate selected layers
        sel <- input[[paste0(tab, "_objs")]]
        if (length(sel) == 0L) return()
        t <- input[[paste0(tab, "_time")]]
        m <- leaflet::leafletProxy(paste0(tab, "_map"))
        for (o in objs[sel]) {
          if (is.null(o)) next
          info <- .shineCogForObject(o, t, maxCells, transparent)
          m <- .shineAddRaster(m, info, o$id, o$categorical, cogUrl(info$file),
                               animLayerId(o$id), replaceLegend = TRUE, opacity = opacity,
                               labels = o$labels)
        }
      })
      output[[paste0(tab, "_fileinfo")]] <- shiny::renderUI({               # source file(s) on display
        sel <- input[[paste0(tab, "_objs")]]
        if (length(sel) == 0L) return(NULL)
        t <- curMapTime()
        shiny::HTML(paste(vapply(objs[sel], function(o) relPath(.shineFileAt(o, t)),
                                 character(1)), collapse = "<br>"))
      })
    }
    for (x in .shineMapTabs(mapObjs)) mapTabServer(x$tab, x$title, x$objs)

    # --- Figures tab (one figure via radio; slider only for time-series figures) ---
    figSelTimes <- shiny::reactive({
      id <- input$fig_objs
      if (is.null(id) || !nzchar(id)) return(numeric(0))
      tt <- figObjs[[id]]$times$time
      sort(unique(tt[!is.na(tt)]))
    })
    output$fig_slider <- shiny::renderUI({
      times <- figSelTimes()
      if (length(times) < 2L) return(NULL)        # single image -> no slider
      shiny::absolutePanel(
        bottom = 10, left = 10, right = 10, fixed = TRUE,
        style = "background: rgba(255,255,255,0.85); padding: 8px; border-radius: 6px; z-index: 1000;",
        shiny::fluidRow(
          shiny::column(2, shiny::actionButton("fig_play", "Pause")),
          shiny::column(10, shiny::sliderInput("fig_time", NULL, min = min(times),
            max = max(times), value = min(times), step = NULL, sep = "", width = "100%",
            ticks = TRUE))))
    })
    # The <img> is drawn once per figure; each time step swaps its src only after the next
    # image has loaded (see .shineFigJs), and every frame is fetched up front, so the figure
    # does not flash blank between steps, even on the first pass.
    output$fig_ui <- shiny::renderUI({
      id <- input$fig_objs
      if (is.null(id) || !nzchar(id)) return(shiny::helpText("Select a figure."))
      o <- figObjs[[id]]
      times <- figSelTimes()
      t <- if (length(times) >= 2L) {
        v <- shiny::isolate(input$fig_time); if (is.null(v)) times[1] else v
      } else NA_real_
      if (length(times) >= 2L) {
        session$sendCustomMessage("shine_fig_preload", as.list(figUrl(o$times$file)))
      }
      # bound by both width and height so tall figures fit fully (no cut-off bottom)
      shiny::tags$img(id = "shine_fig_img", src = figUrl(.shineFileAt(o, t)),
                      style = paste("max-width: 98%; max-height: 90vh;",
                                    "width: auto; height: auto; margin: 8px auto; display: block;"))
    })
    shiny::observeEvent(input$fig_time, {
      id <- input$fig_objs
      if (is.null(id) || !nzchar(id)) return()
      session$sendCustomMessage("shine_fig_src", figUrl(.shineFileAt(figObjs[[id]], input$fig_time)))
    })
    output$fig_fileinfo <- shiny::renderUI({
      id <- input$fig_objs
      if (is.null(id) || !nzchar(id)) return(NULL)
      times <- figSelTimes()
      t <- if (length(times) >= 2L) { v <- input$fig_time; if (is.null(v)) times[1] else v } else NA_real_
      shiny::HTML(relPath(.shineFileAt(figObjs[[id]], t)))
    })
    figPlaying <- shiny::reactiveVal(TRUE)
    shiny::observeEvent(input$fig_play, {
      figPlaying(!figPlaying())
      shiny::updateActionButton(session, "fig_play",
                                label = if (figPlaying()) "Pause" else "Play")
    })
    shiny::observe({                              # animate only when >1 image
      if (!isTRUE(figPlaying())) return()
      times <- figSelTimes()
      if (length(times) < 2L) return()
      secs <- input$fig_speed; if (is.null(secs)) secs <- interval
      shiny::invalidateLater(secs * 1000, session)
      cur <- shiny::isolate(input$fig_time); if (is.null(cur)) cur <- times[1]
      idx <- which.min(abs(times - cur))
      shiny::updateSliderInput(session, "fig_time", value = times[(idx %% length(times)) + 1L])
    })

    # --- Differences tab (last - first) ---
    output$diff_map <- leaflet::renderLeaflet(.shineBaseMap(bounds))
    shiny::observeEvent(input$diff_basemap, {
      leaflet::leafletProxy("diff_map") |>
        leaflet::clearTiles() |> leaflet::addProviderTiles(input$diff_basemap)
    })
    diffDL <- shiny::reactiveVal(NULL)            # list(path, name) of the on-the-fly COG
    # The map is built in the browser only when its tab is first shown; layers sent before it
    # reports its zoom are lost, so the tab stayed empty until it was shown a second time.
    diffReady <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$diff_map_zoom, diffReady(TRUE), once = TRUE)
    shiny::observe({
      if (!identical(input$tabs, "Change from start to end") || !diffReady()) return()
      sel <- input$diff_objs
      m <- leaflet::clearControls(leaflet::clearImages(leaflet::leafletProxy("diff_map")))
      diffDL(NULL)
      for (id in sel) {
        o <- mapObjs[[id]]
        if (is.null(o)) next
        tt <- sort(o$times$time[!is.na(o$times$time)])
        if (length(tt) < 2L) next
        rLast  <- terra::rast(.shineFileAt(o, max(tt)), lyrs = .shineBandAt(o, max(tt)))
        rFirst <- terra::rast(.shineFileAt(o, min(tt)), lyrs = .shineBandAt(o, min(tt)))
        if (isTRUE(o$ordinal)) {                       # difference the level index
          levels(rLast) <- NULL; levels(rFirst) <- NULL   # terra S4 method, gives codes
        }
        r <- rLast - rFirst
        info <- .shineMakeCog(r, paste0(.san(id), "_diff_", min(tt), "_", max(tt), "_t0.tif"),
                              maxCells = maxCells, transparent = 0)  # no change: transparent
        m <- .shineAddDiff(m, info, paste0(id, " (last-first)"), cogUrl(info$file), opacity)
        diffDL(list(path = file.path(.shineCogDir(), info$file),
                    name = paste0(.san(id), "_change_", min(tt), "-", max(tt), ".tif")))
      }
    })
    output$diff_dl <- .shineDownload(diffDL)

    # --- Custom differences (B - A) ---
    custMaps <- .shineCustMaps(contMaps)
    for (ab in c("a", "b")) local({                 # a new map offers its own years
      yearId <- paste0("cust_", ab, "_year")
      shiny::observeEvent(input[[paste0("cust_", ab, "_map")]], {
        yrs <- .shineYears(custMaps[[input[[paste0("cust_", ab, "_map")]]]])
        cur <- shiny::isolate(input[[yearId]])
        shiny::updateSelectInput(session, yearId, choices = yrs,
                                 selected = if (!is.null(cur) && cur %in% yrs) cur else yrs[1])
      }, ignoreInit = TRUE)
    })
    custDL <- shiny::reactiveVal(NULL)            # list(path, name) of the on-the-fly COG
    output$cust_map <- leaflet::renderLeaflet(.shineBaseMap(bounds))
    custReady <- shiny::reactiveVal(FALSE)        # as diffReady: wait for the map to exist
    shiny::observeEvent(input$cust_map_zoom, custReady(TRUE), once = TRUE)
    shiny::observeEvent(input$cust_basemap, {
      leaflet::leafletProxy("cust_map") |>
        leaflet::clearTiles() |> leaflet::addProviderTiles(input$cust_basemap)
    })
    shiny::observe({
      if (!identical(input$tabs, "Custom differences") || !custReady()) return()
      sa <- .shineSnapAt(custMaps, input$cust_a_map, input$cust_a_year)
      sb <- .shineSnapAt(custMaps, input$cust_b_map, input$cust_b_year)
      m <- leaflet::clearControls(leaflet::clearImages(leaflet::leafletProxy("cust_map")))
      custDL(NULL)
      if (!is.null(sa) && !is.null(sb)) {
        ra <- terra::rast(.shineFileAt(sa$o, sa$t), lyrs = .shineBandAt(sa$o, sa$t))
        rb <- terra::rast(.shineFileAt(sb$o, sb$t), lyrs = .shineBandAt(sb$o, sb$t))
        if (!terra::compareGeom(ra, rb, stopOnError = FALSE)) ra <- terra::resample(ra, rb)
        info <- .shineMakeCog(rb - ra, paste0(.san(sa$label), "__", .san(sb$label), "_t0.tif"), maxCells = maxCells,
                              transparent = 0)  # no change: transparent
        m <- .shineAddDiff(m, info, "B - A", cogUrl(info$file), opacity)
        custDL(list(path = file.path(.shineCogDir(), info$file),
                    name = paste0(.san(sb$label), "_minus_", .san(sa$label), ".tif")))
      }
    })
    output$cust_dl <- .shineDownload(custDL)
  }
}
