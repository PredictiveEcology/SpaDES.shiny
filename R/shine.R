#' Interactive Shiny visualizer for SpaDES outputs
#'
#' Recursively scans a folder for raster (`.tif`) and image (`.png`) outputs,
#' groups files that share a base name but differ by an embedded timestamp into
#' time-series "objects", and launches a Shiny app to explore them. Maps (`.tif`)
#' are drawn on a chooseable web basemap; figures (`.png`) are shown as images.
#' Two further tabs show precomputed (`last - first`) and user-defined differences
#' between map snapshots.
#'
#' Rasters are rendered with [leafem::addGeotiff()], which draws them through the
#' tiled `georaster-layer-for-leaflet` canvas layer: pan/zoom redraw per-tile on
#' the client at native resolution (fast, no server round-trip per interaction).
#' Each raster is reprojected once to a web-mercator Cloud-Optimized GeoTIFF (with
#' overviews), cached on disk, and served same-origin via [shiny::addResourcePath()].
#'
#' Multi-band rasters contribute one object per band. Files whose name contains no
#' digit run are treated as static (always-shown) layers with no position on the
#' time slider.
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
#' @param ... Passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#'
#' @return Invisibly, the `shinyApp` object (also when `launch = TRUE`).
#' @examples
#' \dontrun{
#' shine("outputs")        # a path
#' shine(mySim)            # a simList -> uses outputPath(mySim)
#' shine()                 # uses getOption("spades.outputPath")
#' }
#' @export
shine <- function(x, timePattern = "[0-9]+", maxCells = NULL, interval = 2,
                  opacity = 1, launch = TRUE, ...) {
  for (p in c("shiny", "leaflet", "leafem", "terra")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' is required by shine(). Please install it.", call. = FALSE)
    }
  }
  path <- .shineResolvePath(if (missing(x)) NULL else x)
  if (!dir.exists(path)) stop("Folder not found: ", path, call. = FALSE)

  objects <- .shineScan(path, timePattern)
  if (length(objects) == 0L) stop("No .tif or .png files found under: ", path, call. = FALSE)

  app <- shiny::shinyApp(ui = .shineUI(objects, interval),
                         server = .shineServer(objects, path, maxCells, interval, opacity))
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
#               a single row with time = NA means a static layer
# The list is keyed by "<kind>\r<id>" so a map and a figure that share a name
# (e.g. foo.tif and foo.png) do not collide; use .shineById() to re-key by id
# within a single kind.
.shineScan <- function(path, timePattern = "[0-9]+") {
  files <- list.files(path, recursive = TRUE, full.names = TRUE,
                      pattern = "\\.(tif|png)$", ignore.case = TRUE)
  files <- files[!grepl("\\.aux\\.xml$", files, ignore.case = TRUE)]
  if (length(files) == 0L) return(list())
  root <- normalizePath(path, mustWork = FALSE)

  # Parse each file into (key, time, kind, folder)
  parsed <- lapply(files, function(f) {
    stem <- tools::file_path_sans_ext(basename(f))
    m <- gregexpr(timePattern, stem)[[1]]
    if (m[1] == -1L) {
      key <- stem
      time <- NA_real_
    } else {
      i <- length(m)                       # last match = the timestamp
      start <- m[i]; len <- attr(m, "match.length")[i]
      time <- as.numeric(substr(stem, start, start + len - 1L))
      key <- paste0(substr(stem, 1L, start - 1L),
                    substr(stem, start + len, nchar(stem)))
      key <- sub("(?i)[ _-]*year[ _-]*$", "", key, perl = TRUE)  # drop trailing "year"
      key <- gsub("(^[ _-]+)|([ _-]+$)", "", key)                # trim separators
    }
    kind <- if (grepl("\\.tif$", f, ignore.case = TRUE)) "map" else "figure"
    reldir <- sub(root, "", normalizePath(dirname(f), mustWork = FALSE), fixed = TRUE)
    folder <- gsub("\\\\", "/", sub("^[/\\\\]+", "", reldir))    # "" if file is in root
    list(key = key, time = time, file = f, kind = kind, folder = folder)
  })

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
    nb <- terra::nlyr(r)
    bnames <- names(r)
    isFac <- terra::is.factor(r)
    for (b in seq_len(nb)) {
      id <- if (nb > 1L) paste0(key, ": ", bnames[b]) else key
      cat_b <- isTRUE(isFac[b])
      objects[[paste("map", id, sep = "\r")]] <- list(
        id = id, kind = "map", band = b,
        categorical = cat_b,
        ordinal = cat_b && .shineIsOrdinal(.shineFactorLabels(r, b)),
        folder = folder, times = df
      )
    }
  }
  objects
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
.shineFileAt <- function(o, t) {
  df <- o$times
  if (all(is.na(df$time))) return(df$file[1])
  ok <- df[!is.na(df$time), , drop = FALSE]
  ok$file[which.min(abs(ok$time - t))]
}

# Flat list of (object @ time) snapshots for the custom-differences pickers. Each
# element is list(o, t, label). UI and server build this identically (same object
# order), so a snapshot's 1-based index is a stable, HTML-safe radio-button value.
.shineSnapshots <- function(contMaps) {
  snaps <- list()
  for (o in contMaps) {
    for (t in sort(o$times$time[!is.na(o$times$time)])) {
      snaps[[length(snaps) + 1L]] <- list(o = o, t = t, label = paste0(o$id, " @ ", t))
    }
  }
  snaps
}

# ---- COG cache (served same-origin for addGeotiff) ------------------------

.shineCogDir <- function() {
  d <- file.path(tempdir(), "shine_cog")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

.shineCogRange <- new.env(parent = emptyenv())   # cog basename -> c(lo, hi)
.san <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

# Write (once) a web-mercator COG named `key` from SpatRaster `r`; return
# list(file = key, range = c(lo, hi)). Range via terra::minmax (block-wise).
.shineMakeCog <- function(r, key, categorical = FALSE, maxCells = NULL) {
  out <- file.path(.shineCogDir(), key)
  if (!file.exists(out)) {
    if (!is.null(maxCells)) {
      fact <- floor(max(dim(r)[1:2]) / maxCells)
      if (fact > 1L) r <- terra::aggregate(r, fact = fact,
                          fun = if (categorical) "modal" else "mean", na.rm = TRUE)
    }
    r <- terra::project(r, "EPSG:3857", method = if (categorical) "near" else "bilinear")
    suppressWarnings(
      terra::writeRaster(r, out, filetype = "COG", gdal = c("COMPRESS=DEFLATE"),
                         overwrite = TRUE))
  }
  if (is.null(.shineCogRange[[key]])) {
    .shineCogRange[[key]] <- as.vector(terra::minmax(terra::rast(out), compute = TRUE))
  }
  list(file = key, range = .shineCogRange[[key]])
}

# COG for object `o`'s band at time `t` (cache key includes mtime so edits bust it).
.shineCogForObject <- function(o, t, maxCells = NULL) {
  f <- .shineFileAt(o, t)
  key <- paste0(.san(f), "_b", o$band, "_", as.integer(file.mtime(f)), ".tif")
  .shineMakeCog(terra::rast(f, lyrs = o$band), key, o$categorical, maxCells)
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
# reversed so the low end (negative) is red and the high end (positive) is blue
.diverging <- function() rev(grDevices::hcl.colors(64, "Blue-Red 3"))

# Stream COG `info` (from .shineMakeCog) onto a leaflet proxy/map at `url` with a
# matching legend. Re-adding the same `layerId` replaces that layer once the new
# tile has loaded (georaster swaps it in place). `replaceLegend` removes the prior
# legend first (proxy updates); set FALSE when drawing into a fresh map.
.shineAddRaster <- function(map, info, id, categorical, url, layerId,
                            replaceLegend = TRUE, opacity = 1) {
  rng <- info$range
  if (!all(is.finite(rng))) return(map)
  if (diff(rng) == 0) rng <- rng + c(-1, 1) * (abs(rng[1]) + 1) * 1e-6
  map <- leafem::addGeotiff(map, url = url, group = layerId, layerId = layerId,
    opacity = opacity, autozoom = FALSE, imagequery = FALSE,
    colorOptions = leafem::colorOptions(palette = .viridis(), domain = rng,
                                        na.color = "transparent"))
  if (categorical) {
    vals <- seq(floor(rng[1]), ceiling(rng[2]))
    pal <- leaflet::colorFactor(.viridis(), domain = vals, na.color = "transparent")
  } else {
    vals <- rng; pal <- leaflet::colorNumeric(.viridis(), domain = rng, na.color = "transparent")
  }
  lgd <- paste0("lgd_", .san(id))
  if (replaceLegend) map <- leaflet::removeControl(map, lgd)   # one legend per object
  leaflet::addLegend(map, position = "bottomleft", pal = pal, values = vals,
                     title = id, group = id, layerId = lgd)
}

# Stream a difference COG (diverging palette centered at 0) onto a leaflet proxy.
.shineAddDiff <- function(map, info, id, url, opacity = 1) {
  rng <- info$range
  lim <- max(abs(rng)); if (!is.finite(lim) || lim == 0) lim <- 1
  dom <- c(-lim, lim)
  map <- leafem::addGeotiff(map, url = url, group = id, layerId = .san(id),
    opacity = opacity, autozoom = FALSE, imagequery = FALSE,
    colorOptions = leafem::colorOptions(palette = .diverging(), domain = dom,
                                        na.color = "transparent"))
  pal <- leaflet::colorNumeric(.diverging(), domain = dom, na.color = "transparent")
  leaflet::addLegend(map, position = "bottomleft", pal = pal, values = dom,
                     title = id, group = id)
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

# ---- UI -------------------------------------------------------------------

.shineUI <- function(objects, interval = 2) {
  mapObjs <- .shineById(Filter(function(o) o$kind == "map", objects))
  figObjs <- .shineById(Filter(function(o) o$kind == "figure", objects))
  contMaps <- Filter(function(o) !o$categorical, mapObjs)
  # last - first works for continuous maps and ordinal factors (>= 2 timestamps)
  diffMaps <- Filter(function(o) (!o$categorical || isTRUE(o$ordinal)) &&
                       sum(!is.na(o$times$time)) >= 2L, mapObjs)

  mapTimes <- .shineTimes(mapObjs)

  # (object @ year) snapshot choices for custom differences; value = snapshot index
  snaps <- .shineSnapshots(contMaps)
  snapChoices <- as.character(seq_along(snaps))
  names(snapChoices) <- vapply(snaps, `[[`, character(1), "label")

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

  ui <- shiny::navbarPage(
    "shine", id = "tabs", collapsible = TRUE,
    header = shiny::tags$head(shiny::tags$style(shiny::HTML(paste(
      ".leaflet-container { background: #ddd; }",
      # lift the bottom-left color legend up so the time slider doesn't cover it
      ".leaflet-bottom.leaflet-left { margin-bottom: 72px; }")))),

    # ---- Maps tab ----
    shiny::tabPanel("Maps",
      shiny::div(style = "position: relative;",
        leaflet::leafletOutput("map_map", width = "100%", height = "92vh"),
        legendPanel(shiny::tagList(
          shiny::selectInput("map_basemap", "Basemap", choices = .basemaps),
          shiny::tags$hr(),
          shiny::radioButtons("map_objs", "Layer",
                              choices = vapply(mapObjs, `[[`, character(1), "id"),
                              selected = if (length(mapObjs)) mapObjs[[1]]$id else character(0)),
          if (length(mapTimes) >= 2L) shiny::tagList(shiny::tags$hr(), speedSlider("map_speed"))
        )),
        fileInfoPanel("map_fileinfo"),
        timeSliderUI("map", mapTimes)
      )
    ),

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
          shiny::fluidRow(
            shiny::column(6, shiny::radioButtons("cust_a", "A", choices = snapChoices,
              selected = if (length(snapChoices) >= 1) snapChoices[[1]] else character(0))),
            shiny::column(6, shiny::radioButtons("cust_b", "B", choices = snapChoices,
              selected = if (length(snapChoices) >= 2) snapChoices[[2]] else character(0)))
          ),
          shiny::tags$hr(),
          shiny::downloadButton("cust_dl", "Download GeoTIFF")
        ))
      )
    )
  )
  # load the tiled raster renderer's JS at page start so proxy addGeotiff() works
  htmltools::attachDependencies(ui, .shineCogDeps(), append = TRUE)
}

# ---- Server ---------------------------------------------------------------

.shineServer <- function(objects, path, maxCells, interval = 2, opacity = 1) {
  mapObjs  <- .shineById(Filter(function(o) o$kind == "map", objects))
  figObjs  <- .shineById(Filter(function(o) o$kind == "figure", objects))
  contMaps <- Filter(function(o) !o$categorical, mapObjs)
  mapTimes <- .shineTimes(mapObjs)
  bounds <- .shineBounds(objects)

  # serve figure PNGs and the COG cache to the browser (same-origin)
  root <- normalizePath(path)
  shiny::addResourcePath("shineimg", root)
  shiny::addResourcePath("shinecog", .shineCogDir())
  figUrl <- function(f) {
    rel <- sub("^[/\\\\]", "", sub(root, "", normalizePath(f), fixed = TRUE))
    paste0("shineimg/", gsub("\\\\", "/", rel))
  }
  cogUrl <- function(file) paste0("shinecog/", file)

  function(input, output, session) {

    # --- auto-advancing time sliders (default playing, only when a layer is on) ---
    playing <- list(map = shiny::reactiveVal(TRUE))
    advancer <- function(tab, times, selId) {
      if (length(times) < 2L) return()
      shiny::observeEvent(input[[paste0(tab, "_play")]], {
        playing[[tab]](!playing[[tab]]())
        shiny::updateActionButton(session, paste0(tab, "_play"),
                                  label = if (playing[[tab]]()) "Pause" else "Play")
      })
      shiny::observe({
        if (!isTRUE(playing[[tab]]())) return()
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
    advancer("map", mapTimes, "map_objs")

    # --- Maps tab ---
    # Each object animates by re-adding a layerId, which makes georaster replace
    # that layer in place (LayerManager.addLayer removes the old same-id layer).
    # Opaque layers (opacity >= 1) double-buffer with two alternating ids so the
    # previous frame stays visible until the new one paints (seamless, and the
    # opaque new frame fully covers the old -> no ghosting). Semi-transparent
    # layers use one id (a brief redraw per step, but stacking two would ghost).
    # Adding/removing layers or changing basemap rebuilds the map (the reliable
    # way to drop a georaster layer), preserving the current view.
    mapView <- shiny::reactiveValues(center = NULL, zoom = NULL)
    shiny::observeEvent(input$map_map_center, mapView$center <- input$map_map_center)
    shiny::observeEvent(input$map_map_zoom,   mapView$zoom   <- input$map_map_zoom)
    buf <- new.env(parent = emptyenv())   # object id -> current buffer letter (opaque only)
    animLayerId <- function(id, init = FALSE) {
      if (opacity < 1) return(.san(id))            # single layer
      if (init) { buf[[id]] <- "A"; return(paste0(.san(id), "_A")) }
      nxt <- if (identical(buf[[id]], "A")) "B" else "A"
      buf[[id]] <- nxt
      paste0(.san(id), "_", nxt)
    }
    curMapTime <- function() if (length(mapTimes)) {
      v <- input$map_time; if (is.null(v)) mapTimes[1] else v
    } else NA_real_

    output$map_map <- leaflet::renderLeaflet({
      sel <- input$map_objs
      bm <- if (is.null(input$map_basemap)) .basemaps[[1]] else input$map_basemap
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
      for (o in mapObjs[sel]) {
        if (is.null(o)) next
        info <- .shineCogForObject(o, t, maxCells)
        m <- .shineAddRaster(m, info, o$id, o$categorical, cogUrl(info$file),
                             animLayerId(o$id, init = TRUE), replaceLegend = FALSE,
                             opacity = opacity)
      }
      m
    })

    shiny::observeEvent(input$map_time, {                        # animate selected layers
      sel <- input$map_objs
      if (length(sel) == 0L) return()
      t <- input$map_time
      m <- leaflet::leafletProxy("map_map")
      for (o in mapObjs[sel]) {
        if (is.null(o)) next
        info <- .shineCogForObject(o, t, maxCells)
        m <- .shineAddRaster(m, info, o$id, o$categorical, cogUrl(info$file),
                             animLayerId(o$id), replaceLegend = TRUE, opacity = opacity)
      }
    })
    output$map_fileinfo <- shiny::renderUI({               # source file(s) on display
      sel <- input$map_objs
      if (length(sel) == 0L) return(NULL)
      t <- curMapTime()
      shiny::HTML(paste(vapply(mapObjs[sel], function(o) basename(.shineFileAt(o, t)),
                               character(1)), collapse = "<br>"))
    })

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
    output$fig_ui <- shiny::renderUI({
      id <- input$fig_objs
      if (is.null(id) || !nzchar(id)) return(shiny::helpText("Select a figure."))
      o <- figObjs[[id]]
      times <- figSelTimes()
      t <- if (length(times) >= 2L) { v <- input$fig_time; if (is.null(v)) times[1] else v } else NA_real_
      # bound by both width and height so tall figures fit fully (no cut-off bottom)
      shiny::tags$img(src = figUrl(.shineFileAt(o, t)),
                      style = paste("max-width: 98%; max-height: 90vh;",
                                    "width: auto; height: auto; margin: 8px auto; display: block;"))
    })
    output$fig_fileinfo <- shiny::renderUI({
      id <- input$fig_objs
      if (is.null(id) || !nzchar(id)) return(NULL)
      times <- figSelTimes()
      t <- if (length(times) >= 2L) { v <- input$fig_time; if (is.null(v)) times[1] else v } else NA_real_
      shiny::HTML(basename(.shineFileAt(figObjs[[id]], t)))
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
    shiny::observe({
      if (!identical(input$tabs, "Change from start to end")) return()  # only when map exists
      sel <- input$diff_objs
      m <- leaflet::clearControls(leaflet::clearImages(leaflet::leafletProxy("diff_map")))
      diffDL(NULL)
      for (id in sel) {
        o <- mapObjs[[id]]
        if (is.null(o)) next
        tt <- sort(o$times$time[!is.na(o$times$time)])
        if (length(tt) < 2L) next
        rLast  <- terra::rast(.shineFileAt(o, max(tt)), lyrs = o$band)
        rFirst <- terra::rast(.shineFileAt(o, min(tt)), lyrs = o$band)
        if (isTRUE(o$ordinal)) {                       # difference the level index
          levels(rLast) <- NULL; levels(rFirst) <- NULL   # terra S4 method, gives codes
        }
        r <- rLast - rFirst
        info <- .shineMakeCog(r, paste0(.san(id), "_diff_", min(tt), "_", max(tt), ".tif"),
                              maxCells = maxCells)
        m <- .shineAddDiff(m, info, paste0(id, " (last-first)"), cogUrl(info$file), opacity)
        diffDL(list(path = file.path(.shineCogDir(), info$file),
                    name = paste0(.san(id), "_change_", min(tt), "-", max(tt), ".tif")))
      }
    })
    output$diff_dl <- .shineDownload(diffDL)

    # --- Custom differences (B - A) ---
    snaps <- .shineSnapshots(contMaps)            # same order/indices as the UI radios
    parseSnap <- function(val) {
      if (length(val) != 1L) return(NULL)
      i <- suppressWarnings(as.integer(val))
      if (is.na(i) || i < 1L || i > length(snaps)) return(NULL)
      snaps[[i]]
    }
    custDL <- shiny::reactiveVal(NULL)            # list(path, name) of the on-the-fly COG
    output$cust_map <- leaflet::renderLeaflet(.shineBaseMap(bounds))
    shiny::observeEvent(input$cust_basemap, {
      leaflet::leafletProxy("cust_map") |>
        leaflet::clearTiles() |> leaflet::addProviderTiles(input$cust_basemap)
    })
    shiny::observe({
      if (!identical(input$tabs, "Custom differences")) return()   # only when map exists
      a <- input$cust_a; b <- input$cust_b
      m <- leaflet::clearControls(leaflet::clearImages(leaflet::leafletProxy("cust_map")))
      sa <- parseSnap(a); sb <- parseSnap(b)
      custDL(NULL)
      if (!is.null(sa) && !is.null(sb)) {
        ra <- terra::rast(.shineFileAt(sa$o, sa$t), lyrs = sa$o$band)
        rb <- terra::rast(.shineFileAt(sb$o, sb$t), lyrs = sb$o$band)
        if (!terra::compareGeom(ra, rb, stopOnError = FALSE)) ra <- terra::resample(ra, rb)
        info <- .shineMakeCog(rb - ra, paste0(.san(a), "__", .san(b), ".tif"), maxCells = maxCells)
        m <- .shineAddDiff(m, info, "B - A", cogUrl(info$file), opacity)
        custDL(list(path = file.path(.shineCogDir(), info$file),
                    name = paste0(.san(sb$label), "_minus_", .san(sa$label), ".tif")))
      }
    })
    output$cust_dl <- .shineDownload(custDL)
  }
}
