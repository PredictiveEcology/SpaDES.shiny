# SpaDES.shiny (development version)

* `shine()`: a single file whose name has digits but no "year" (e.g. a pixel count) is a static layer, not a time; the time slider spans only real time series.
* `shine()`: `transparent` now defaults to `0`, so "none" (never harvested, never burned) shows the basemap. Use `transparent = NULL` to draw 0 values.
* `shine()`: the legend of a categorical map shows its category names instead of their numeric codes.
* `shine()` gains `transparent`, values drawn transparent like NoData (e.g. `0` in a count map), and `exclude`, regular expressions for files not to show.
* `shine()`: in the two difference tabs, "no change" (0) is transparent instead of white.

# SpaDES.shiny 0.1.0

* Package restarted from scratch (the previous deprecated contents were removed).
* New `shine()`: an interactive Shiny viewer for `SpaDES` outputs, with Maps, Figures,
  Differences, and Custom-differences tabs. Rasters are reprojected to web-mercator
  Cloud-Optimized GeoTIFFs and rendered tiled via `leafem::addGeotiff()`.
