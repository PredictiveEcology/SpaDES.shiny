# SpaDES.shiny (development version)

* `shine()`: the legend of a categorical map shows its category names instead of their numeric codes.

# SpaDES.shiny 0.1.0

* Package restarted from scratch (the previous deprecated contents were removed).
* New `shine()`: an interactive Shiny viewer for `SpaDES` outputs, with Maps, Figures,
  Differences, and Custom-differences tabs. Rasters are reprojected to web-mercator
  Cloud-Optimized GeoTIFFs and rendered tiled via `leafem::addGeotiff()`.
