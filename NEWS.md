# SpaDES.shiny (development version)

* `shine(sim)`: the Output maps slider plays again. With both an Output and an Input maps tab, it never moved, so the first map stayed on its first year (often blank, e.g. a burn map with no fires yet).
* `shine()`: Custom differences picks A and B with two dropdowns each, a map and then one of its years, instead of a radio button for every map and year. With many maps the page had thousands of them; the LandRCBM outputs took 14 s to show the first map, now 3 s.

* `shine()`: a page left open no longer runs out of memory. Each time step kept the previous map in the browser for good (leafem's `addGeotiff()` adds two mouse handlers per call that hold the whole raster and are never removed), so a playing slider used about 10 MB more per step until the "can't get the memory" banner appeared after a few hours. `shine()` now draws rasters without those handlers; memory stays flat.

* `shine()`: years are found in more file names. A hash after the year (`year2000__1838902.tif`) put times of about two million on the map slider and in the Custom differences choices; now the year is the time. Files such as `Tmax04_future_2031_t_25-26.tif`, one per year, were each a separate map; they are now one map with a slider. `yr2000` counts as a year, and the layers of one file named by year (`Tmax_sm_future_2011` ... `2100`) are one map with a slider.
* `shine(sim)`: maps listed in `outputs(sim)` are in an "Output maps" tab and all other maps in an "Input maps" tab, each with its own slider. A slider only moves while its tab is shown. With a folder path there is one "Maps" tab, as before.
* `shine()`: time-series figures no longer flash blank between steps on the first pass; every frame is loaded ahead and each step shows only once its image is ready.

* `shine()`: when the browser tab can no longer get the memory it needs to draw maps, the page says so in a red banner, and the R console prints the same message, instead of leaving every map blank. Reload the page, or if that does not help, close every tab of the server and open it again.
* `shine()`: a map with a single value (e.g. the first harvest year, every pixel cut once) is drawn in the colour its legend shows. It was drawn dark purple next to a teal legend entry.
* `shine()`: maps of whole numbers, such as how many times a pixel was harvested, now show only whole numbers, on the map and in the legend. They were reprojected and shrunk by averaging, which made values such as 3.2, and the legend placed its ticks between whole numbers. A map with a single value gets a single legend entry.
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
