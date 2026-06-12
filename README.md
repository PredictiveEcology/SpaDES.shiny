# SpaDES.shiny

<!-- badges: start -->
[![R-CMD-check](https://github.com/PredictiveEcology/SpaDES.shiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PredictiveEcology/SpaDES.shiny/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Shiny apps for visualizing [`SpaDES`](https://spades.predictiveecology.org) simulation
outputs.

> **Note:** This package was previously deprecated and has been restarted from scratch.
> The current focus is `shine()`, an interactive outputs viewer.

## Installation

```r
remotes::install_github("PredictiveEcology/SpaDES.shiny")
```

## `shine()`

`shine()` recursively scans a folder for raster (`.tif`) and image (`.png`) outputs, groups
files that share a base name but differ by an embedded timestamp into time-series "objects",
and launches a Shiny app to explore them:

- **Maps** – `.tif` rasters drawn on a chooseable web basemap, rendered with the tiled
  [`leafem::addGeotiff()`](https://r-spatial.github.io/leafem/) renderer for fast pan/zoom at
  native resolution. Toggle layers in the legend; a time slider animates time-series.
- **Figures** – `.png` images, one at a time, with a time slider for time-series figures.
- **Differences** – per-object `last - first` difference maps.
- **Custom differences** – pick any two map snapshots (`object @ year`) and view `B - A`.

```r
library(SpaDES.shiny)
shine("outputs")
```
