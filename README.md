<!-- badges: start -->
[![R build status](https://github.com/PredictiveEcology/SpaDES.shiny/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/SpaDES.shiny/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES.shiny)](https://cran.r-project.org/package=SpaDES.shiny)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES.shiny)](https://cran.r-project.org/package=SpaDES.shiny)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.shiny/branch/master/graph/badge.svg)](https://codecov.io/gh/PredictiveEcology/SpaDES.shiny?branch=master)
<!-- badges: end -->

Utilities for building `shiny`-based apps for `SpaDES` simulations.

## Installation

### Current stable release

**Install from CRAN:**

```r
install.packages("SpaDES.shiny")
```

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.shiny", dependencies = TRUE) # stable
```

### Development version (unstable)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.shiny", ref = "development", dependencies = TRUE) # unstable
```

## Using the package

`SpaDES.shiny` provides you with a set of reusable shiny modules that allow you to speed up the development process, as well as an app generator that allows you to bootstrap your application.
See the accompanying vignettes to get started:

```r
browseVignettes("SpaDES.shiny")
```
