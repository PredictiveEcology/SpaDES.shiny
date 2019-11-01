[![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.shiny.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES.shiny)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/spades.shiny/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/SpaDES.shiny/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/SpaDES.shiny?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES.shiny)](https://cran.r-project.org/package=SpaDES.shiny)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES.shiny)](https://cran.r-project.org/package=SpaDES.shiny)



<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png">

# SpaDES.shiny

[![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.shiny.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES.shiny)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/spades-shiny/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/SpaDES.shiny/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/SpaDES.shiny?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES.shiny)](https://cran.r-project.org/package=SpaDES.shiny)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES.shiny)](https://cran.r-project.org/package=SpaDES.shiny)

Utilities for building `shiny`-based apps for `SpaDES` simulations.

## Installation

### Current stable release

**Install from CRAN:**

```r
#install.packages("SpaDES.shiny") ## not yet on CRAN. Install via GitHub per below.
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
