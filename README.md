# SpaDES.shiny

Utilities for building `shiny`-based apps for `SpaDES` simulations.

**Website:** [http://SpaDES.PredictiveEcology.org](http://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

Building packages from source requires the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)).

### Current stable release

[![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.shiny.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES.shiny)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/spades.shiny/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/SpaDES.shiny/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/SpaDES.shiny?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES.shiny)](https://cran.r-project.org/package=SpaDES.shiny)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES.shiny)](https://cran.r-project.org/package=SpaDES.shiny)

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

[![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.shiny.svg?branch=development)](https://travis-ci.org/PredictiveEcology/SpaDES.shiny)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/development?svg=true)](https://ci.appveyor.com/project/achubaty/spades.shiny/branch/development)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/SpaDES.shiny/badge.svg?branch=development)](https://coveralls.io/github/PredictiveEcology/SpaDES.shiny?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.shiny", ref = "development", dependencies = TRUE) # unstable
```
