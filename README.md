# SpaDES.shiny

[![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.shiny.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES.shiny)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/spades.shiny/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/SpaDES.shiny/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/SpaDES.shiny?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES.shiny)](https://cran.r-project.org/package=SpaDES.shiny)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES.shiny)](https://cran.r-project.org/package=SpaDES.shiny)

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

`Spades.shiny` provides you with a set of reusable shiny modules that allow you to speed up the development process.

In addition to these shiny modules `SpaDES.shiny` provides an app generator that allows you to bootstrap your application.
All you need to do is define structure of your app and call the `newApp()` function.
In the following sections you'll learn what does the application structure look like and we'll show you some examples of using `SpaDES.shiny`.

### Defining application structure

Following is the structure that `SpaDES.shiny` expects to be provided:

modules:

| type          | name          | id            | parameters                                    |
| ------------- | ------------- | ------------- | --------------------------------------------- |
| shinyModule   | slider        | mySlider1     | list("\\"Slider label\\"", 0, 100, 50, 1)     |
| shinyModule   | slider        | mySlider2     | list("\\"Slider label 2\\"", 0, 200, 100, 10) |
| shinyModule   | export        | myExport3     | list("data.frame(x = 1:5, y = 6:10)"))        |

layout:

| tabName        | menuItemName   | icon         | moduleId  |
| -------------- | -------------- | ------------ | --------- |
| sliderTabName  | Slider Module  | map          | mySlider1 |
| sliderTabName2 | Another Slider | sliders      | mySlider2 |
| export         | My Export      | file-image-o | myExport3 |

In `metadata$modules` you define modules that you want to be used in your app. Right now the only `type` is "shinyModule".
(TODO: After it is implemented add information about "customShinyModule" which creates a new empty module for user to implement in the app).
- `name` corresponds to the name of the module and the `id` assigns unique identifier for that module instance.
- `parameters` define the parameters that should be passed to the module.

Essentially based on the `metadata$modules` the generator on server side will build `callModule(<type>, "<id>", <parameters>)` directives.

In `metadata$layout` you define how the modules should be displayed.

- `tabName` does not matter to you that much - it just tells ui what the name of the tab should be.
- `menuItemName` defines what user sees in the menu.
- `icon` is displayed next to `menuItemName`.
- `moduleId` points to a module id from the list of modules defined in `metadata$modules`.

### Code example

You can either use `data.frame` or `tibble` to define app structure.
Below are two examples on how to create an app defined in the paragraph above.

#### Using `data.frame`

```r
devtools::install_github("PredictiveEcology/SpaDES.shiny", ref = "develop")
library(SpaDES.shiny)

appMetadata <- list(
  modules = data.frame(
    type = c("shinyModule", "shinyModule", "shinyModule"),
    name = c("slider", "slider", "export"),
    id = c("mySlider1", "mySlider2", "myExport1"),
    stringsAsFactors = FALSE
  ),
  layout = data.frame(
    tabName = c("sliderTabName1", "sliderTabName2", "myExport1"),
    menuItemName = c("Slider Module", "Another Slider", "My Export"),
    icon = c("map", "sliders", "file-image-o"),
    moduleId = c("mySlider1", "mySlider2", "myExport1"),
    stringsAsFactors = FALSE
  )
)

appMetadata$modules$parameters <- list(list(), list(), list("data.frame(x = 1:5, y = 6:10)"))
appMetadata$layout$moduleUIParameters <- list(
                                              list("\"Slider label\"", 0, 100, 50, 1),
                                              list("\"Slider label 2\"", 0, 200, 100, 10),
                                              list(c("csv", "txt", "xls", "rds")))

newApp(getwd(), appMetadata)

shiny::runApp(".")
```

#### Using `tibble`

```r
devtools::install_github("PredictiveEcology/SpaDES.shiny", ref = "develop")
library(SpaDES.shiny)
library(tibble)

appMetadata <- list(
  modules = tribble(
    ~type, ~name, ~id, ~parameters,
    "shinyModule", "slider", "mySlider1", list(),
    "shinyModule", "slider", "mySlider2", list(),
    "shinyModule", "export", "myExport1", list("data.frame(x = 1:5, y = 6:10)")
  ),
  layout = tribble(
    ~tabName, ~menuItemName, ~icon, ~moduleId, ~moduleUIParameters,
    "sliderTabName1", "Slider Module", "map", "mySlider1", list("\"Slider label\"", 0, 100, 50, 1),
    "sliderTabName2", "Another Slider", "sliders", "mySlider2", list("\"Slider label 2\"", 0, 200, 100, 10),
    "myExport1", "My Export", "file-image-o", "myExport1", list(c("csv", "txt", "xls", "rds"))
  )
)
newApp(getwd(), appMetadata)

shiny::runApp(".")
```
