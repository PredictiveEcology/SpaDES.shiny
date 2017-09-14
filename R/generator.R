uiTemplatePath <- system.file(package = "SpaDES.shiny", "templates/ui.R.template")
serverTemplatePath <- system.file(package = "SpaDES.shiny", "templates/server.R.template")
tabItemTemplatePath <- system.file(package = "SpaDES.shiny", "templates/tabItem.template")
menuItemTemplatePath <- system.file(package = "SpaDES.shiny", "templates/menuItem.template")

#' Render a template using \pkg{whisker} package.
#'
#' @param templatePath     Path to the template file.
#' @param data             Named list or environment with variables that will be used during rendering.
#'
#' @return Rendered template.
#'
#' @author Damian Rodziewicz
#' @importFrom whisker whisker.render
renderTemplate <- function(templatePath, data) {
  template <- readLines(templatePath)
  whisker.render(template, data)
}

renderTabItems <- function(appMetadata) {
  renderTemplate(tabItemTemplatePath, list(tabName = "moduleInfo", moduleUI = "module"))
}

renderMenuItems <- function(appMetadata) {
  renderTemplate(menuItemTemplatePath, list(menuItemName = "Module Info", tabName = "moduleInfo", icon = "puzzle-piece"))
}

#' Generate and save ui.R file.
#'
#' @param appDir         The directory path to use for the new app.
#' @param appMetadata    Application metadata.
#'
#' @return None. Invoked for the side-effect of writing generated template to file.
#'
#' @author Damian Rodziewicz
generateSpadesShinyUI <- function(appDir, appMetadata) {
  uiPath <- file.path(appDir, "ui.R")

  data <- list(
    tabItems = renderTabItems(appMetadata),
    menuItems = renderMenuItems(appMetadata)
  )

  renderedContent <- renderTemplate(uiTemplatePath, data)
  writeLines(renderedContent, uiPath)
}

#' Generate and save server.R file.
#'
#' @param appDir         The directory path to use for the new app.
#' @param appMetadata    Application metadata.
#'
#' @return None. Invoked for the side-effect of writing generated template to file.
#'
#' @author Damian Rodziewicz
generateSpadesShinyServer <- function(appDir, appMetadata) {
  serverPath <- file.path(appDir, "server.R")

  renderedContent <- renderTemplate(serverTemplatePath, appMetadata)
  writeLines(renderedContent, serverPath)
}

#' Use an existing shiny module.
#'
#' This method creates a metadata object to use in application metadata.
#' Created object describes an existing shiny module.
#'
#' @param moduleName     Name of the module to use.
#'
#' @return Metadata object that describes an existing shiny module.
#'
#' @author Damian Rodziewicz
shinyModule <- function(moduleName) {
  moduleName
}

#' Create a new \pkg{SpaDES.shiny} app
#'
#' Setup the necessary directory structure for a new app, and create a new app
#' based on a template.
#'
#' @param appDir         The directory path to use for the new app.
#' @param appMetadata    Application metadata.
#'
#' @return None. Invoked for the side-effect of creating a new app.
#'
#' @author Damian Rodziewicz
#' @export
#' @importFrom magrittr %>%
#' @importFrom R.utils isAbsolutePath
#' @importFrom reproducible checkPath
#' @rdname newApp
newApp <- function(appDir, appMetadata) { # nolint
  appDir <- if (isAbsolutePath(appDir)) { # nolint
    appDir
  } else {
    file.path("/srv/shiny-server", appDir)
  }

  checkPath(appDir, create = TRUE)
  generateSpadesShinyUI(appDir, appMetadata)
  generateSpadesShinyServer(appDir, appMetadata)

  message("New SpaDES.shiny app created!\n",
          "If running on shiny server, please ensure the app directory has the",
          " correct permissions set:\n",
          "  chmod -R shiny:shiny ", appDir) # TODO revisit deployment

  return(invisible())
}
