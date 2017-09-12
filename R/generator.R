uiTemplatePath <- system.file(package = "SpaDES.shiny", "templates/ui.template")
serverTemplatePath <- system.file(package = "SpaDES.shiny", "templates/server.template")

#' Render a template using \pkg{whisker} package and write the result to a file.
#'
#' @param templatePath     Path to the template file.
#' @param data             Named list or environment with variables that will be used during rendering.
#' @param path             Path where the rendered file should be saved.
#'
#' @return None. Invoked for the side-effect of writing generated template to file.
#'
#' @author Damian Rodziewicz
#' @importFrom whisker whisker.render
renderTemplate <- function(templatePath, data, path) {
  template <- readLines(templatePath)
  renderedContent <- whisker.render(template, data)
  writeLines(renderedContent, path)
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
  uiPath <- paste0(appDir, "/", "ui.R")
  renderTemplate(uiTemplatePath, appMetadata, uiPath)
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
  serverPath <- paste0(appDir, "/", "server.R")
  renderTemplate(serverTemplatePath, appMetadata, serverPath)
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
