if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("id"))
}

uiTemplatePath <- system.file(package = "SpaDES.shiny", "templates/ui.R.template")
serverTemplatePath <- system.file(package = "SpaDES.shiny", "templates/server.R.template")
globalTemplatePath <- system.file(package = "SpaDES.shiny", "templates/global.R.template")
tabItemTemplatePath <- system.file(package = "SpaDES.shiny", "templates/tabItem.template")
menuItemTemplatePath <- system.file(package = "SpaDES.shiny", "templates/menuItem.template")

#' Render a template using \pkg{whisker} package.
#'
#' @param templatePath     Path to the template file.
#' @param data             Named list or environment with variables that will be used during rendering.
#'
#' @return Rendered template.
#'
#' @importFrom whisker whisker.render
#'
#' @author Damian Rodziewicz
renderTemplate <- function(templatePath, data) {
  template <- readLines(templatePath)
  whisker.render(template, data)
}

#' Retrieve a module metadata from modules tibble.
#'
#' @param modules   Tibble with modules metadata. Tibble format: type, name, id, parameters.
#' @param moduleId  Id of the module to retrieve.
#'
#' @return Tibble containing the module if it was found. Empty tibble otherwise.
#'
#' @author Damian Rodziewicz
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom utils head
#'
getModuleById <- function(modules, moduleId) {
  module <- modules %>%
    dplyr::filter(id == moduleId) %>%
    head()

  return(module)
}

#' Render parameters
#'
#' DESCRIPTION NEEDED (TODO)
#'
#' @param parameters description needed (TODO)
#'
renderParameters <- function(parameters) {
  renderedParameters <- if (length(parameters) > 0) {
    paste(",", paste(parameters, collapse = ", "))
  } else {
    ""
  }
}

#' Render a tab item.
#'
#' @param tabName            Name of the tab that this item corresponds to.
#' @param module             Tibble with module metadata. Tibble format: type, name, id, parameters.
#' @param moduleUIParameters Module UI parameters.
#'
#' @return Rendered tab item.
#'
#' @author Damian Rodziewicz
renderTabItem <- function(tabName, module, moduleUIParameters) {
  # TODO: Separate id for each module so that user can have two modules A with different ids.
  parameters <- renderParameters(moduleUIParameters)
  tabContent <- paste0(module$name, "UI(\"", module$id, "\"", parameters, ")")
  tabItem <- renderTemplate(tabItemTemplatePath, list(tabName = tabName, tabContent = tabContent))

  return(tabItem)
}

#' Render tab items for provided layout and available modules.
#'
#' @param layout         Tibble with layout metadata. Tibble format: tabName, menuItemName, icon, moduleName.
#' @param modules        Tibble with modules metadata. Tibble format: type, name, id, parameters.
#'
#' @return Rendered tab items.
#'
#' @importFrom purrr pmap
#'
#' @author Damian Rodziewicz
renderTabItems <- function(layout, modules) {
  tabItems <- purrr::pmap(list(layout$tabName, layout$moduleId, layout$moduleUIParameters), function(tabName, moduleId, moduleUIParameters) {
    module <- getModuleById(modules, moduleId)
    renderTabItem(tabName, module, moduleUIParameters)
  })

  return(paste0(tabItems, collapse = ",\n"))
}

#' Render a menu item.
#'
#' @param tabName          Name of the tab that this item corresponds to.
#' @param menuItemName     Name of the menu item to display.
#' @param icon             Icon to display next to menu item name.
#'
#' @return Rendered menu item.
#'
#' @author Damian Rodziewicz
renderMenuItem <- function(tabName, menuItemName, icon) {
  menuItem <- renderTemplate(menuItemTemplatePath, list(tabName = tabName, menuItemName = menuItemName, icon = icon))

  return(menuItem)
}

#' Render menu items for provided layout and available modules.
#'
#' @param layout         Tibble with layout metadata. Tibble format: tabName, menuItemName, icon, moduleName.
#' @param modules        Tibble with modules metadata. Tibble format: type, name, id, parameters.
#'
#' @return Rendered menu items.
#'
#' @importFrom purrr pmap
#'
#' @author Damian Rodziewicz
renderMenuItems <- function(layout, modules) {
  menuItems <- purrr::pmap(list(layout$tabName, layout$menuItemName, layout$icon), renderMenuItem)

  return(paste(menuItems, collapse = ",\n"))
}

#' Render and save ui.R file.
#'
#' @param appDir         The directory path to use for the new app.
#' @param appMetadata    Application metadata.
#'
#' @return None. Invoked for the side-effect of writing rendered template to file.
#'
#' @author Damian Rodziewicz
renderSpadesShinyUI <- function(appDir, appMetadata) {
  uiPath <- file.path(appDir, "ui.R")

  data <- list(
    menuItems = renderMenuItems(appMetadata$layout, appMetadata$modules),
    tabItems = renderTabItems(appMetadata$layout, appMetadata$modules)
  )

  renderedContent <- renderTemplate(uiTemplatePath, data)
  writeLines(renderedContent, uiPath)
}

#' Render a callModule directive.
#'
#' @param name             Module name.
#' @param id               Module id.
#' @param parameters       Server parameters used when calling the module.
#'
#' @return Rendered callModule directive.
#'
#' @author Damian Rodziewicz
renderCallModuleDirective <- function(name, id, parameters) {
  # TODO: Separate id for each module so that user can have two modules A with different ids.
  renderedParameters <- renderParameters(parameters)
  callModuleDirective <- paste0("callModule(", name, ", \"", id, "\"", renderedParameters, ")")

  return(callModuleDirective)
}

#' Render callModule directives for provided modules.
#'
#' @param modules        Tibble with modules metadata. Tibble format: type, name, id, parameters.
#'
#' @return Rendered callModule directives.
#'
#' @importFrom purrr pmap
#'
#' @author Damian Rodziewicz
renderCallModuleDirectives <- function(modules) {
  callModuleDirectives <- purrr::pmap(list(modules$name, modules$id, modules$parameters), renderCallModuleDirective)

  return(paste(callModuleDirectives, collapse = "\n"))
}

#' Render and save server.R file.
#'
#' @param appDir         The directory path to use for the new app.
#' @param appMetadata    Application metadata.
#'
#' @return None. Invoked for the side-effect of writing rendered template to file.
#'
#' @author Damian Rodziewicz
renderSpadesShinyServer <- function(appDir, appMetadata) {
  serverPath <- file.path(appDir, "server.R")
  modules <- appMetadata$modules

  callModuleDirectives <- renderCallModuleDirectives(appMetadata$modules)

  data <- list(
    callModuleDirectives = callModuleDirectives
  )

  renderedContent <- renderTemplate(serverTemplatePath, data)
  writeLines(renderedContent, serverPath)
}

#' Render and save global.R file.
#'
#' @param appDir         The directory path to use for the new app.
#' @param appMetadata    Application metadata.
#'
#' @return None. Invoked for the side-effect of writing rendered template to file.
#'
#' @author Damian Rodziewicz
renderSpadesShinyGlobal <- function(appDir, appMetadata) {
  globalPath <- file.path(appDir, "global.R")

  renderedContent <- renderTemplate(globalTemplatePath, appMetadata)
  writeLines(renderedContent, globalPath)
}

#' Use an existing shiny module.
#'
#' Creates a metadata object to use in application metadata.
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
#'
#' @export
#'
#' @importFrom R.utils isAbsolutePath
#' @importFrom reproducible checkPath
#'
#' @rdname newApp
newApp <- function(appDir, appMetadata) {
  appDir <- if (isAbsolutePath(appDir)) {
    appDir
  } else {
    file.path("/srv/shiny-server", appDir)
  }

  checkPath(appDir, create = TRUE)
  renderSpadesShinyUI(appDir, appMetadata)
  renderSpadesShinyServer(appDir, appMetadata)
  renderSpadesShinyGlobal(appDir, appMetadata)

  message("New SpaDES.shiny app created!\n",
          "If running on shiny server, please ensure the app directory has the",
          " correct permissions set:\n",
          "  chmod -R shiny:shiny ", appDir) # TODO revisit deployment

  return(invisible())
}
