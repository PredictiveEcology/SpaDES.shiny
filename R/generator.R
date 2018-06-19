if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("id"))
}

globalTemplatePath <- system.file(package = "SpaDES.shiny", "templates/global.R.template")
menuItemTemplatePath <- system.file(package = "SpaDES.shiny", "templates/menuItem.template")
serverTemplatePath <- system.file(package = "SpaDES.shiny", "templates/server.R.template")
tabItemTemplatePath <- system.file(package = "SpaDES.shiny", "templates/tabItem.template")
uiTemplatePath <- system.file(package = "SpaDES.shiny", "templates/ui.R.template")

#' Render a template using \pkg{whisker} package.
#'
#' @param templatePath     Path to the template file.
#' @param data             Named list or environment with variables that will be
#'                         used during rendering.
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

#' Render the name of the app.
#'
#' @param title  Name of the app to be displayed in the top left (above the sidebar).
#'
#' @return Rendered app name.
#'
#' @author Alex Chubaty
renderTitle <- function(title) {
  name <- ifelse(is.null(title), "Template App", title)
  return(deparse(paste(name)))
}

#' Render the copyright holder name of the app.
#'
#' @param copyright  Name of the app copyright holder.
#'
#' @return Rendered app copyright holder name.
#'
#' @author Alex Chubaty
renderCopyright <- function(copyright) {
  cph <- ifelse(is.null(copyright), "The Author(s)", copyright)
  return(deparse(paste(cph)))
}

#' Render additional sidebar footer info.
#'
#' @param text  Custom text to insert into app sidebar footer.
#'
#' @return Rendered additional sidebar footer info.
#'
#' @author Alex Chubaty
renderSidebar <- function(text) {
  return(deparse(paste(text)))
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
  #ns <- NS(id)

  parameters <- renderParameters(moduleUIParameters)
  tabContent <- paste0(module$name, "UI(\"", module$id, "\"", parameters, ")")
  #tabContent <- paste0(module$name, "UI(\"", ns(module$id), "\"", parameters, ")")
  tabItem <- renderTemplate(tabItemTemplatePath, list(tabName = tabName, tabContent = tabContent))

  return(tabItem)
}

#' Render tab items for provided layout and available modules.
#'
#' @param layout    Tibble with layout metadata.
#'                  Tibble format: \code{tabName}, \code{menuItemName}, \code{icon},
#'                  \code{moduleName}.
#'
#' @param modules   Tibble with modules metadata.
#'                  Tibble format: \code{type}, \code{name}, \code{id}, \code{parameters}.
#'
#' @return Rendered tab items.
#'
#' @importFrom purrr pmap
#'
#' @author Damian Rodziewicz
renderTabItems <- function(layout, modules) {
  tabItems <- purrr::pmap(
    list(layout$tabName, layout$moduleId, layout$moduleUIParameters),
    function(tabName, moduleId, moduleUIParameters) {
      module <- getModuleById(modules, moduleId)
      renderTabItem(tabName, module, moduleUIParameters)
    }
  )

  return(paste0(tabItems, collapse = ",\n      "))
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
  menuItem <- renderTemplate(menuItemTemplatePath,
                             list(tabName = tabName,
                                  menuItemName = menuItemName, icon = icon))

  return(menuItem)
}

#' Render menu items for provided layout and available modules.
#'
#' @inheritParams renderTabItems
#'
#' @return Rendered menu items.
#'
#' @importFrom purrr pmap
#'
#' @author Damian Rodziewicz
renderMenuItems <- function(layout, modules) {
  menuItems <- purrr::pmap(list(layout$tabName, layout$menuItemName, layout$icon), renderMenuItem)

  return(paste(menuItems, collapse = ",\n      "))
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
    title = renderTitle(appMetadata$title),
    menuItems = renderMenuItems(appMetadata$layout, appMetadata$modules),
    tabItems = renderTabItems(appMetadata$layout, appMetadata$modules),
    sidebarWidth = ifelse(is.null(appMetadata$sidebar$width), 300, appMetadata$sidebar$width)
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
  #ns <- NS(id)

  renderedParameters <- renderParameters(parameters)
  callModuleDirective <- paste0("callModule(", name, ", \"", id, "\"", renderedParameters, ")")
  #callModuleDirective <- paste0("callModule(", name, ", \"", ns(id), "\"", renderedParameters, ")")

  return(callModuleDirective)
}

#' Render callModule directives for provided modules.
#'
#' @note \code{spades_simInit} and \code{spades_expt} modules are omitted,
#'       as they are rendered separately using \code{\link{renderSimInitDirective}}
#'       and \code{\link{renderExperimentDirectives}}, respectively.
#'
#' @param modules  Tibble with modules metadata. Tibble format: type, name, id, parameters.
#'
#' @return Rendered callModule directives.
#'
#' @author Damian Rodziewicz
#' @importFrom purrr pmap
renderCallModuleDirectives <- function(modules) {
  # omit simInit and experiment modules
  omit <- which(c("spades_simInit", "spades_expt") %in% modules$name)

  callModuleDirectives <- purrr::pmap(
    list(modules$name[-omit], modules$id[-omit], modules$parameters[-omit]),
    renderCallModuleDirective
  )

  return(paste(callModuleDirectives, collapse = "\n  "))
}

#' Render callModule directives for simInit module if provided.
#'
#' @param modules  Tibble with modules metadata. Tibble format: type, name, id, parameters.
#'
#' @return Rendered callModule directives.
#'
#' @author Alex Chubaty
#' @importFrom purrr pmap
renderSimInitDirective <- function(modules) {
  if ("spades_simInit" %in% modules$name) {
    id <- which(modules$name == "spades_simInit")
    directive <- purrr::pmap(
      list(modules$name[id], modules$id[id], modules$parameters[id]),
      renderCallModuleDirective
    )
    directive <- paste("mySim <-", directive)

    line1 <- "### simulation initialization"
    line2 <- paste(directive, "\n")

    return(paste(c(line1, line2), collapse = "\n  "))
  } else {
    return(character(0))
  }
}

#' Render callModule directives for experiment module if provided.
#'
#' Additonal customizations can be provided via two files: \file{pre_experiment.R}
#' and \file{post_experiment.R}.
#'
#' @param modules  Tibble with modules metadata. Tibble format: type, name, id, parameters.
#'
#' @return Rendered callModule directives.
#'
#' @author Alex Chubaty
#' @importFrom purrr pmap
renderExperimentDirectives <- function(modules) {
  if ("spades_expt" %in% modules$name) {
    id <- which(modules$name == "spades_expt")
    directive <- purrr::pmap(
      list(modules$name[id], modules$id[id], modules$parameters[id]),
      renderCallModuleDirective
    )
    directive <- paste("mySimOut <-", directive)

    line1 <- "## pre-experiment customizations"
    line2 <- "if (file.exists(\"pre_experiment.R\")) source(\"pre_experiment.R\", local = TRUE)\n  "
    line3 <- "## run the simulation experiment"
    line4 <- paste(directive, "\n  ")
    line5 <- "## post-experiment customizations"
    line6 <- "if (file.exists(\"post_experiment.R\")) source(\"post_experiment.R\", local = TRUE)\n"

    return(paste(c(line1, line2, line3, line4, line5, line6), collapse = "\n  "))
  } else {
    return(character(0))
  }
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

  data <- list(
    simInitDirectives = renderSimInitDirective(appMetadata$modules),
    experimentDirectives = renderExperimentDirectives(appMetadata$modules),
    ###
    callModuleDirectives = renderCallModuleDirectives(appMetadata$modules),
    copyright = renderCopyright(appMetadata$copyright),
    sidebarFooter = renderSidebar(appMetadata$sidebar$footer)
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
