.onAttach <- function(libname, pkgname) {
  ## set internal paths to template files
  .pkgEnv$globalTemplatePath <- system.file(package = "SpaDES.shiny", "templates/global.R.template")
  .pkgEnv$menuItemTemplatePath <- system.file(package = "SpaDES.shiny", "templates/menuItem.template")
  .pkgEnv$serverTemplatePath <- system.file(package = "SpaDES.shiny", "templates/server.R.template")
  .pkgEnv$tabItemTemplatePath <- system.file(package = "SpaDES.shiny", "templates/tabItem.template")
  .pkgEnv$uiTemplatePath <- system.file(package = "SpaDES.shiny", "templates/ui.R.template")

  return(invisible(NULL))
}
