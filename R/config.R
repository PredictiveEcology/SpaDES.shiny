#' Read an app's yaml config file
#'
#' @param file Filepath to a yaml config file (\file{_config.yml})
#'
#' @return Invisibly return the config list.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom R.utils isAbsolutePath
#' @importFrom yaml yaml.load_file
#'
readConfig <- function(file) {
  stopifnot(file.exists(file))
  config <- yaml.load_file(file)

  # Rserve options # TODO add corresponding pieces to writeConfig
  if (is.null(config$Rserve)) config$Rserve <- FALSE # nolint
  if (is.null(config$RservePorts)) {
    config$RservePorts <- data.frame(user = "shiny", port = "6311", # nolint
                                     stringsAsFactors = FALSE)
  } else {
    config$RservePorts <- as.data.frame(config$RservePorts, # nolint
                                        stringsAsFactors = FALSE)
  }

  # SpaDES.shiny app directory
  if (!is.null(config$APP_DIR)) {
    if (!isAbsolutePath(config$APP_DIR)) {
      config$APP_DIR <- file.path("/srv/shiny-server", config$APP_DIR) # nolint
    }
    checkPath(config$APP_DIR) # do not create dir during read
  }

  # R library directory
  if (!is.null(config$RLIB_DIR)) {
    if (!isAbsolutePath(config$RLIB_DIR)) {
      config$RLIB_DIR <- file.path(config$APP_DIR, config$RLIB_DIR) # nolint
    }
    checkPath(config$RLIB_DIR) # do not create dir during read
  }

  # output directory
  if (!isAbsolutePath(config$OUTPUT_DIR)) {
    config$OUTPUT_DIR <- file.path(config$APP_DIR, # nolint
                                   paste0("output_", Sys.info()[["user"]])) # TODO revisit
  }
  tryCatch(checkPath(config$OUTPUT_DIR),
           error = function(err) err$message <- err$error)

  return(config)
}

#' Write yaml config file
#'
#' @param config    List of configuration variables to write to file
#' @param file      Path to yaml config file
#'
#' @export
#' @importFrom reproducible checkPath
#' @importFrom yaml as.yaml
#' @rdname configFile
writeConfig <- function(config, file) {
  cat(as.yaml(config), file = file)
  return(invisible())
}

#' Create a new app config file
#'
#' @param APP_DIR    The directory path to use for the new app.
#' @param ...        Additional arguments to populate app's \file{_config.yml} file.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom reproducible checkPath
#' @rdname newConfig
newConfig <- function(APP_DIR, ...) { # nolint
  userdefined <- list(...)

  ## read from template
  config <- readConfig(system.file(package = "SpaDES.shiny", "_config/_config.yml"))

  config$APP_DIR <- APP_DIR # nolint
  lapply(names(userdefined), function(x) {
    config[[x]] <<- userdefined[[x]]
  })

  d <- checkPath(file.path(config$APP_DIR, "_config"), create = TRUE)
  writeConfig(config, file.path(d, "_config.yml"))
  return(invisible(config))
}

#' Update yaml config file
#'
#' E.g., copy updated config from git repo to app config without clobbering
#'
#' @param fileFrom  Path to yaml config file to update from
#' @param fileTo    Path to yaml config file
#'
#' @author Alex Chubaty
#' @export
#' @rdname updateConfig
updateConfig <- function(fileFrom, fileTo) {
  # load from
  configApp <- readConfig(fileTo)
  configNew <- readConfig(fileFrom)

  ## do the updating magic

  # TODO

  # write to
  writeConfig(configNew, fileTo)

  return(invisible())
}

#' Create a new \pkg{SpaDES.shiny} app
#'
#' Setup the necessary directory structure for a new app, and create a new app
#' based on a template.
#'
#' The optional argument RLIB_DIR may be set to override the default of using a
#' \code{packrat} library managing the app's package dependencies.
#'
#' @inheritParams newConfig
#'
#' @return Invisibly return the config list.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom magrittr %>%
#' @importFrom R.utils isAbsolutePath
#' @importFrom reproducible checkPath
#' @importFrom utils install.packages
#' @rdname newAppDeprecated
newAppDeprecated <- function(APP_DIR, ...) { # nolint
  APP_DIR <- if (isAbsolutePath(APP_DIR)) { # nolint
    APP_DIR
  } else {
    file.path("/srv/shiny-server", APP_DIR)
  } %>%
    checkPath(create = TRUE)

  ## create a new config file in '_config/_config.yml'
  ## this will set the default RLIB_DIR if not user-specified
  config <- newConfig(APP_DIR, ...)

  ## create RLIB_DIR and install required packages
  if (is.null(config$RLIB_DIR)) {
    status <- libInit(APP_DIR)
    if (status != 0) stop("Unable to initialize packrat library for this app.\n",
                          "Please delete the app directory and try again.")
  } else {
    checkPath(config$RLIB_DIR, create = TRUE)
    message("Initializing package library and installing packages.\n",
            "This could take a while depending on your internet connection speed.")
    response <- readline("Do you want to proceed? [Y/n]: ")
    if (substr(tolower(response), 1, 1) != "n") {
      install.packages(pkgDeps("SpaDES.shiny"), config$RLIB_DIR) # TODO simplify once on CRAN
    } else {
      warning("App package library initialization cancelled.\n",
              "Please delete the app directory and try again.")
      return()
    }
  }

  ## create app directories and copy files
  appDirs <- c("app_data", "cache", "output_shiny") # TODO revisit APP_DIR structure
  pkgDirs <- c("_config", "modules", "www")

  lapply(appDirs, function(d) {
    file.path(APP_DIR, d) %>% checkPath(create = TRUE)
  })

  lapply(pkgDirs, function(d) {
    file.path(APP_DIR, d) %>% checkPath(create = TRUE)
    file.copy(system.file(package = "SpaDES.shiny", d), APP_DIR, recursive = TRUE)
  })

  files <- c("style.css", "global.R", "server.R", "ui.R")
  lapply(files, function(f) {
    file.copy(system.file(package = "SpaDES.shiny", f), file.path(APP_DIR, f))
  })

  message("New SpaDES.shiny app created!\n",
          "If running on shiny server, please ensure the app directory has the",
          " correct permissions set:\n",
          "  chmod -R shiny:shiny ", APP_DIR) # TODO revisit deployment

  return(invisible(config))
}

#' Clone an existing \pkg{SpaDES.shiny} app to a new directory
#'
#' Setup the necessary directory structure for a new app, if not already present.
#' Useful for copying an existing app for development or deployment purposes.
#'
#' This does not copy the app \file{cache/} nor \file{output/} directories.
#'
#' @param from      Directory of the app from which to copy/clone.
#' @param to        New app directory to be created.
#' @param symlinks  Logical indicating whether symlinks should be used for large
#'                  directories, including \file{app_data/}, \file{modules/},
#'                  and \file{packrat/}. Using symlinks speeds up app cloning.
#'
#' @return None. Invoked for the side-effect of copyin an existing app.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom reproducible checkPath
cloneApp <- function(from, to, symlinks = FALSE) {
  to <- checkPath(to, create = TRUE)

  ## create app directories and copy files
  # TODO revisit app dir structure
  toLink <- c("app_data", "modules")
  toCopy <- c("_config", "www") # handled separately
  toMake <- c("cache", "output_shiny")

  lapply(toMake, function(d) {
    file.path(to, d) %>% checkPath(create = TRUE)
  })

  lapply(toLink, function(d) {
    if (symlinks) {
      file.symlink(file.path(from, d), file.path(to, d))
    } else {
      file.path(to, d) %>% checkPath(create = TRUE)
      file.copy(file.path(from, d), to, recursive = TRUE)
    }
  })

  # toCopy directories handled separately
  file.path(to, "_config") %>% checkPath(create = TRUE)
  if (!file.exists(file.path(from, "_config", "_config.yml"))) {
    file.copy(system.file("_config/_config.yml", package = "SpaDES.shiny"), to, recursive = TRUE)
  } else {
    file.copy(file.path(from, "_config"), to, recursive = TRUE)
  }

  file.path(to, "www") %>% checkPath(create = TRUE)
  if (!dir.exists(file.path(from, "www"))) {
    file.copy(system.file("www", package = "SpaDES.shiny"), to, recursive = TRUE)
  } else {
    file.copy(file.path(from, "www"), to, recursive = TRUE)
  }

  files <- c("style.css", "global.R", "server.R", "ui.R")
  lapply(files, function(f) {
    file.copy(file.path(from, f), file.path(to, f))
  })

  # copy over packrat library if needed
  if (dir.exists(file.path(from, "packrat"))) {
    if (symlinks) {
      file.symlink(file.path(from, "packrat"), file.path(to, "packrat"))
    } else {
      # packrat generates spurious 'nested symlinks' warnings
      suppressWarnings(file.copy(file.path(from, "packrat"), to, recursive = TRUE))

      # temporary workaround for inSpaDES#173
      libR <- file.path(to, "packrat", "lib-R")
      unlink(list.files(libR, full.names = TRUE), recursive = TRUE)
    }
  }
  if (file.exists(file.path(from, ".Rprofile"))) {
    file.copy(file.path(from, ".Rprofile"), file.path(to, ".Rprofile"))
  }

  message("Cloned SpaDES.shiny app created!\n",
          "If running on shiny server, please ensure the app directory has the",
          " correct permissions set:\n",
          "  chmod -R shiny:shiny ", to) # TODO revisit deployment steps
}
