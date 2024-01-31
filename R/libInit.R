#' Initialize a \pkg{SpaDES.shiny} app's \pkg{packrat} library
#'
#' Spawns a new `Rscript` process to setup an app's package library using
#' \pkg{packrat}. It initializes the `packrat` library inside the application
#' directory, and proceeds to install all \pkg{SpaDES.shiny} package dependencies,
#' plus \pkg{devtools}.
#' **WARNING: this may take a while to complete!**
#'
#' @note This spawns and runs in another R process, during which time your
#' current R process will wait, and RStudio may appear to hang while it's running.
#'
#' @param APP_DIR   File path giving the application directory
#'
#' @author Alex Chubaty
#' @export
#' @importFrom R.utils isAbsolutePath
#' @rdname libInit
libInit <- function(APP_DIR) { # nolint
  APP_DIR <- if (!isAbsolutePath(APP_DIR)) { # nolint
    file.path("/srv/shiny-server", APP_DIR)
  } else {
    APP_DIR
  }
  stopifnot(dir.exists(APP_DIR))

  message("Initializing package library and installing packages.\n",
          "This could take a while depending on your internet connection speed.")
  response <- readline("Do you want to proceed? [Y/n]: ")
  if (substr(tolower(response), 1, 1) != "n") {
    result <- system2(
      "Rscript",
      paste(file.path(system.file(package = "SpaDES.shiny"), "scripts/libInit.R"), APP_DIR),
      wait = TRUE
    )
    return(invisible(result))
  } else {
    warning("App package library initialization cancelled.\n",
            "Please delete the app directory and try again.")
    return(invisible(-1))
  }
}
