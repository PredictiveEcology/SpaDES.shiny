#' Initialize a \pkg{packrat} repository for a \code{SpaDES.shiny} app
#'
#' @author Alex Chubaty
#'
args <- commandArgs(TRUE)
local({
  stopifnot(length(args) == 1)
  APP_DIR <- args[1] # nolint
  packrat::init(APP_DIR, restart = FALSE)
  utils::install.packages("devtools", dependencies = TRUE)
  devtools::install_github("s-u/fastshp")
  devtools::install_github("SpaDES.shiny@development") # TODO update once working in master branch

  packrat::snapshot(APP_DIR)

  result <- if (all(deps %in% unname(utils::installed.packages()[, "Package"]))) {
    0 # success
  } else {
    1 # error
  }
  return(invisible(result))
})
