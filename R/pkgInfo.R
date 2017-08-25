#' Get installed package dependencies
#'
#' This is simply a wrapper around \code{tools::package_dependencies}
#'
#' @param x     An installed package name.
#' @param type  One of "Depends", "Imports", "LinkingTo", "Suggests", "Enhances",
#'              or "all". Default is \code{NULL}, which returns all.
#'
#' @return A character vector of package dependencies.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom magrittr %>%
#' @importFrom tools package_dependencies
#' @importFrom utils installed.packages
#' @rdname pkgInfoHelpers
pkgDeps <- function(x, type = NULL) {
  if (is.null(type)) type <- "all"
  package_dependencies(x, installed.packages(), which = type) %>%
    unlist() %>%
    unname()
}

#' Get version from a package's \file{DESCRIPTION}
#'
#' This is simply a wrapper around \code{utils::packageVersion} to 1) allow a
#' vector of package names to be passed as an argument; 2) to warn instead of error
#' when a package is not installed; and 3) return versions as character strings.
#'
#' @param lib.loc  a character vector of directory names of R libraries, or NULL.
#'        The default value of NULL corresponds to all libraries currently known.
#'        If the default is used, the loaded packages and namespaces are searched
#'        before the libraries.
#'
#' @return A character vector of package dependencies.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom magrittr %>%
#' @importFrom utils packageVersion
#' @rdname pkgInfoHelpers
pkgVers <- function(x, lib.loc = NULL) { # nolint
  sapply(x, function(y) {
    tryCatch(packageVersion(y, lib.loc), error = function(e) warning(e)) %>%
      as.character()
  }) %>% unlist()
}

#' Get authors from a package's DESCRIPTION
#'
#' @return A person vector of package authors.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom magrittr %>%
#' @importFrom stringr str_trim
#' @importFrom utils as.person as.personList packageDescription
#' @rdname pkgInfoHelpers
pkgAuthors <- function(x) {
  gsub("\n ", "", packageDescription(x)$Author) %>%
    gsub("], ", "]; ", .) %>%
    str_trim() %>%
    gsub("\\s+", " ", .) %>%
    strsplit(., "; ") %>%
    unlist() %>%
    gsub(", ", ",", .) %>% # hack to allow commas in names
    lapply(., as.person) %>%
    as.personList()
}

#' Generate Package Dependency Version Information
#'
#' @details Generates a box containing a data table with \pkg{SpaDES.shiny} package
#' dependency version information (i.e., all the packages required or used by \pkg{SpaDES.shiny}.)
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#'
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname pkgInformation
pkgInformationUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("pkgInfo"))
}

#' @param input    shiny server input object
#' @param output   shiny server output object
#' @param session  shiny server session object
#' @param config   \pkg{SpaDES.shiny} app configuration object (e.g., from \code{\link{readConfig}})
#'
#' @export
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny renderUI
#' @importFrom shinydashboard box
#' @importFrom utils installed.packages
#' @rdname pkgInformation
pkgInformation <- function(input, output, session, config) {
  output$pkgInfo <- renderUI({
    ns <- session$ns

    pkgs <- c("inSpaDES", pkgDeps("inSpaDES"))
    vers <- pkgVers(pkgs)
    libs <- installed.packages()[, "Package"] %in% pkgs %>%
      installed.packages()[., "LibPath"]

    box(
      title = "Package Version Information", status = "primary", width = 12,
      solidHeader = TRUE, collapsible = FALSE,
      output$appInfoPkgs <- DT::renderDataTable(data.frame(
        Package = pkgs,
        Version = vers,
        LibPath = libs,
        stringsAsFactors = FALSE
      ), rownames = FALSE),
      DT::dataTableOutput(ns("appInfoPkgs"))
    )
  })
}
