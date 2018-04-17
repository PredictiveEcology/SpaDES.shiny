#' Generate Authors and Copyright Information
#'
#' @details Generates a fluidRow containing two boxes
#' about the app's Authors and Contributors and the Copyright and License.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @author Alex Chubaty & Alex Tso (module)
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname copyrightAuthors
copyrightAuthorsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("authors"))
}

#' @param input          shiny server input object
#' @param output         shiny server output object
#' @param session        shiny server session object
#' @param appName        Character string giving the name of your SpaDES.shiny app.
#' @param authorInfo     Character string or person list containing your app's
#'                       authorship info.
#' @param copyrightInfo  Character string containing your app's copyright info.
#' @param licenseFile    Filepath to your app's LICENSE file
#'                       (default is \file{LICENSE} in your shiny app dir).
#' @param status         The boxes' status, passed to \code{\link[shinydashboard]{box}}
#'                       (default \code{"primary"}, which results in dark blue).
#'
#' @export
#' @importFrom shiny a br fluidRow h4 hr HTML p renderUI strong tags verbatimTextOutput wellPanel
#' @importFrom shinydashboard box
#' @rdname copyrightAuthors
copyrightAuthors <- function(input, output, session, appName, authorInfo,
                             copyrightInfo, fundingInfo, licenseFile = "LICENSE", status = "primary") {
  output$authors <- renderUI({
    if (is.null(appName)) appName <- ""
    if (is.na(appName)) appName <- ""

    if (is.null(authorInfo)) authorInfo <- ""
    authorInfo[is.na(authorInfo)] <- ""

    if (is.null(copyrightInfo)) copyrightInfo <- ""
    if (is.na(copyrightInfo)) copyrightInfo <- ""

    if (is.null(licenseFile)) licenseFile <- ""
    if (is.na(licenseFile)) licenseFile <- ""

    if (inherits(authorInfo, "person")) {
      authorInfo <- tags$ul(lapply(authorInfo %>% as.character(), function(x) {
        suppressWarnings(tags$li(HTML(x)))
      }))
    }

    fluidRow(
      box(
        title = "Authors and Contributors", status = status,
        solidHeader = TRUE, collapsible = TRUE, width = 12,
        ### app's authorship information
        if (all(nzchar(authorInfo))) h4(appName),
        if (all(nzchar(authorInfo))) p(strong("App authors: ")), # TODO: add app vers?
        if (all(nzchar(authorInfo))) authorInfo,
        if (all(nzchar(authorInfo))) hr(),

        ### SpaDES.shiny package authorship info
        h4("SpaDES.shiny"),
        p("An R package providing a shiny interface for SpaDES"),
        p(strong("Package version: "), packageVersion("SpaDES.shiny") %>% as.character()),
        p(strong("Package authors: ")),
        tags$ul(lapply(pkgAuthors("SpaDES.shiny") %>% as.character(), function(x) {
          suppressWarnings(tags$li(HTML(x)))
        })),
        hr(),

        ### SpaDES package authorship info
        h4("SpaDES: Spatial Discrete Event Simulation"),
        p(paste0("An R package to easily implement a variety of simulation models, ",
                 "with a focus on spatially explicit models.")),
        p(strong("Package version: "), packageVersion("SpaDES") %>% as.character()),
        p(strong("Package authors: ")),
        tags$ul(lapply(pkgAuthors("SpaDES") %>% as.character(), function(x) {
          suppressWarnings(tags$li(HTML(x)))
        })),
        p("Website: ", a("http://SpaDES.PredictiveEcology.org",
                         href = "http://SpaDES.PredictiveEcology.org",
                         target = "_blank"),
          br(),
          "GitHub: ", a("https://github.com/PredictiveEcology/SpaDES",
                        href = "https://github.com/PredictiveEcology/SpaDES",
                        target = "_blank")
        )
      ),

      box(
        title = "Copyright and License", status = status, width = 12,
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        p(HTML(paste(copyrightInfo))),
        if (file.exists(licenseFile)) {
          wellPanel(verbatimTextOutput(
            readLines(licenseFile) %>% paste(., collapse = "\n")
          ))
        }
      )
    )
  })
}
