#' Generate Authors and Copyright Information
#'
#' @details Generates a \code{fluidRow} containing two boxes
#' about the app's Authors and Contributors and the Copyright and License.
#'
#' @template id
#'
#' @author Alex Chubaty & Alex Tso (module)
#' @export
#' @importFrom shiny NS uiOutput
#' @rdname copyrightAuthors
copyrightAuthorsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("authors"))
}

#' @template input
#' @template output
#' @template session
#' @param appName        Character string giving the name of your \code{SpaDES.shiny} app.
#' @param authorInfo     Character string or person list containing your app's authorship info.
#' @param copyrightInfo  Character string containing your app's copyright info.
#' @param fundingInfo    Character string containing your app's funding/partner info.
#' @param licenseFile    Path to your app's \file{LICENSE} file (default is \file{LICENSE} in
#'                       the shiny app dir).
#' @param status         The boxes' status, passed to \code{\link[shinydashboard]{box}}
#'                       (default \code{"primary"}, which results in dark blue).
#'
#' @export
#' @importFrom shiny a br column fluidRow h4 hr HTML p renderUI strong tags
#' @importFrom shiny verbatimTextOutput wellPanel
#' @importFrom shinydashboard box
#' @rdname copyrightAuthors
copyrightAuthors <- function(input, output, session, appName, authorInfo,
                             copyrightInfo, fundingInfo, licenseFile = "LICENSE",
                             status = "primary") {
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
        column(
          width = 6,
          h4("SpaDES.shiny"),
          p("An R package providing a shiny interface for SpaDES"),
          p(strong("Package version: "), packageVersion("SpaDES.shiny") %>% as.character()),
          p(strong("Package authors: ")),
          tags$ul(lapply(pkgAuthors("SpaDES.shiny") %>% as.character(), function(x) {
            suppressWarnings(tags$li(HTML(x)))
          }))
        ),

        ### SpaDES package authorship info
        column(
          width = 6,
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
