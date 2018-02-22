#' Authorize a user using Google Drive account
#'
#' In Google Drive, create a blank text file (e.g., name_of_your_app.txt) with
#' access control settings that allow only authorized users to download/view etc.
#'
#' Copy the (private) Drive file url and pass that as the \code{authFile} argument.
#'
#' Authentication status is stored in \code{session$userData$AUTH_GOOGLE()}, a reactive value.
#'
#' @param id  An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny actionButton updateActionButton
#' @rdname authGoogle
authGoogleUI <- function(id) {
  ns <- NS(id)

  actionButton(ns("authButton"), label = "Log In", icon = icon("paper-plane"),
               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
}

#' @param input     shiny server input
#' @param output    shiny server output
#' @param session   shiny server session
#' @param authFile  Private url to a Google Drive file with access control.
#'
#' @export
#' @importFrom googledrive as_id drive_auth drive_deauth
#' @importFrom shiny icon reactiveVal updateActionButton
#' @importFrom utils getFromNamespace
#' @rdname authGoogle
authGoogle <- function(input, output, session, authFile) {
  access_cred <- getFromNamespace("access_cred", "googledrive")
  session$userData$AUTH_GOOGLE <- reactiveVal(ifelse(is.null(access_cred()), FALSE, TRUE))
  tmpf <- normalizePath(tempfile(fileext = ".txt"), winslash = "/", mustWork = FALSE)

  observeEvent(input$authButton, {
    auth_status <- session$userData$AUTH_GOOGLE()

    if (isTRUE(auth_status)) {
      googledrive::drive_deauth(clear_cache = TRUE, verbose = FALSE)
      session$userData$AUTH_GOOGLE(FALSE)
      updateActionButton(session, "authButton", label = "Log In", icon = icon("paper-plane"))
      file.remove(tmpf)
    } else {
      googledrive::drive_auth(cache = FALSE, verbose = FALSE)
      googledrive::drive_download(googledrive::as_id(authFile), tmpf,
                                  overwrite = TRUE, verbose = FALSE)

      if (file.exists(tmpf)) {
        session$userData$AUTH_GOOGLE(TRUE)
        updateActionButton(session, "authButton", label = "Log Out", icon = icon("ban"))
      }
    }
  })
}
