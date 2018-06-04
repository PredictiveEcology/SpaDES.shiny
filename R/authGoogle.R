#' Authorize a user using Google Drive account
#'
#' Logs a user into their Google account for authorized use of the app.
#' Allows app content to be made conditionally available to 3 classes of user:
#'
#' 1. Anonymous (non-logged in) users;
#' 2. Logged in users who are not autheticated against a whitelist;
#' 3. Logged in and authenticated users.
#'
#' This allows authentication to be managed via Google rather than locally on the shiny server.
#' Only the whitelist of approved users is kept locally.
#'
#' Login status is stored in \code{session$userData$userLoggedIn()}, a reactive value.
#' Authentication status is stored in \code{session$userData$userAuthorized()}, a reactive value.
#'
#' @section Additional requirements:
#' Your \file{global.R} file should set the following options:
#' 1. `googleAuthR.scopes.selected`: `c("https://www.googleapis.com/auth/userinfo.email", "https://www.googleapis.com/auth/userinfo.profile")`.
#' 2. `googleAuthR.webapp.client_id`: your Google app oauth id.
#' 3. `googleAuthR.webapp.client_secret`: your Google app oauth "secret".
#'
#' Be sure to also set \code{appURL} and \code{authUsers} in \file{global.R}.
#'
#' See the authentication vignette (\code{vignette("authentication", "SpaDES.shiny")}).
#'
#' @note Based on \code{googleAuthR::googleAuth} and \code{googleAuthR::googleAuthUI}.
#'
#' @param id  An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom shiny actionButton
#' @rdname authGoogle
authGoogleUI <- function(id) {
  ns <- NS(id)

  tagList(
    p(textOutput(ns("username"))),
    googleAuthUI(ns("loginButton"))
  )
}

#' @param input     shiny server input
#' @param output    shiny server output
#' @param session   shiny server session
#' @param appURL    URL to the hosted app.
#' @param authUsers A character vector of authorized user email addresses.
#' @param icon      Default \code{"google"}. Name of icon to display beside the login button.
#'                  Use of an icon with \pkg{shinydashboard} may produce undesired results.
#'                  Disable use of the icon with \code{icon = NULL}.
#'
#' @return          A list of the user's Google profile details (name, email, etc.)
#'
#' @export
#' @importFrom googleAuthR Authentication with_shiny
#' @importFrom googledrive as_id
#' @importFrom googleID get_user_info whitelist
#' @importFrom shiny a icon isolate need reactive reactiveVal updateActionButton validate
#' @importFrom shinyjs onclick runjs useShinyjs
#' @importFrom utils getFromNamespace
#' @rdname authGoogle
authGoogle <- function(input, output, session, appURL, authUsers, icon = "google") {
  ns <- session$ns

  session$userData$userAuthorized <- reactiveVal(FALSE)
  session$userData$userLoggedIn <- reactiveVal(FALSE)

  ## Google authentication token
  accessToken <- callModule(googleAuth, "loginButton", login_text = "Login via Google")

  ## Google user information
  userDetails <- reactive({
    if (isTruthy(accessToken())) {
      session$userData$userLoggedIn(TRUE)
      with_shiny(googleID::get_user_info, shiny_access_token = accessToken())
    } else {
      NULL
    }
  })

  output$username <- renderText({
    validate(
      need(userDetails(), "Please log in using your Google account.")
    )
    paste("Logged in as:", userDetails()$displayName)
  })

  observe({
    if (isTRUE(session$userData$userLoggedIn())) {
      auth_status <- googleID::whitelist(userDetails(), authUsers)
      ifelse(auth_status, session$userData$userAuthorized(TRUE), session$userData$userAuthorized(FALSE))

      ## Workaround to avoid shinyaps.io URL problems
      shinyjs::onclick("sign_out",
                       shinyjs::runjs(paste0("window.location.href = '", appURL, "';")))
    } else {
      session$userData$userAuthorized(FALSE)
    }
  }, label = "observer__login_status")

  return(userDetails)
}
