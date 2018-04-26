#' Authorize a user using Google Drive account
#'
#' Logs a user into their Google account for authorized use of the app.
#' Allows app content to be made conditionally available to 3 classes of user:
#'
#' 1. Anonymous (non-logged in) users;
#' 2. Logged in users who are not autheticated against a whitelist;
#' 3. Logged in and authenticated users.
#'
#' This mechanism relies on the file permissions set on a file in Google Drive.
#' Simply create a blank text file (e.g., name_of_your_app.txt) in Google Drive
#' and edit the access control settings to allow only authorized users to download/view
#' this file.
#' You will need the private URL (link) to this file.
#' Assign this URL to a variable in your \file{global.R} and pass that as the
#' \code{authFile} argument when calling the \code{authGoogle} module.
#'
#' This allows access control to be managed via Google rather than locally on the shiny server.
#'
#' Login status is stored in \code{session$userData$userLoggedIn()}, a reactive value.
#' Authentication status is stored in \code{session$userData$userAuthorized()}, a reactive value.
#'
#' @section Additional requirements:
#' Your \file{global.R} file should set the following options:
#' 1. `googleAuthR.scopes.selected`: `c("https://www.googleapis.com/auth/drive.readonly", "https://www.googleapis.com/auth/userinfo.email", "https://www.googleapis.com/auth/userinfo.profile")`.
#' 2. `googleAuthR.webapp.client_id`: your Google app oauth id.
#' 3. `googleAuthR.webapp.client_secret`: your Google app oauth "secret".
#'
#' Be sure to also set \code{appURL} and \code{authFile} in \file{global.R}.
#'
#' See the authentication vignette (\code{vignette("authentication", "SpaDES.shiny")}).
#'
#' @note Based on \code{googleAuthR::googleAuth} and \code{googleAuthR::googleAuthUI}.
#'
#' @param id  An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @author Alex Chubaty
#' @author Mark Edmondson
#' @export
#' @importFrom shiny actionButton
#' @rdname authGoogle
authGoogleUI <- function(id) {
  ns <- NS(id)

  tagList(
    p(textOutput(ns("username"))),
    uiOutput(ns("button"))
  )
}

#' @param input     shiny server input
#' @param output    shiny server output
#' @param session   shiny server session
#' @param appURL    URL to the hosted app.
#' @param authFile  Private url to a Google Drive file with access control.
#'
#' @return          A list of the user's Google profile details (name, email, etc.)
#'
#' @export
#' @importFrom googleAuthR Authentication with_shiny
#' @importFrom googledrive as_id
#' @importFrom googleID get_user_info
#' @importFrom shiny a icon isolate need reactive reactiveVal updateActionButton validate
#' @importFrom shinyjs onclick runjs useShinyjs
#' @importFrom utils getFromNamespace
#' @rdname authGoogle
authGoogle <- function(input, output, session, appURL, authFile) {
  ns <- session$ns
  authReturnCode <- getFromNamespace("authReturnCode", "googleAuthR")
  gar_api_generator <- getFromNamespace("gar_api_generator", "googleAuthR")
  gar_shiny_getAuthUrl <- getFromNamespace("gar_shiny_getAuthUrl", "googleAuthR")
  gar_shiny_getToken <- getFromNamespace("gar_shiny_getToken", "googleAuthR")
  gar_shiny_getUrl <- getFromNamespace("gar_shiny_getUrl", "googleAuthR")

  auth_drive <- function(file) {
    authFileID <- as.character(googledrive::as_id(file))
    api_url <- "https://www.googleapis.com/drive/v3/files/"
    g <- gar_api_generator(paste0(api_url, authFileID), "GET")
    req <- try(g())

    if (!is(req, "try-error")) {
      if (req$status_code == "200") {
        return(TRUE) # status 200 means authenticated correctly
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  }

  login_text <- "Login via Google"   ##
  logout_text <- "Logout"            ##
  login_class <- "btn btn-primary"   ##
  logout_class <- "btn btn-default"  ##
  access_type <- "online"            ## match.arg(access_type)
  approval_prompt <- "auto"          ## match.arg(approval_prompt)

  session$userData$userAuthorized <- reactiveVal(FALSE)
  session$userData$userLoggedIn <- reactiveVal(FALSE)
  tmpf <- normalizePath(tempfile(fileext = ".txt"), winslash = "/", mustWork = FALSE)

  ## Google authentication token
  accessToken <- reactive({
    if (!is.null(authReturnCode(session))) {
      app_url <- gar_shiny_getUrl(session)
      access_token <- gar_shiny_getToken(authReturnCode(session), app_url)
      Authentication$set("public", "app_url", app_url, overwrite = TRUE)
      Authentication$set("public", "shiny", TRUE, overwrite = TRUE)
      access_token
    } else {
      NULL
    }
  })

  ## Google user information
  userDetails <- reactive({
    validate(
      need(accessToken(), "Please log in with your Google account.")
    )
    session$userData$userLoggedIn(TRUE)
    with_shiny(get_user_info, shiny_access_token = accessToken())
  })

  output$username <- renderText({
    validate(
      need(userDetails(), "Getting user details...")
    )
    paste("Logged in as:", userDetails()$displayName)
  })

  ## the UI components
  output$button <- renderUI({
    if (is.null(isolate(accessToken()))) {
      auth_url <- gar_shiny_getAuthUrl(gar_shiny_getUrl(session),
                                       access_type = access_type,
                                       approval_prompt = approval_prompt)
      actionLink(
        ns("sign_in"),
        a(login_text, href = auth_url, class = login_class, role = "button"),
        icon = icon("google")
      )
    } else {
      shiny_url <- gar_shiny_getUrl(session)
      a(logout_text, id = ns("sign_out"), href = shiny_url, class = logout_class, role = "button")
    }
  })

  observe({
    if (isTRUE(session$userData$userLoggedIn())) {
      auth_status <- with_shiny(auth_drive, shiny_access_token = accessToken(), file = authFile)
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
