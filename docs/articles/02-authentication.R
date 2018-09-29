## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----global.R, eval=FALSE------------------------------------------------
#  library(googleAuthR)
#  library(googledrive)
#  library(googleID)
#  
#  options(googleAuthR.scopes.selected = c(
#    "https://www.googleapis.com/auth/drive.readonly",
#    "https://www.googleapis.com/auth/userinfo.email",
#    "https://www.googleapis.com/auth/userinfo.profile"
#  ))
#  options(googleAuthR.webapp.client_id = "YOUR-GOOGLE-APP-ID-STRING")
#  options(googleAuthR.webapp.client_secret = "YOUR-GOOGLE-APP-SECRET")
#  
#  ## the URL to your shiny app
#  appURL <- "http://yourapp.example.com"
#  
#  ## your private URL to the shared file in Google Drive
#  authFile <- "https://drive.google.com/file/d/XXXXXXXX/view?usp=sharing"

## ---- eval=FALSE---------------------------------------------------------
#  if (session$userData$userLoggedIn()) {
#    # content for sogged in users
#  } else {
#    # content for non-logged in users
#  }

## ---- eval=FALSE---------------------------------------------------------
#  if (session$userData$userAuthorized()) {
#    # content for authorized (whitelisted) users
#  } else {
#    # content for non-authorized users
#  }

