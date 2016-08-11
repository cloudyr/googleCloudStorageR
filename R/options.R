.onLoad <- function(libname, pkgname) {

  scopes <- getOption("googleAuthR.scopes.selected")
  if(!("https://www.googleapis.com/auth/devstorage.full_control" %in% scopes)){
    # packageStartupMessage("Adding https://www.googleapis.com/auth/devstorage.full_control scope")
    new_scopes <- c(getOption("googleAuthR.scopes.selected"),
                    "https://www.googleapis.com/auth/devstorage.full_control")
  } else {
    new_scopes <- scopes
  }

  options(googleAuthR.scopes.selected = new_scopes)

  if(Sys.getenv("GCS_CLIENT_ID") != ""){
    options(googleAuthR.client_id = Sys.getenv("GCS_CLIENT_ID"))
  }

  if(Sys.getenv("GCS_CLIENT_SECRET") != ""){
    options(googleAuthR.client_secret = Sys.getenv("GCS_CLIENT_SECRET"))
  }

  if(Sys.getenv("GCS_WEB_CLIENT_ID") != ""){
    options(googleAuthR.webapp.client_id = Sys.getenv("GCS_WEB_CLIENT_ID"))
  }

  if(Sys.getenv("GCS_WEB_CLIENT_SECRET") != ""){
    options(googleAuthR.webapp.client_id = Sys.getenv("GCS_WEB_CLIENT_SECRET"))
  }

  if(Sys.getenv("GCS_DEFAULT_BUCKET") != ""){
    .gcs_env$bucket <- Sys.getenv("GCS_DEFAULT_BUCKET")
    # packageStartupMessage("Set default bucket name to '", Sys.getenv("GCS_DEFAULT_BUCKET"),"'")
  }

  invisible()

}
