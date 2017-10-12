.onLoad <- function(libname, pkgname) {

  invisible()

}

.onAttach <- function(libname, pkgname){

  attempt <- try(googleAuthR::gar_attach_auto_auth("https://www.googleapis.com/auth/devstorage.full_control",
                                    environment_var = "GCS_AUTH_FILE"))

  if(inherits(attempt, "try-error")){
    warning("Problem using auto-authentication when loading from GCS_AUTH_FILE: \n", attempt, "
            Run googleAuthR::gar_auth() or googleAuthR::gar_auth_service() instead.")
  }

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
    packageStartupMessage("Set default bucket name to '", Sys.getenv("GCS_DEFAULT_BUCKET"),"'")
  }

  invisible()

}
