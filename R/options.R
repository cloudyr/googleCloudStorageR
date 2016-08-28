.onLoad <- function(libname, pkgname) {



  invisible()

}

.onAttach <- function(libname, pkgname){

  googleAuthR::gar_attach_auto_auth("https://www.googleapis.com/auth/devstorage.full_control",
                                    environment_var = "GCS_AUTH_FILE",
                                    travis_environment_var = "TRAVIS_GCS_AUTH_FILE")

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
