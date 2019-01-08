.onLoad <- function(libname, pkgname) {

  op <- options()
  op.googleCloudStorageR <- list(
    googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/devstorage.full_control")
  )
  
  toset <- !(names(op.googleCloudStorageR) %in% names(op))
  
  if(any(toset)) options(op.googleCloudStorageR[toset])
  
  invisible()

}

.onAttach <- function(libname, pkgname){

  googleAuthR::gar_attach_auto_auth(c("https://www.googleapis.com/auth/devstorage.full_control",
                                      "https://www.googleapis.com/auth/cloud-platform"),
                                      environment_var = "GCS_AUTH_FILE")

  if(Sys.getenv("GCS_DEFAULT_BUCKET") != ""){
    .gcs_env$bucket <- Sys.getenv("GCS_DEFAULT_BUCKET")
    packageStartupMessage("Set default bucket name to '", Sys.getenv("GCS_DEFAULT_BUCKET"),"'")
  }

  invisible()

}
