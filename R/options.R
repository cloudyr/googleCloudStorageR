.onLoad <- function(libname, pkgname) {

  op <- options()
  op.googleCloudStorageR <- list(
    googleCloudStorageR.upload_limit =  5000000L
  )
  
  toset <- !(names(op.googleCloudStorageR) %in% names(op))
  
  if(any(toset)) options(op.googleCloudStorageR[toset])
  
  invisible()

}

.onAttach <- function(libname, pkgname){

  googleAuthR::gar_attach_auto_auth(
    c("https://www.googleapis.com/auth/devstorage.full_control",
      "https://www.googleapis.com/auth/cloud-platform"),
    environment_var = "GCS_AUTH_FILE")

  default_bucket <- Sys.getenv("GCS_DEFAULT_BUCKET")
  if(nzchar(default_bucket)){
    .gcs_env$bucket <- default_bucket
    packageStartupMessage("Set default bucket name to '", default_bucket,"'")
  }

  invisible()

}
