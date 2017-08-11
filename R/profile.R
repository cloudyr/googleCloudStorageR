#' Save your R session to the cloud on startup
#'
#' Place within your \code{.Rprofile} to load and save your session data automatically
#'
#' @param bucket The bucket holding your session data
#'
#' @details
#'
#' This will save your R sessions to Google Cloud Storage.  Control which projects via where you place the \code{.Rprofile} - see \code{?Startup} for details.
#'
#' If using RStudio in Google Compute Engine, this can be useful to make sure your
#'   RStudio project data is persistent between sessions.
#'
#' You will need to authenticate first,
#'  the easiest way is to make sure your authentication file is
#'  available in environment file \code{GCS_DEFAULT_BUCKET}
#'
#' @examples
#'
#' \dontrun{
#'
#' ## within your .Rprofile file
#' .First <- function(){
#'   cat("\n# Welcome Mark! Today is ", date(), "\n")
#'
#'   googleCloudStorageR::gcs_first()
#' }
#'
#'
#' .Last <- function(){
#'   googleCloudStorageR::gcs_last()
#'   message("\nGoodbye Mark at ", date(), "\n")
#' }
#'
#' }
#' @export
gcs_first <- function(bucket = Sys.getenv("GCS_DEFAULT_BUCKET")){

  if(bucket == ""){
    message("No bucket set at GCS_DEFAULT_BUCKET, not attempt to load workspace")
    return()
  }

  local({
    if(interactive()){
      options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.read_write")
      auth_try <- googleAuthR::gar_gce_auth()
      if(is.null(auth_try)){
        gcs_auth()
      }
      o <- tryCatch(gcs_list_objects(prefix = getwd(), bucket = bucket),
                    error = function(ex){
                      message("Couldn't connect to Google Cloud Storage")
                      return()
                    })
      gcs_rdata <- paste0(getwd(),"/.RData")
      if(gcs_rdata %in% o$name){
        message("\n[Workspace loaded from \n",
                "gs://",bucket,
                gcs_rdata, "]")
        tryCatch(suppressMessages(gcs_load(file = gcs_rdata,
                                           bucket = bucket)),
                 error = function(ex){
                   warning("# No file found on GCS")
                 })

      } else {
        message("\nNo GCS .RData found for this working directory")
      }
    }
  })

}

#' @export
#' @rdname gcs_first
gcs_last <- function(bucket = Sys.getenv("GCS_DEFAULT_BUCKET")){

  if(bucket == ""){
    message("No bucket set at GCS_DEFAULT_BUCKET, not attempt to save workspace")
    return()
  }

  if(interactive()){
    options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.read_write")
    auth_try <- googleAuthR::gar_gce_auth()
    if(is.null(auth_try)){
      gcs_auth()
    }
    message("\nSaving .RData to Google Cloud Storage:\n",
            bucket)
    tryCatch(gcs_save_image(bucket = bucket,
                            saveLocation = getwd(),
                            envir = .GlobalEnv),
             error = function(ex){
               warning("Problem saving to GCS")
             })
  }

}
