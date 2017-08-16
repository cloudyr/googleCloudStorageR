#' Save your R session to the cloud on startup
#'
#' Place within your \code{.Rprofile} to load and save your session data automatically
#'
#' @param bucket The bucket holding your session data, default environment arg \code{GCS_SESSION_BUCKET}
#' @param directory The name of the directory to download from
#'
#' @details
#'
#' The folder you want to save to GCS will also need to have a file called \code{_gcssave.yaml} in the root of the directory.  It can hold the following arguments:
#'
#' \itemize{
#'  \item [Required] \code{bucket} - the GCS bucket to save to
#'  \item [Optional] \code{loaddir} - if the folder name is different to the current, where to load the R session from
#'  \item [Optional] \code{pattern} - a regex of what files to save at the end of the session
#'  \item [Optional] \code{load_on_startup} - if \code{FALSE} will not attempt to load on startup
#' }
#'
#' The bucket name can also be defined via the environment arg \code{GCE_SESSION_BUCKET}. The yaml file name will take precedence.
#'
#' The folder is named the full working path to the working directory when saved to GCS. e.g. \code{/Users/mark/dev/your-r-project} - if you create a new R project with the same filepath and bucket set the files will download automatically.  If loading from a different filepath (e.g. with \code{loadir} set in yaml), when you exit and save the files will be saved under your new working directory.
#'
#' Files with the same name will not be overwritten.  If you want them to be, delete or rename them then reload the R session.
#'
#' If using RStudio in Google Compute Engine, this can be useful to make sure your
#'   RStudio project data is persistent between sessions.
#'
#' You will need to authenticate first,
#'  the easiest way is to make sure your authentication file is
#'  available in environment file \code{GCS_AUTH_FILE}, or if on Google Compute Engine it will reuse the Google Cloud authentication via \link[googleAuthR]{gar_gce_auth}
#'
#' @examples
#'
#' \dontrun{
#'
#' ## within your .Rprofile file
#' .First <- function(){
#'   cat("\n# Welcome Mark! Today is ", date(), "\n")
#'
#'   ## will look for download if GCS_SESSION_BUCKET env arg set
#'   googleCloudStorageR::gcs_first()
#' }
#'
#'
#' .Last <- function(){
#'   # will only upload if a _gcssave.yaml in directory with bucketname
#'   googleCloudStorageR::gcs_last()
#'   message("\nGoodbye Mark at ", date(), "\n")
#' }
#'
#' }
#' @export
#' @importFrom googleAuthR gar_gce_auth
#' @importFrom yaml yaml.load_file
gcs_first <- function(directory = getwd(),
                      bucket = Sys.getenv("GCS_SESSION_BUCKET")){

  local({
    if(interactive()){
      if(file.exists("_gcssave.yaml")){
        yaml <- yaml.load_file("_gcssave.yaml")
      } else {
        yaml <- NULL
      }

      if(!is.null(yaml$load_on_startup)){
        if(!yaml$load_on_startup){
          message("Skipping cloud load as load_on_startup = FALSE")
          return()
        }
      }

      if(!is.null(yaml$loaddir)){
        gcs_rdata <- yaml$loaddir
      } else {
        gcs_rdata <- directory
      }

      if(!is.null(yaml$bucket)){
        bucket <- yaml$bucket
      }

      if(bucket == ""){
        ## attempt load from GCE metadata if possible
        bucket <- tryCatch(googleComputeEngineR::gce_metadata_env('GCS_SESSION_BUCKET'),
                           error = function(ex){
                             message("No GCS_SESSION_BUCKET, yaml or GCE metadata found,
                                     no attempt to load workspace")
                             return()
                           })
      }

      options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.read_write")
      auth_try <- gar_gce_auth()
      if(is.null(auth_try)){
        gcs_auth()
      }

      o <- tryCatch(gcs_list_objects(prefix = gcs_rdata, bucket = bucket),
                    error = function(ex){
                      message("Couldn't connect to Google Cloud Storage")
                      return()
                    })

      if(gcs_rdata %in% o$name){
        message("\n[Workspace loaded from: \n",
                "gs://",bucket,
                gcs_rdata, "]")
        tryCatch(suppressMessages(gcs_load_all(gcs_rdata,
                                               bucket = bucket,
                                               exdir = getwd())),
                 error = function(ex){
                   warning("# No file found on GCS - ", ex)
                 })

      } else {
        message("\nNo cloud data found for this working directory")
      }
    }
  })

}

#' @export
#' @rdname gcs_first
#' @importFrom yaml yaml.load_file
#' @importFrom googleAuthR gar_gce_auth
gcs_last <- function(bucket = Sys.getenv("GCS_SESSION_BUCKET")){

  if(!interactive()){
    return()
  }

  if(!file.exists("_gcssave.yaml") & !file.exists("_gcssave.yml")){
    message("No Cloud Storage bucket set as no _gcssave.yaml file found, no attempt to save workspace")
    return()
  } else { #ridic yml or yaml
    if(file.exists("_gcssave.yaml")){
      yaml <- yaml.load_file("_gcssave.yaml")
    } else {
      yaml <- yaml.load_file("_gcssave.yml")
    }
  }

  if(!is.null(yaml$bucket)){
    bucket <- yaml$bucket
  }

  if(!is.null(yaml$pattern)){
    pattern <- yaml$pattern
  } else{
    pattern = ""
  }


  if(bucket == ""){
    message("No Cloud Storage bucket set at GCS_SESSION_BUCKET, no attempt to save workspace")
    return()
  }
  options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.read_write")
  auth_try <- gar_gce_auth()
  if(is.null(auth_try)){
    gcs_auth()
  }

  message("\nSaving data to Google Cloud Storage:\n",
          bucket)
  tryCatch(gcs_save_all(getwd(), bucket = bucket, pattern = pattern),
           error = function(ex){
             warning("Problem saving to GCS")
           })

}
