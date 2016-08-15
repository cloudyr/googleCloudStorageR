#' Save an R session to the Google Cloud
#'
#' Performs \link{save.image} then saves it to Google Cloud Storage.
#'
#' @param bucket Bucket to store objects in
#' @param file Where to save the file in GCS and locally
#'
#' @details
#'
#' \code{gcs_save(bucket = "your_bucket")} will save all objects in the workspace
#'   to \code{.RData} folder on Google Cloud Storage within \code{your_bucket}.
#'
#' Restore the objects using \code{gcs_load(bucket = "your_bucket")}
#'
#' This will overwrite any data with the same name in folder.
#'
#' @family R session data functions
#' @return TRUE if successful
#' @export
gcs_save <- function(bucket = gcs_get_global_bucket(),
                     file = ".RData"){

  save.image(file = file)

  gcs_upload(file, bucket = bucket, name = file)

  TRUE

}


#' Load an R session from the Google Cloud
#'
#' Load R objects that have been saved using \link{gcs_save}
#'
#' @param bucket Bucket the stored objects are in
#' @param file Where the files are stored
#' @param envir Environment to load objects into
#'
#' @family R session data functions
#' @return TRUE if successful
#' @export
gcs_load <- function(bucket = gcs_get_global_bucket(),
                     file = ".RData",
                     envir = .GlobalEnv){

  gcs_get_object(file, bucket = bucket, saveToDisk = file)
  load(file, envir = envir)

  TRUE
}
