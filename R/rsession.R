#' Save an R session to the Google Cloud
#'
#' Performs \link{save.image} then saves it to Google Cloud Storage.
#'
#' @param file Where to save the file in GCS and locally
#' @param bucket Bucket to store objects in
#' @param envir Environment to save from
#'
#' @details
#'
#' \code{gcs_save_image(bucket = "your_bucket")} will save all objects in the workspace
#'   to \code{.RData} folder on Google Cloud Storage within \code{your_bucket}.
#'
#' Restore the objects using \code{gcs_load(bucket = "your_bucket")}
#'
#' This will overwrite any data with the same name in folder.
#'
#' @family R session data functions
#' @return TRUE if successful
#' @export
gcs_save_image <- function(file = ".RData",
                           bucket = gcs_get_global_bucket(),
                           envir = parent.frame()){

  gcs_save(list = ls(all.names = TRUE, envir = envir),
           file = file,
           bucket = bucket,
           envir = envir)

  TRUE

}

#' Save an R object to the Google Cloud
#'
#' Performs \link{save} then saves it to Google Cloud Storage.
#'
#' For all session data use \link{gcs_save_image} instead.
#'
#' @param ... The names of the objects to be saved (as symbols or character strings).
#' @param file The file name that will be uploaded (conventionally with file extension \code{.RData})
#' @param bucket Bucket to store objects in
#' @param envir Environment to search for objects to be saved
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
gcs_save <- function(...,
                     file,
                     bucket = gcs_get_global_bucket(),
                     envir = parent.frame()){

  tmp <- tempfile()
  on.exit(unlink(tmp))

  save(..., file = tmp, envir = envir)

  gcs_upload(tmp, bucket = bucket, name = file)

  TRUE

}


#' Load an R session from the Google Cloud
#'
#' Load R objects that have been saved using \link{gcs_save} or \link{gcs_save_image}
#'
#' @param file Where the files are stored
#' @param bucket Bucket the stored objects are in
#' @param envir Environment to load objects into
#'
#' @details
#'
#' The argument \code{file}'s default is to load an image file
#'   called \code{.RData} from \link{gcs_save_image} into the Global environment.
#'
#' This would overwrite your existing \code{.RData} file in the working directory, so
#'   change the file name if you don't wish this to be the case.
#'
#' @family R session data functions
#' @return TRUE if successful
#' @export
gcs_load <- function(file = ".RData",
                     bucket = gcs_get_global_bucket(),
                     envir = .GlobalEnv){

  gcs_get_object(file, bucket = bucket, saveToDisk = file)
  load(file, envir = envir)

  TRUE
}

#' Source an R script from the Google Cloud
#'
#' Source an R script and run via \link{source}
#'
#' @param script The name of the script on GCS
#' @param bucket Bucket the stored objects are in
#' @param ... Passed to \link{source}
#'
#' @family R session data functions
#' @return TRUE if successful
#' @export
gcs_source <- function(script,
                       bucket = gcs_get_global_bucket(),
                       ...){

  file <- tempfile()
  on.exit(unlink(file))

  suppressMessages(gcs_get_object(script, bucket = bucket, saveToDisk = file))
  source(file, ...)
}
