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
#' This will overwrite any data with the same name in your current local environment.
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

#' Save .RData objects to the Google Cloud
#'
#' Performs \link{save} then saves it to Google Cloud Storage.
#'
#' @param ... The names of the objects to be saved (as symbols or character strings).
#' @param file The file name that will be uploaded (conventionally with file extension \code{.RData})
#' @param bucket Bucket to store objects in
#' @param envir Environment to search for objects to be saved
#'
#' @details
#'
#' For all session data use \link{gcs_save_image} instead.
#'
#' \code{gcs_save(ob1, ob2, ob3, file = "mydata.RData")} will save the objects specified to an \code{.RData} file then save it to Cloud Storage, to be loaded later using \link{gcs_load}.
#'
#' For any other use, its better to use \link{gcs_upload} and \link{gcs_get_object} instead.
#'
#' Restore the R objects using \code{gcs_load(bucket = "your_bucket")}
#'
#' This will overwrite any data within your local environment with the same name.
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


#' Load .RData objects or sessions from the Google Cloud
#'
#' Load R objects that have been saved using \link{gcs_save} or \link{gcs_save_image}
#'
#' @param file Where the files are stored
#' @param bucket Bucket the stored objects are in
#' @param envir Environment to load objects into
#' @param saveToDisk Where to save the loaded file.  Default same file name
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
                     envir = .GlobalEnv,
                     saveToDisk = file){

  gcs_get_object(file, bucket = bucket, saveToDisk = saveToDisk)
  load(file, envir = envir)

  TRUE
}

#' Source an R script from the Google Cloud
#'
#' Download an R script and run it immediately via \link{source}
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

  file <- tempfile(fileext = ".R")
  on.exit(unlink(file))

  gcs_get_object(script, bucket = bucket, saveToDisk = file)

  assertthat::assert_that(assertthat::is.readable(file))

  source(file, ...)
}
