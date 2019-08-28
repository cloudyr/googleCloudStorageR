#' Save an R session to the Google Cloud
#'
#' Performs \link{save.image} then saves it to Google Cloud Storage.
#'
#' @param file Where to save the file in GCS and locally
#' @param bucket Bucket to store objects in
#' @param saveLocation Which folder in the bucket to save file
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
#' @return The GCS object
#' @export
gcs_save_image <- function(file = ".RData",
                           bucket = gcs_get_global_bucket(),
                           saveLocation = NULL,
                           envir = parent.frame()){

  if(!is.null(saveLocation)){
    file <- paste0(saveLocation, "/", file)
  }

  bucket <- as.bucket_name(bucket)

  gcs_save(list = ls(all.names = TRUE, envir = envir),
           file = file,
           bucket = bucket,
           envir = envir)

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
#' @return The GCS object
#' @export
gcs_save <- function(...,
                     file,
                     bucket = gcs_get_global_bucket(),
                     envir = parent.frame()){

  tmp <- tempfile()
  on.exit(unlink(tmp))

  bucket <- as.bucket_name(bucket)

  save(..., file = tmp, envir = envir)

  gcs_upload(tmp, bucket = bucket, name = file)

}


#' Load .RData objects or sessions from the Google Cloud
#'
#' Load R objects that have been saved using \link{gcs_save} or \link{gcs_save_image}
#'
#' @param file Where the files are stored
#' @param bucket Bucket the stored objects are in
#' @param envir Environment to load objects into
#' @param saveToDisk Where to save the loaded file.  Default same file name
#' @param overwrite If file exists, overwrite. Default TRUE.
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
                     saveToDisk = file,
                     overwrite = TRUE){

  bucket <- as.bucket_name(bucket)

  gcs_get_object(file, bucket = bucket,
                 saveToDisk = saveToDisk, overwrite = overwrite)
  load(saveToDisk, envir = envir)

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
#' @import assertthat
#' @export
gcs_source <- function(script,
                       bucket = gcs_get_global_bucket(),
                       ...){

  file <- tempfile(fileext = ".R")
  on.exit(unlink(file))

  bucket <- as.bucket_name(bucket)

  gcs_get_object(script, bucket = bucket, saveToDisk = file)

  assert_that(is.readable(file))

  source(file, ...)
}

#' Save/Load all files in directory to Google Cloud Storage
#'
#' This function takes all the files in the directory, zips them, and saves/loads/deletes them to the cloud.  The upload name will be the directory name.
#'
#' @param directory The folder to upload/download
#' @param bucket Bucket to store within
#' @param pattern An optional regular expression. Only file names which match the regular expression will be saved.
#' @param exdir When downloading, specify a destination directory if required
#' @param list When downloading, only list where the files would unzip to
#'
#' @details
#'
#' Zip/unzip is performed before upload and after download using \link[zip]{zipr}.
#'
#'
#' @return When uploading the GCS meta object; when downloading \code{TRUE} if successful
#'
#' @export
#' @importFrom zip zip
#' @family R session data functions
gcs_save_all <- function(directory = getwd(),
                         bucket = gcs_get_global_bucket(),
                         pattern = ""){

  tmp <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp))

  bucket <- as.bucket_name(bucket)

  the_files <- file.path(directory,
                         list.files(path = directory,
                                    all.files = TRUE,
                                    recursive = TRUE,
                                    pattern = pattern))

  zip::zipr(tmp, files = the_files)

  gcs_upload(tmp, bucket = bucket, name = directory)

}

#' @export
#' @rdname gcs_save_all
#' @importFrom utils unzip
gcs_load_all <- function(directory = getwd(),
                         bucket = gcs_get_global_bucket(),
                         exdir = directory,
                         list = FALSE){
  tmp <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp))

  bucket <- as.bucket_name(bucket)

  gcs_get_object(directory, bucket = bucket, saveToDisk = tmp)

  tmp2 <- tempdir()
  on.exit(unlink(tmp2))

  unzipped <- unzip(tmp, exdir = tmp2)

  new_files <- gsub(directory,exdir,gsub(tmp2, "", unzipped))

  if(list){
    return(new_files)
  }

  mapply(function(x, name){
    suppressWarnings(file.copy(x, name,
                               overwrite = FALSE,
                               recursive = TRUE,
                               copy.date = TRUE))
  }, unzipped, new_files)

  TRUE

}

#' @export
#' @rdname gcs_save_all
gcs_delete_all <- function(directory = getwd(),
                           bucket = gcs_get_global_bucket()){

  bucket <- as.bucket_name(bucket)

  o <- gcs_list_objects(bucket, prefix = directory)

  if(!is.null(o$name) && directory %in% o$name){
    gcs_delete_object(o$name, bucket = bucket)
  } else {
    message("No files found to delete for ", directory, " in ", bucket)
  }

}
