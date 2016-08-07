#' Upload a file of arbitary type
#'
#' Simple Upload - for files up to 5MB to Google Cloud Storage
#'
#' @param file data.frame, list or filepath character to what you are uploading
#' @param bucket bucketname you are uploading to
#' @param type MIME type, guessed from file extension if NULL
#' @param name What to call the file once uploaded. Default is the filepath.
#'
#' @details
#'
#' Will turn data.frames into \code{.csv} for upload via \link[utils]{write.csv}
#' Will turn lists into \code{.json} via \link[jsonlite]{toJSON}
#'
#' @section scopes:
#'
#' Requires scopes \code{https://www.googleapis.com/auth/devstorage.read_write}
#'   or \code{https://www.googleapis.com/auth/devstorage.full_control}
#'
#' @return If successful, a metadata object
#' @import httr utils
#' @export
gcs_upload <- function(file, bucket, type = NULL, name = file){

  upload_limit <- 5000000

  if(inherits(file, "character")){
    # a filepath
    if(file.size(file) > upload_limit) stop("File size too large, over 5MB")

    bb <- httr::upload_file(file, type = type)
  } else if(inherits(file, "data.frame")){
    temp <- tempfile(fileext = ".csv")
    on.exit(unlink(temp))
    write.csv(file, file = temp)
    if(file.size(temp) > upload_limit) stop("File size too large, over 5MB")
    bb <- httr::upload_file(temp)
    name <- paste0(deparse(substitute(file)),".csv")
  } else if(inherits(file, "list")){
    temp <- tempfile(fileext = ".json")
    on.exit(unlink(temp))
    write(jsonlite::toJSON(file), temp)
    if(file.size(temp) > upload_limit) stop("File size too large, over 5MB")
    bb <- httr::upload_file(temp)
    name <- paste0(deparse(substitute(file)),".json")
  } else {
    stop("Unsupported object type passed in argument file.")
  }

  ## simple upload <5MB
  up <-
    googleAuthR::gar_api_generator("https://www.googleapis.com/upload/storage/v1",
                                    "POST",
                                   path_args = list(b = "myBucket",
                                                    o = ""),
                                   pars_args = list(uploadType="media",
                                                    name=name))

  req <- up(path_arguments = list(b = bucket),
            pars_arguments = list(name = name),
            the_body = bb)

  req$content
}
