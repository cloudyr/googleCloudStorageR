#' Upload a file of arbitary type
#'
#' Simple Upload - for files up to 5MB to Google Cloud Storage
#'
#' @param file data.frame, list, R object or filepath (character) to upload file
#' @param bucket bucketname you are uploading to
#' @param type MIME type, guessed from file extension if NULL
#' @param name What to call the file once uploaded. Default is the filepath.
#' @param object_function If not NULL, function to use on file before upload
#' @param object_metadata Optional metadata for object created via \link{gcs_metadata_object}
#' @param predefinedAcl Specify user access to object. Default is 'private'
#'
#' @details
#'
#' If \code{object_function} is NULL and \code{file} is not a character filepath,
#'   the defaults are:
#'
#' \itemize{
#'   \item file's class is \code{data.frame} - \link[utils]{write.csv}
#'   \item file's class is \code{list} - \link[jsonlite]{toJSON}
#'  }
#'
#' If \code{object_function} is not NULL and \code{file} is not a character filepath,
#'   then \code{object_function} will be applied to the R object specified
#'   in \code{file} before upload. You may want to also use \code{name} to ensure the correct
#'   file extension is used e.g. \code{name = 'myobject.feather'}
#'
#' If \code{file} or \code{name} argument contains folders e.g. \code{/data/file.csv} then
#'   the file will be uploaded with the same folder structure e.g. in a \code{/data/} folder.
#' Use \code{name} to override this.
#'
#' @section scopes:
#'
#' Requires scopes \code{https://www.googleapis.com/auth/devstorage.read_write}
#'   or \code{https://www.googleapis.com/auth/devstorage.full_control}
#'
#' @return If successful, a metadata object
#' @import httr utils
#' @export
gcs_upload <- function(file,
                       bucket = gcs_get_global_bucket(),
                       type = NULL,
                       name = deparse(substitute(file)),
                       object_function = NULL,
                       object_metadata = NULL,
                       predefinedAcl = c(
                         "private",
                         "authenticatedRead",
                         "bucketOwnerFullControl",
                         "buckerOwnerRead",
                         "projectPrivate",
                         "publicRead"
                       )){

  predefinedAcl <- match.arg(predefinedAcl)
  upload_limit <- 5000000

  ## no leading slashes
  name <- gsub("^/","", utils::URLencode(name, reserved = TRUE))

  if(inherits(file, "character")){
    # a filepath

    temp <- file

  } else if(!is.null(object_function)){
    # user specified object write function

    temp <- tempfile()
    on.exit(unlink(temp))

    write(object_function(file), temp)

    if(!file.exists(temp)){
      stop("Problem writing file using object_function")
    }

  } else if(inherits(file, "data.frame")){
    # data.frame via write.csv

    temp <- tempfile(fileext = ".csv")
    on.exit(unlink(temp))

    write.csv(file, file = temp)

    name <- if(!grepl("\\.csv$", name)) paste0(name,".csv") else name

  } else if(inherits(file, "list")){
    # JSON via toJSON

    temp <- tempfile(fileext = ".json")
    on.exit(unlink(temp))

    write(jsonlite::toJSON(file), temp)

    name <- if(!grepl("\\.json$", name)) paste0(name,".json") else name

  } else {
    stop("Unsupported object type passed in argument file: specify filepath or define a
         write function using argument object_function")
  }

  if(file.size(temp) > upload_limit){
    ## resumable upload
    stop("File size too large, over 5MB")
  } else if (!is.null(object_metadata)){
    ## multipart upload

    if(!is.null(object_metadata$name)){
      name <- object_metadata$name
    }

    temp2 <- tempfile()
    on.exit(unlink(temp2))

    ## http://stackoverflow.com/questions/31080363/how-to-post-multipart-related-content-with-httr-for-google-drive-api
    writeLines(jsonlite::toJSON(object_metadata), temp2)

    bb <- list(
      metadata = httr::upload_file(temp2, type = "application/json; charset=UTF-8"),
      content = httr::upload_file(temp, type = type)
    )

    up <-
      googleAuthR::gar_api_generator("https://www.googleapis.com/upload/storage/v1",
                                     "POST",
                                     path_args = list(b = "myBucket",
                                                      o = ""),
                                     pars_args = list(uploadType="multipart",
                                                      name=name,
                                                      predefinedAcl=predefinedAcl),
                                     customConfig = list(
                                       encode = "multipart",
                                       httr::add_headers("Content-Type" =  "multipart/related")
                                     ))

    req <- up(path_arguments = list(b = bucket),
              pars_arguments = list(name = name),
              the_body = bb)
    out <- structure(req$content, class = "gcs_objectmeta")

  } else {
    ## simple upload <5MB
    bb <- httr::upload_file(temp, type = type)

    up <-
      googleAuthR::gar_api_generator("https://www.googleapis.com/upload/storage/v1",
                                     "POST",
                                     path_args = list(b = "myBucket",
                                                      o = ""),
                                     pars_args = list(uploadType="media",
                                                      name=name,
                                                      predefinedAcl=predefinedAcl))

    req <- up(path_arguments = list(b = bucket),
              pars_arguments = list(name = name),
              the_body = bb)

    out <- structure(req$content, class = "gcs_objectmeta")
  }

  out

}

#
# #' Make the multipart inner content
# #'
# #' @param f The original unbatched Google API function call generated by \code{gar_api_generator}
# #'
# #' @keywords internal
# make_multipart_body <- function(metadata){
#
#   ## construct POST request
#
#   if(!is.null(f$the_body)){
#     batch_body <- jsonlite::toJSON(f$the_body, auto_unbox = TRUE)
#     myMessage("Batch Body JSON parsed to:", batch_body, level=1)
#     part_content_length <- nchar(batch_body, type="bytes")
#
#     header <- paste(boundary,
#                     "Content-Type: application/http",
#                     paste0("Content-ID: ",f$name),
#                     sep = "\r\n")
#     body_header <- paste(req,
#                          "Content-Type: application/json",
#                          paste("Content-Length: ", part_content_length),
#                          "\r\n",
#                          sep="\r\n")
#
#     parsed <- paste(header, body_header, batch_body, "\r\n", sep = "\r\n")
#
#   } else {
#
#     header <- paste(boundary,
#                     "Content-Type: application/http",
#                     paste0("Content-ID: ",f$name),
#                     sep = "\r\n")
#     parsed <- paste(header, req,"\r\n", sep = "\r\n")
#
#   }
#
#   parsed
#
# }
