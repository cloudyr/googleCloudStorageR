#' Upload a file or arbitary type
#'
#' Requires scopes https://www.googleapis.com/auth/devstorage.read_write
#'   or https://www.googleapis.com/auth/devstorage.full_control
#'
#' @param file filepath to what you are uploading
#' @param bucket bucketname you are uploading to
#' @param type MIME type, guessed if NULL
#' @param name What to call the file once uploaded. Default is "myObject"
#'
#' @return If successful, a metadata object
#' @export
gcs_upload <- function(file, bucket, type = NULL, name = "myObject"){


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
            the_body = httr::upload_file(file, type = type))

  req$content
}
