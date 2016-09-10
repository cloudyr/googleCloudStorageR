#' Get the download URL
#'
#' Create the download URL for objects in buckets
#'
#' @param object_name A vector of object names
#' @param bucket A vector of bucket names
#'
#' bucket names should be length 1 or same length as object_name
#'
#' @return the URL for downloading objects
#'
#' You may need to update access for your email to get access via \link{gcs_update_object_acl}
#'
#' @family download functions
#' @export
gcs_download_url <- function(object_name, bucket = gcs_get_global_bucket()){
  testthat::expect_type(bucket, "character")
  testthat::expect_type(object_name, "character")

  ## no leading slashes
  object_name <- gsub("^/","", utils::URLencode(object_name, reserved = TRUE))

  if(length(bucket) != 1 && length(bucket) != length(object_name)){
    stop("bucket must be length 1 or same length as object_name")
  }

  paste0("https://storage.cloud.google.com",
         "/", bucket,
         "/", object_name)
}

#' Parse downloaded objects straight into R
#'
#' Wrapper for httr::content
#'
#' @param object The object downloaded
#' @param encoding Default to UTF-8
#'
#' @family download functions
#' @seealso gcs_get_object
#' @export
gcs_parse_download <- function(object, encoding = "UTF-8"){
  out <- httr::content(object, encoding = encoding)
  message("Object parsed to class: ", paste(class(out), collapse = " "))
  out
}
