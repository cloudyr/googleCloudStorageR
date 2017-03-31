#' Get the download URL
#'
#' Create the download URL for objects in buckets
#'
#' @param object_name A vector of object names
#' @param bucket A vector of bucket names
#' @param public TRUE to return a public URL
#'
#' @details
#' \code{bucket} names should be length 1 or same length as \code{object_name}
#'
#' Download URLs can be either authenticated behind a login that you may need to update
#'   access for via \link{gcs_update_object_acl}, or public to all if their \code{predefinedAcl = 'publicRead'}
#'
#' Use the \code{public = TRUE} to return the URL accessible to all, which changes the domain name from
#'   \code{storage.cloud.google.com} to \code{storage.googleapis.com}
#'
#' @return the URL for downloading objects
#'
#'
#'
#' @family download functions
#' @export
gcs_download_url <- function(object_name, bucket = gcs_get_global_bucket(), public = FALSE){
  testthat::expect_type(bucket, "character")
  testthat::expect_type(object_name, "character")

  ## no leading slashes
  object_name <- gsub("^/","", utils::URLencode(object_name, reserved = TRUE))

  if(length(bucket) != 1 && length(bucket) != length(object_name)){
    stop("bucket must be length 1 or same length as object_name")
  }

  if(public){
    domain <- "https://storage.googleapis.com"
  } else {
    domain <- "https://storage.cloud.google.com"
  }

  file.path(domain, bucket, object_name, fsep = "/")
}

#' Parse downloaded objects straight into R
#'
#' Wrapper for \code{httr}'s \link[httr]{content}.  This is the default function used in \link{gcs_get_object}
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
