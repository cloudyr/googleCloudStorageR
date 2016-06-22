#' Get the download URL
#' 
#' Create the download URL for objects in buckets
#' 
#' @param bucket A vector of bucket names
#' @param object_name A vector of object names
#' 
#' bucket names should be length 1 or same length as object_name
#' 
#' @return the URL for downloading objects
#' 
#' You may need to update access for your email to get access via \link{gcs_update_acl}
#' 
#' @family download functions
#' @export
gcs_download_url <- function(bucket, object_name){
  testthat::expect_type(bucket, "character")
  testthat::expect_type(object_name, "character")
  
  if(length(bucket) != 1 && length(bucket) != length(object_name)){
    stop("bucket must be length 1 or same length as object_name")
  }
  
  paste0("https://storage.cloud.google.com",
         "/", bucket,
         "/", object_name)
}