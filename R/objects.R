#' List objects in a bucket
#'
#' @param bucket bucket containing the objects
#'
#' @return A data.frame of the objects
#'
#' @family object functions
#' @export
gcs_list_objects <- function(bucket){

  testthat::expect_type(bucket, "character")
  testthat::expect_length(bucket, 1)

  lo <- googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/",
                                      path_args = list(b = bucket,
                                                       o = ""))
  req <- lo()

  req$content$items

}

#' Get an object in a bucket directly
#'
#' This retrieves an object directly.
#' This differs from providing downloads via a download link as you can
#'   do via \link{gcs_download_url}
#'
#'
#' @param object_name name of object in the bucket
#' @param bucket bucket containing the objects
#' @param meta If TRUE then get info about the object, not the object itself
#' @param saveToDisk Specify a filename to save directly to disk.
#' @param parseObject If saveToDisk is NULL, whether to parse with \link{gcs_parse_download}
#'
#' @return The object, or TRUE if sucessfully saved to disk.
#'
#' @family object functions
#' @export
gcs_get_object <- function(object_name,
                           bucket,
                           meta = FALSE,
                           saveToDisk = NULL,
                           parseObject = TRUE){

  testthat::expect_type(bucket, "character")
  testthat::expect_type(object_name, "character")
  testthat::expect_length(bucket, 1)
  testthat::expect_length(object_name, 1)

  if(meta){
    alt = ""
  } else {
    options(googleAuthR.rawResponse = TRUE)
    alt = "media"
  }

  ob <- googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/",
                                       path_args = list(b = bucket,
                                                        o = object_name),
                                       pars_args = list(alt = alt))
  req <- ob()

  if(!meta){
    options(googleAuthR.rawResponse = FALSE)

    if(req$status_code == 404){
      stop("File not found. Check object_name and if you have read permissions.
           Looked for ", object_name)
    }

    if(!is.null(saveToDisk)){

      bin <- httr::content(req, "raw")
      writeBin(bin, saveToDisk)
      message("Saved ", object_name, " to ", saveToDisk)
      out <- TRUE

    } else {
      message("Downloaded ", object_name)

      if(parseObject){
        out <- gcs_parse_download(req)
      } else {
        out <- req
      }
    }

  } else {
    out <- req$content
  }


  out

}
