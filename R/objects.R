#' List objects in a bucket
#'
#' @param bucket bucket containing the objects
#' @param detail Set level of detail
#'
#' @details
#'
#' Columns returned by \code{detail} are:
#'
#' \itemize{
#'   \item \code{summary} - name, size, updated
#'   \item \code{more} - as above plus: bucket, contentType, storageClass, timeCreated
#'   \item \code{full} - as above plus: id, selfLink, generation, metageneration, md5Hash, mediaLink, crc32c, etag
#'  }
#'
#'
#' @return A data.frame of the objects
#'
#' @family object functions
#' @export
gcs_list_objects <- function(bucket = gcs_get_global_bucket(),
                             detail = c("summary","more","full")){

  detail <- match.arg(detail)

  testthat::expect_type(bucket, "character")
  testthat::expect_length(bucket, 1)

  parse_lo <- function(x){
    nextPageToken <- x$nextPageToken
    x <- x$items
    x$timeCreated <- timestamp_to_r(x$timeCreated)
    x$updated <- timestamp_to_r(x$updated)
    x$kind <- NULL
    x$size <- vapply(as.numeric(x$size), function(x) format_object_size(x, "auto"), character(1))

    attr(x, "nextPageToken") <- nextPageToken
    x
  }

  lo <- googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/",
                                      path_args = list(b = bucket,
                                                       o = ""),
                                      data_parse_function = parse_lo)
  req <- lo()

  ## page through list if necessary
  if(!is.null(attr(req, "nextPageToken"))){
    npt <- attr(req, "nextPageToken")

    lo2 <- googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/",
                                         path_args = list(b = bucket,
                                                          o = ""),
                                         pars_args = list(pageToken = npt),
                                         data_parse_function = parse_lo)


    while(!is.null(npt)){
      myMessage("Paging through results: ", npt, level = 3)
      more_req <- lo2(pars_arguments = npt)
      npt <- attr(more_req, "nextPageToken")
      req <- rbind(req, more_req)
    }
  }



  out_names <- switch(detail,
      summary = c("name", "size", "updated"),
      more = c("name", "size", "bucket", "contentType", "timeCreated", "updated", "storageClass"),
      full = TRUE
                )

  req[,out_names]


}

# Parse gs:// URIs to bucket and name
gcs_parse_gsurls <- function(gsurl){
  testthat::expect_type(gsurl, "character")

  if(grepl("^gs://", gsurl)){
    ## parse out bucket and object name
    bucket <- gsub("^gs://(.+?)/(.+)$","\\1", gsurl)
    obj <- gsub(paste0("^gs://",bucket,"/"), "", gsurl)

    out <- list(bucket = bucket, obj = obj)
  } else {
    # not a gs:// URL
    out <- NULL
  }

  out

}

#' Get an object in a bucket directly
#'
#' This retrieves an object directly.
#'
#'
#' @param object_name name of object in the bucket that will be URL encoded, or a \code{gs://} URL
#' @param bucket bucket containing the objects. Not needed if using a \code{gs://} URL
#' @param meta If TRUE then get info about the object, not the object itself
#' @param saveToDisk Specify a filename to save directly to disk.
#' @param parseObject If saveToDisk is NULL, whether to parse with \link{gcs_parse_download}
#'
#' @details
#'
#' This differs from providing downloads via a download link as you can
#'   do via \link{gcs_download_url}
#'
#' For \code{object_name} you can use a \code{gs://} URI instead,
#' in which case it will take the bucket name from that URI and \code{bucket} argument
#' will be overridden.  The URLs should be in the form \code{gs://bucket/object/name}
#'
#' @return The object, or TRUE if sucessfully saved to disk.
#'
#' @family object functions
#' @export
gcs_get_object <- function(object_name,
                           bucket = gcs_get_global_bucket(),
                           meta = FALSE,
                           saveToDisk = NULL,
                           parseObject = TRUE){

  testthat::expect_type(bucket, "character")
  testthat::expect_type(object_name, "character")
  testthat::expect_length(bucket, 1)
  testthat::expect_length(object_name, 1)

  parse_gsurl <- gcs_parse_gsurls(object_name)
  if(!is.null(parse_gsurl)){
    object_name <- parse_gsurl$obj
    bucket <- parse_gsurl$bucket
  }

  object_name <- URLencode(object_name, reserved = TRUE)

  if(meta){
    alt = ""
  } else {
    options(googleAuthR.rawResponse = TRUE)
    on.exit(options(googleAuthR.rawResponse = FALSE))
    alt = "media"
  }

  ob <- googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/",
                                       path_args = list(b = bucket,
                                                        o = object_name),
                                       pars_args = list(alt = alt))
  req <- ob()


  if(!meta){
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
    out <- structure(req$content, class = "gcs_objectmeta")
  }


  out

}

#' Make metadata for an object
#'
#' Use this to pass to uploads in \link{gcs_upload}
#'
#' @inheritParams Object
#' @param object_name Name of the object. GCS uses this version if also set elsewhere.
#'
#' @return Object metadata for uploading of class \code{gar_Object}
#' @family object functions
#' @export
gcs_metadata_object <- function(object_name = NULL,
                                metadata = NULL,
                                md5Hash = NULL,
                                crc32c = NULL,
                                contentLanguage = NULL,
                                contentEncoding = NULL,
                                contentDisposition = NULL,
                                cacheControl = NULL){

  parse_gsurl <- gcs_parse_gsurls(object_name)
  if(!is.null(parse_gsurl)){
    object_name <- parse_gsurl$obj
    # bucket <- parse_gsurl$bucket
  }

  object_name <- if(!is.null(object_name)) URLencode(object_name, reserved = TRUE)

  out <- Object(name = object_name,
                metadata = metadata,
                md5Hash = md5Hash,
                crc32c = crc32c,
                contentLanguage = contentLanguage,
                contentEncoding = contentEncoding,
                contentDisposition = contentDisposition,
                cacheControl = cacheControl)

  out


}

#' Delete an object
#'
#' Deletes an object from a bucket
#'
#' @param object_name Object to be deleted
#' @param bucket Bucket to delete object from
#' @param generation If present, deletes a specific version.
#'
#' Default if \code{generation} is NULL is to delete the latest version.
#'
#' @return If successful, TRUE.
#' @family object functions
#' @export
gcs_delete_object <- function(object_name,
                              bucket = gcs_get_global_bucket(),
                              generation = NULL){
  testthat::expect_type(bucket, "character")
  testthat::expect_type(object_name, "character")
  testthat::expect_length(bucket, 1)
  testthat::expect_length(object_name, 1)

  parse_gsurl <- gcs_parse_gsurls(object_name)
  if(!is.null(parse_gsurl)){
    object_name <- parse_gsurl$obj
    bucket <- parse_gsurl$bucket
  }

  object_name <- URLencode(object_name, reserved = TRUE)

  pars <- list(generation = generation)
  pars <- rmNullObs(pars)

  ob <- googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/",
                                       "DELETE",
                                       path_args = list(b = bucket,
                                                        o = object_name),
                                       pars_args = pars)

  ## suppress warnings of no JSON content detected
  suppressWarnings(ob())

  myMessage("Deleted '", object_name, "' from bucket '", bucket,"'")
  TRUE

}

#' Object Object
#'
#' @details
#' An object.
#'
#' @param acl Access controls on the object
#' @param bucket The name of the bucket containing this object
#' @param cacheControl Cache-Control directive for the object data
#' @param componentCount Number of underlying components that make up this object
#' @param contentDisposition Content-Disposition of the object data
#' @param contentEncoding Content-Encoding of the object data
#' @param contentLanguage Content-Language of the object data
#' @param contentType Content-Type of the object data
#' @param crc32c CRC32c checksum, as described in RFC 4960, Appendix B; encoded using base64 in big-endian byte order
#' @param customerEncryption Metadata of customer-supplied encryption key, if the object is encrypted by such a key
#' @param etag HTTP 1
#' @param generation The content generation of this object
#' @param id The ID of the object
#' @param md5Hash MD5 hash of the data; encoded using base64
#' @param mediaLink Media download link
#' @param metadata User-provided metadata, in key/value pairs
#' @param metageneration The version of the metadata for this object at this generation
#' @param name The name of this object
#' @param owner The owner of the object
#' @param selfLink The link to this object
#' @param size Content-Length of the data in bytes
#' @param storageClass Storage class of the object
#' @param timeCreated The creation time of the object in RFC 3339 format
#' @param timeDeleted The deletion time of the object in RFC 3339 format
#' @param updated The modification time of the object metadata in RFC 3339 format
#'
#' @return Object object
#'
#' @family Object functions
Object <- function(acl = NULL,
                   bucket = NULL,
                   cacheControl = NULL,
                   componentCount = NULL,
                   contentDisposition = NULL,
                   contentEncoding = NULL,
                   contentLanguage = NULL,
                   contentType = NULL,
                   crc32c = NULL,
                   customerEncryption = NULL,
                   etag = NULL,
                   generation = NULL,
                   id = NULL,
                   md5Hash = NULL,
                   mediaLink = NULL,
                   metadata = NULL,
                   metageneration = NULL,
                   name = NULL,
                   owner = NULL,
                   selfLink = NULL,
                   size = NULL,
                   storageClass = NULL,
                   timeCreated = NULL,
                   timeDeleted = NULL,
                   updated = NULL) {
  structure(list(acl = acl,
                 bucket = bucket,
                 cacheControl = cacheControl,
                 componentCount = componentCount,
                 contentDisposition = contentDisposition,
                 contentEncoding = contentEncoding,
                 contentLanguage = contentLanguage,
                 contentType = contentType,
                 crc32c = crc32c,
                 customerEncryption = customerEncryption,
                 etag = etag,
                 generation = generation,
                 id = id,
                 kind = "storage#object",
                 md5Hash = md5Hash,
                 mediaLink = mediaLink,
                 metadata = metadata,
                 metageneration = metageneration,
                 name = name,
                 owner = owner,
                 selfLink = selfLink,
                 size = size,
                 storageClass = storageClass,
                 timeCreated = timeCreated,
                 timeDeleted = timeDeleted, updated = updated), class = "gar_Object")
}
