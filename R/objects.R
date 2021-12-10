#' List objects in a bucket
#'
#' @param bucket bucket containing the objects
#' @param detail Set level of detail
#' @param prefix Filter results to objects whose names begin with this prefix
#' @param delimiter Use to list objects like a directory listing.
#' @param versions If \code{TRUE}, lists all versions of an object as distinct results in order of increasing generation number. 
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
#'  \code{delimited} returns results in a directory-like mode: items will contain only objects whose names,
#'     aside from the prefix, do not contain delimiter. In conjunction with the prefix filter,
#'     the use of the delimiter parameter allows the list method to operate like a directory listing,
#'     despite the object namespace being flat.
#'     For example, if delimiter were set to \code{"/"}, then listing objects from a bucket that contains the
#'     objects \code{"a/b", "a/c", "dddd", "eeee", "e/f"} would return objects \code{"dddd" and "eeee"},
#'     and prefixes \code{"a/" and "e/"}.
#'
#'
#' @return A data.frame of the objects
#'
#' @family object functions
#' @export
#' @importFrom googleAuthR gar_api_generator gar_api_page
gcs_list_objects <- function(bucket = gcs_get_global_bucket(),
                             detail = c("summary","more","full"),
                             prefix = NULL,
                             delimiter = NULL,
                             versions = FALSE){

  detail <- match.arg(detail)

  bucket <- as.bucket_name(bucket)

  pars <- list(prefix = prefix,
               delimiter = delimiter,
               pageToken = "",
               versions = versions)
  pars <- rmNullObs(pars)

  lo <- gar_api_generator("https://www.googleapis.com/storage/v1/",
                          path_args = list(b = bucket,
                                           o = ""),
                          pars_args = pars,
                          data_parse_function = parse_lo)
  
  req <- gar_api_page(lo, 
                      page_f = function(x) attr(x, "nextPageToken"),
                      page_method = "param", 
                      page_arg = "pageToken")
  
  limit_columns(my_reduce_rbind(req), detail = detail)
}

limit_columns <- function(req, detail){
  
  if(nrow(req) == 0){
    return(data.frame())
  }
  
  out_names <- switch(detail,
                      summary = c("name", "size", "updated"),
                      more = c("name", "size", "bucket", "contentType",
                               "timeCreated", "updated", "storageClass"),
                      full = TRUE)
  
  req[,out_names]
}

## parse
parse_lo <- function(x){
  nextPageToken <- x$nextPageToken
  if(is.null(x$items)){
    myMessage("No objects found", level = 3)
    return(data.frame())
  }
  x <- x$items
  if(is.null(x$prefixes)){
    myMessage("No prefixes found", level = 2)
    prefixes <- NULL
  } else {
    prefixes <- x$prefixes
  }
  x$timeCreated <- timestamp_to_r(x$timeCreated)
  x$updated <- timestamp_to_r(x$updated)
  x$kind <- NULL
  x$size <- vapply(as.numeric(x$size), function(x) format_object_size(x, "auto"), character(1))

  ## extra columns for composite objects (#73)
  x$componentCount  <- if(is.null(x$componentCount)) NA else x$componentCount
  x$contentLanguage <- if(is.null(x$contentLanguage)) NA else x$contentLanguage

  attr(x, "nextPageToken") <- nextPageToken
  attr(x, "prefixes") <- prefixes
  attr(x, "metadata") <- x$metadata
  
  x$metadata <- NULL
  
  x
}

# Parse gs:// URIs to bucket and name
gcs_parse_gsurls <- function(gsurl){

  assertthat::assert_that(is.character(gsurl))

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
#' @param saveToDisk Specify a filename to save directly to disk
#' @param overwrite If saving to a file, whether to overwrite it
#' @param parseObject If saveToDisk is NULL, whether to parse with \code{parseFunction}
#' @param generation The generation number for the noncurrent version, if you have object versioning enabled in your bucket e.g. \code{"1560468815691234"}
#' @param parseFunction If saveToDisk is NULL, the function that will parse the download.  Defaults to \link{gcs_parse_download}
#'
#' @details
#'
#' This differs from providing downloads via a download link as you can
#'   do via \link{gcs_download_url}
#'
#' \code{object_name} can use a \code{gs://} URI instead,
#' in which case it will take the bucket name from that URI and \code{bucket} argument
#' will be overridden.  The URLs should be in the form \code{gs://bucket/object/name}
#'
#' By default if you want to get the object straight into an R session the parseFunction is \link{gcs_parse_download} which wraps \code{httr}'s \link[httr]{content}.
#'
#' If you want to use your own function (say to unzip the object) then supply it here.  The first argument should take the downloaded object.
#'
#' @return The object, or TRUE if successfully saved to disk.
#'
#' @examples
#'
#' \dontrun{
#'
#' ## something to download
#' ## data.frame that defaults to be called "mtcars.csv"
#' gcs_upload(mtcars)
#'
#' ## get the mtcars csv from GCS, convert it to an R obj
#' gcs_get_object("mtcars.csv")
#'
#' ## get the mtcars csv from GCS, save it to disk
#' gcs_get_object("mtcars.csv", saveToDisk = "mtcars.csv")
#'
#'
#' ## default gives a warning about missing column name.
#' ## custom parse function to suppress warning
#' f <- function(object){
#'   suppressWarnings(httr::content(object, encoding = "UTF-8"))
#' }
#'
#' ## get mtcars csv with custom parse function.
#' gcs_get_object("mtcars.csv", parseFunction = f)
#' 
#' ## download an RDS file using helper gcs_parse_rds()
#' 
#' gcs_get_object("obj.rds", parseFunction = gcs_parse_rds)
#' 
#' ## to download from a folder in your bucket
#' my_folder <- "your_folder/"
#' objs <- gcs_list_objects(prefix = my_folder)
#' 
#' dir.create(my_folder)
#' 
#' # download all the objects to that folder
#' dls <- lapply(objs$name, function(x) gcs_get_object(x, saveToDisk = x))
#' 

#' 
#' }
#'
#' @family object functions
#' @importFrom utils URLdecode
#' @import assertthat
#' @importFrom httr write_disk
#' @importFrom googleAuthR gar_api_generator
#' @export
gcs_get_object <- function(object_name,
                           bucket = gcs_get_global_bucket(),
                           meta = FALSE,
                           saveToDisk = NULL,
                           overwrite = FALSE,
                           parseObject = TRUE,
                           parseFunction = gcs_parse_download,
                           generation = NULL){

  assert_that(
    is.string(object_name),
    is.flag(meta),
    is.flag(parseObject)
  )

  parse_gsurl <- gcs_parse_gsurls(object_name)
  if(!is.null(parse_gsurl)){
    object_name <- parse_gsurl$obj
    bucket <- parse_gsurl$bucket
  }
  
  if(!is.null(generation)){
    assert_that(is.string(generation))
  }

  bucket <- as.bucket_name(bucket)

  object_name <- URLencode(object_name, reserved = TRUE)

  if(meta){
    alt = "json"
  } else {
    options(googleAuthR.rawResponse = TRUE)
    on.exit(options(googleAuthR.rawResponse = FALSE), add = TRUE)
    alt = "media"
  }

  ## download directly to disk
  if(!is.null(saveToDisk)){

    assert_that(is.logical(overwrite))

    customConfig <- list(write_disk(saveToDisk, overwrite = overwrite))
  } else {
    customConfig <- NULL
  }
  
  cli::cli_process_start(paste("Downloading", URLdecode(object_name)))
  
  pars_args <- list(alt = alt,
                    generation = generation)
  pars_args <- rmNullObs(pars_args)

  ob <- gar_api_generator("https://www.googleapis.com/storage/v1/",
                          path_args = list(b = bucket,
                                           o = object_name),
                          pars_args = pars_args,
                          customConfig = customConfig)
  req <- ob()
  
  if(req$status_code == 404){
    stop("File not found. Check object_name and if you have read permissions.
           Looked for ", object_name, call. = FALSE)
  }

  if(meta){
    cli::cli_process_done(
      msg_done = paste("Metadata for",URLdecode(object_name)))
    return(structure(req$content, class = "gcs_objectmeta"))
  }
  
  if(!is.null(saveToDisk)){
    
    msg <- paste("Saved", URLdecode(object_name), "to", saveToDisk,
                 " (",format_object_size(file.size(saveToDisk), "auto"),")")
    
    cli::cli_process_done(msg_done = msg)
    
    return(TRUE)
    
  }
    
  cli::cli_status_update("Downloaded - parsing object")
  
  out <- req
  
  if(parseObject){
    out <- tryCatch(
      parseFunction(req),
      error = function(err){
        stop("Problem parsing the object with supplied parseFunction.", 
             call. = FALSE)
      })
  }
  
  cli::cli_process_done(msg_done = paste("Downloaded and parsed",
                                         URLdecode(object_name),
                                         "into R object of class:", 
                                         paste(class(out), collapse = ", ")))
  
  out
  
}

#' Make metadata for an object
#'
#' Use this to pass to uploads in \link{gcs_upload}
#'
#' @inheritParams Object
#' @param object_name Name of the object. GCS uses this version if also set elsewhere, or a \code{gs://} URL
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
  }

  object_name <- if(!is.null(object_name)) URLencode(object_name)

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
#' @param object_name Object to be deleted, or a \code{gs://} URL
#' @param bucket Bucket to delete object from
#' @param generation If present, deletes a specific version.
#'
#' Default if \code{generation} is NULL is to delete the latest version.
#'
#' @return If successful, TRUE.
#' @family object functions
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
#' @export
#' @seealso To delete all objects in a bucket see \link{gcs_delete_bucket_objects}
gcs_delete_object <- function(object_name,
                              bucket = gcs_get_global_bucket(),
                              generation = NULL){

  assert_that(
    is.string(object_name)
  )

  parse_gsurl <- gcs_parse_gsurls(object_name)
  if(!is.null(parse_gsurl)){
    object_name <- parse_gsurl$obj
    bucket <- parse_gsurl$bucket
  }

  bucket <- as.bucket_name(bucket)

  object_name <- URLencode(object_name, reserved = TRUE)

  pars <- list(generation = generation)
  pars <- rmNullObs(pars)

  ob <- gar_api_generator("https://www.googleapis.com/storage/v1/",
                          "DELETE",
                          path_args = list(b = bucket,
                                           o = object_name),
                          pars_args = pars)

  ## suppress warnings of no JSON content detected
  suppressWarnings(ob())

  myMessage("Deleted '", object_name, "' from bucket '", bucket,"'")
  TRUE

}

#' Copy an object
#' 
#' Copies an object to a new destination
#' 
#' @param source_object The name of the object to copy, or a \code{gs://} URL
#' @param destination_object The name of where to copy the object to, or a \code{gs://} URL
#' @param source_bucket The bucket of the source object
#' @param destination_bucket The bucket of the destination
#' @param rewriteToken Include this field (from the previous rewrite response) on each rewrite request after the first one, until the rewrite response 'done' flag is true.
#' @param destinationPredefinedAcl Apply a predefined set of access controls to the destination object. If not NULL must be one of the predefined access controls such as \code{"bucketOwnerFullControl"}
#' 
#' @return If successful, a rewrite object.
#' @family object functions
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
#' @export
gcs_copy_object <- function(source_object,
                            destination_object,
                            source_bucket = gcs_get_global_bucket(),
                            destination_bucket = gcs_get_global_bucket(),
                            rewriteToken = NULL,
                            destinationPredefinedAcl = NULL){
  
  assert_that(
    is.string(source_object),
    is.string(destination_object)
  )
  
  source_gcs <- gcs_parse_gsurls(source_object)
  if(!is.null(source_gcs)){
    source_object <- source_gcs$obj
    source_bucket <- source_gcs$bucket
  }
  destination_gcs <- gcs_parse_gsurls(destination_object)
  if(!is.null(destination_gcs)){
    destination_object <- destination_gcs$obj
    destination_bucket <- destination_gcs$bucket
  }
  
  source_bucket <- as.bucket_name(source_bucket)
  destination_bucket <- as.bucket_name(destination_bucket)

  source_object <- URLencode(source_object, reserved = TRUE)
  destination_object <- URLencode(destination_object, reserved = TRUE)
  
  the_url <- sprintf("https://www.googleapis.com/storage/v1/b/%s/o/%s/rewriteTo/b/%s/o/%s", 
                     source_bucket, source_object, destination_bucket, destination_object)
  pars <- NULL
  if(!is.null(rewriteToken)){
    pars <- list(rewriteToken = rewriteToken)
  }
  if(!is.null(destinationPredefinedAcl)){
    assert_that(is.string(destinationPredefinedAcl))
    pars <- c(pars, list(destinationPredefinedAcl = destinationPredefinedAcl))
  }
  ob <- gar_api_generator(the_url,
                          "POST",
                          pars_args = pars,
                          data_parse_function = function(x) x)
  ob()
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
#' @keywords internal
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
  structure(rmNullObs(list(acl = acl,
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
                 timeDeleted = timeDeleted, updated = updated)), class = "gar_Object")
}
