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

  assertthat::assert_that(
    is.character(bucket),
    is.character(object_name)
  )

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

create_signature <- function(path,
                             json_key,
                             my_content_type,
                             expiration_ts,
                             verb = "GET",
                             # extension_headers = "",
                             md5hash = ""){

  assertthat::assert_that(
    is.character(json_key),
    as.numeric(Sys.time()) < as.numeric(expiration_ts),
    is.character(path),
    is.unit(path),
    is.character(verb),
    is.unit(verb),
    is.character(my_content_type),
    is.unit(my_content_type)
  )

  sig_string <- paste(verb,
                      md5hash,
                      my_content_type,
                      as.numeric(expiration_ts),
                      # extension_headers,
                      path,
                      sep = "\n")

  if(getOption("googleAuthR.verbose") < 3){
    myMessage("StringToSign\n", level = 2)
    print(sig_string)
    myMessage("--", level = 2)
  }


  curl::curl_escape(openssl::base64_encode(openssl::signature_create(data=charToRaw(sig_string),
                                                                     key=json_key,
                                                                     hash=openssl::sha256)))
}

#' Create a signed URL
#'
#' Create a URL with a time-limited read and write to an object, regardless whether they have a Google account
#'
#' @param meta_obj A meta object from \link{gcs_get_object}
#' @param expiration_ts A timestamp of class \code{"POSIXct"} such as from \code{Sys.time()} or a numeric in seconds from Unix Epoch.  Default is 60 mins.
#' @param verb The URL verb of access e.g. \code{GET} or \code{PUT}. Default \code{GET}
#' @param md5hash An optional md5 digest value
#' @param includeContentType For getting the URL via browsers this should be set to \code{FALSE} (the default).  Otherwise, set to \code{TRUE} to include the content type of the object in the request needed.
#'
#' @seealso \url{https://cloud.google.com/storage/docs/access-control/signed-urls}
#'
#' @description
#'
#' This creates a signed URL which you can share with others who may or may not have a Google account.
#' The object will be available until the specified timestamp.
#'
#' @examples
#'
#' \dontrun{
#'
#' obj <- gcs_get_object("your_file", meta = TRUE)
#'
#' signed <- gcs_signed_url(obj)
#'
#' temp <- tempfile()
#' on.exit(unlink(temp))
#'
#' download.file(signed, destfile = temp)
#' file.exists(temp)
#'
#' }
#'
#' @export
#' @family download functions
gcs_signed_url <- function(meta_obj,
                           expiration_ts = Sys.time() + 3600,
                           verb = "GET",
                           md5hash = NULL,
                           includeContentType = FALSE){

  assertthat::assert_that(
    inherits(meta_obj, "gcs_objectmeta"),
    assertthat::is.readable(Sys.getenv("GCS_AUTH_FILE"))
  )

  my_content_type <- if(includeContentType) meta_obj$contentType else ""

  json_file <- jsonlite::fromJSON(Sys.getenv("GCS_AUTH_FILE"))

  sig <- create_signature(paste0("/",meta_obj$bucket, "/", meta_obj$name),
                          json_key = json_file$private_key,
                          my_content_type = my_content_type,
                          expiration_ts = round(as.numeric(expiration_ts)),
                          verb = verb,
                          md5hash = md5hash)

  dl_url <- gcs_download_url(meta_obj$name, public = TRUE)

  sprintf("%s?GoogleAccessId=%s&Expires=%s&Signature=%s",
          dl_url, json_file$client_email, round(as.numeric(expiration_ts)), sig)

}


