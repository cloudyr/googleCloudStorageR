#' Change or fetch bucket version status
#' 
#' @description 
#' 
#' Turn bucket versioning on or off, check status (default), or
#' list archived versions of objects in the bucket and view their generation numbers.
#'
#' @param action "status", "enable", "disable", or "list"
#' @param bucket gcs bucket
#'
#' @return If \code{action="list"} a versioned_objects dataframe
#'         If \code{action="status"} a boolean on if versioning is TRUE or FALSE
#'         If \code{action="enable" or "disable"} TRUE if operation is successful
#' @importFrom googleAuthR gar_api_generator
#' @export
#' 
#' @examples 
#' 
#' \dontrun{

#'   buck <- gcs_get_global_bucket()
#'   gcs_version_bucket(buck, action = "disable")
#'   
#'   gcs_version_bucket(buck, action = "status")
#'   # Versioning is NOT ENABLED for "your-bucket"
#'   gcs_version_bucket(buck, action = "enable")
#'   # TRUE
#'   gcs_version_bucket(buck, action = "status")
#'   # Versioning is ENABLED for "your-bucket"
#'   gcs_version_bucket(buck, action = "list")
#' 
#' 
#' }
#' 
gcs_version_bucket <- function(bucket, 
                               action = c("status", "enable", "disable", "list")) {
  action <- match.arg(action)

  bucket <- as.bucket_name(bucket)

  # check that it is a valid bucket that user has access to
  tryCatch({
    gcs_get_bucket(bucket)
    # cat(sprintf("\n \"%s\" is a valid bucket.\n", bucket))
  }, error = function(e) {
    stop("Bucket not found. Check bucket name and if you have read permissions.
    Looked for ", bucket, call. = FALSE)
  })

  # will only be different for list
  url <- sprintf(
    "https://www.googleapis.com/storage/v1/b/%s",
    bucket
  )
  pars_args <- list(fields = "versioning")

  # Default is to check status
  if (action == "status") {
    api <- gar_api_generator(url,
      "GET",
      pars_args = pars_args,
      data_parse_function = function(x) {
        if (length(x) == 0) { 
          x 
        } else {
          x$versioning$enabled
        }
      },
      checkTrailingSlash = FALSE
    )

    if (length(api()) == 0) { # If versioning has never been enabled before
      myMessage(sprintf("Versioning is NOT ENABLED for \"%s\"\n", bucket), level = 3)
      return(FALSE)
    } else {
      if (api() == TRUE) {
        myMessage(sprintf("Versioning is ENABLED for \"%s\"\n", bucket), level = 3)
        return(TRUE)
      } else {
        myMessage(sprintf("Versioning is NOT ENABLED for \"%s\"\n", bucket), level = 3)
        return(FALSE)
      }
    }
  }
  # on or off
  else if (action %in% c("enable", "disable")) {

    body <- list(versioning = list(enabled = (action == "enable")))

    api <- gar_api_generator(url,
      "PATCH",
      pars_args = pars_args,
      data_parse_function = function(x) x$versioning$enabled,
      checkTrailingSlash = FALSE
    )

    api(the_body = body)
  }
  else if (action == "list") {
    # To list both live and archived versions of an object and view their generation numbers 
    # (filter only archived versions for final output)
    # Archived versions of objects have a `timeDeleted` property.

    url <- sprintf(
      "https://www.googleapis.com/storage/v1/b/%s/o",
      bucket
    )
    pars_args <- list(versions = "true")

    api <- gar_api_generator(url,
      "GET",
      pars_args = pars_args,
      data_parse_function = function(x) x$items,
      checkTrailingSlash = FALSE
    )
    if (!("timeDeleted" %in% colnames(api()))) {
      stop("`timeDeleted` property not found. No archived versions currently in bucket: ", bucket, call. = FALSE)
    } else {
      api <- api()
      api[!is.na(api$timeDeleted), ]
    }
  }
}
