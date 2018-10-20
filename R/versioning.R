#' Turn bucket versioning on or off, check status (default), or
#' list both live and archived versions of objects in the bucket
#' @param action "status", "enable", "disable", or "list"
#' @param bucket gcs bucket
#'
#' @return versioned_objects list #only if "list" action
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
gcs_version_bucket <- function(bucket, action = "status") {
  
  # support the case where user enters bucket with gs:// prefix
  bucket <- gsub("^gs://", "", bucket)
  # check that it is a valid bucket that user has access to
  tryCatch({
    gcs_get_bucket(bucket)
    # cat(sprintf("\n \"%s\" is a valid bucket.\n", bucket))
  }, error = function(e) {
    e
    stop("Bucket not found. Check bucket name and if you have read permissions.
    Looked for ", bucket)
  })
  
  # options(googleAuthR.verbose = 0)
  # will only be different for list
  url <- sprintf(
    "https://www.googleapis.com/storage/v1/b/%s",
    bucket
  )
  pars_args <- list(fields = "versioning")
  
  # Default is to check status
  if (action == "status") {
    
    api <- googleAuthR::gar_api_generator(url,
                                          "GET",
                                          pars_args = pars_args,
                                          data_parse_function = function(x) x,
                                          checkTrailingSlash = FALSE
    )
    
    if (length(api()) == 0) { #If versioning has never been enabled before
      cat(sprintf("Versioning is NOT ENABLED for \"%s\"", bucket))
    } else {
      if (api()$versioning$enabled == TRUE) {
        cat(sprintf("Versioning is ENABLED for \"%s\"", bucket))
      }
      else {
        cat(sprintf("Versioning is NOT ENABLED for \"%s\"", bucket))
      }
    }
  }
  # on or off
  else if (action %in% c("enable","disable")) {
    
    trueorfalse <- ifelse(action == "enable", "true", "false")
    
    #https://stackoverflow.com/questions/42356834/how-to-post-multipart-binary-non-binary-with-httr-for-salesforce-api
    # x <- as.character(jsonlite::toJSON(sprintf('{
    #   "versioning": {
    #     "enabled": %s
    #   }
    # }', trueorfalse)))
    # 
    # metadata <- curl::form_data(x,"application/json")
    
    #body <- list(metadata = metadata)
    
    #body = list(y = upload_file(metadata))
    
    body <- list(x = jsonlite::toJSON(sprintf('{
      "versioning": {
        "enabled": %s
      }
    }', trueorfalse)))
    
    # body <- list(y = upload_file(system.file('{
    #   "versioning": {
    #     "enabled": true
    #   }
    # }')))
    # 
    api <- googleAuthR::gar_api_generator(url,
                                          "PATCH",
                                          pars_args = pars_args,
                                          data_parse_function = function(x) x,
                                          checkTrailingSlash = FALSE,
                                          customConfig = list(httr::add_headers("Content-Type" = "application/json"),
                                                              encode = "json")
    )
    
    api(the_body = body)
    
  }
  
  #TO DO: on, off, or list
}