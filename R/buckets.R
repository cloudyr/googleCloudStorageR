## store bucket name
.gcs_env <- new.env(parent = emptyenv())

#' Set global bucket name
#'
#' Set a bucket name used for this R session
#'
#' @param bucket bucket name you want this session to use by default
#'
#' @details
#'   This sets a bucket to a global environment value so you don't need to
#' supply the bucket argument to other API calls.
#'
#' @return The bucket name (invisibly)
#'
#' @family bucket functions
#' @export
gcs_global_bucket <- function(bucket){
  stopifnot(inherits(bucket, "character"),
            length(bucket) == 1)

  .gcs_env$bucket <- bucket
  message("Set default bucket name to '", bucket,"'")
  return(invisible(.gcs_env$bucket))

}

#' Get global bucket name
#'
#' Bucket name set this session to use by default
#'
#' @return Bucket name
#'
#' @details
#'   Set the bucket name via \link{gcs_global_bucket}
#'
#' @family bucket functions
#' @export
gcs_get_global_bucket <- function(){

  if(!exists("bucket", envir = .gcs_env)){
    stop("Bucket is NULL and couldn't find global bucket name.
         Set it via gcs_global_bucket")
  }

  .gcs_env$bucket

}

#' List buckets
#'
#' List the buckets your projectId has access to
#'
#' @param projectId Project containing buckets to list
#' @param prefix Filter results to names begnning with this prefix
#' @param projection Properties to return. Default noAcl omits acl properties
#' @param maxResults Max number of results
#' @param detail Set level of detail
#'
#' @details
#'
#' Columns returned by \code{detail} are:
#'
#' \itemize{
#'   \item \code{summary} - name, storageClass, location ,updated
#'   \item \code{full} - as above plus: id, selfLink, projectNumber, timeCreated, metageneration, etag
#'  }
#'
#' @return data.frame of buckets
#'
#' @family bucket functions
#' @export
gcs_list_buckets <- function(projectId,
                             prefix = "",
                             projection = c("noAcl","full"),
                             maxResults = 1000,
                             detail = c("summary","full")){

  projection <- match.arg(projection)
  detail <- match.arg(detail)

  testthat::expect_is(projectId, "character")
  testthat::expect_is(prefix, "character")
  testthat::expect_is(maxResults, "numeric")

  parse_lb <- function(x){
    x <- x$items
    x$kind <- NULL
    x$timeCreated <- timestamp_to_r(x$timeCreated)
    x$updated <- timestamp_to_r(x$updated)

    x
  }

  lb <-
    googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/b",
                                   "GET",
                                   pars_args = list(project=projectId,
                                                    prefix=prefix,
                                                    projection=projection),
                                   data_parse_function = parse_lb)

  out <- lb()

  out_names <- switch(detail,
                      summary = c("name", "storageClass", "location" ,"updated"),
                      full = TRUE
  )

  out[,out_names]

}

#' Get bucket info
#'
#' Meta data about the bucket
#'
#' @param bucket Name of a bucket
#' @param ifMetagenerationMatch Return only if metageneration matches
#' @param ifMetagenerationNotMatch Return only if metageneration does not match
#' @param projection Properties to return. Default noAcl omits acl properties
#'
#' @return A bucket resource object
#' @family bucket functions
#' @export
gcs_get_bucket <- function(bucket = gcs_get_global_bucket(),
                           ifMetagenerationMatch = NULL,
                           ifMetagenerationNotMatch = NULL,
                           projection = c("noAcl","full")){

  projection <- match.arg(projection)

  testthat::expect_is(bucket, "character")
  testthat::expect_is(projection, "character")

  pars_args <- list(ifMetagenerationMatch=ifMetagenerationMatch,
                    ifMetagenerationNotMatch=ifMetagenerationNotMatch,
                    projection=projection)
  pars_args <- rmNullObs(pars_args)

  bb <-
    googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/",
                                   "GET",
                                   path_args = list(b = bucket),
                                   pars_args = pars_args)
  req <- bb()

  structure(req$content, class = "gcs_bucket")
}

#' Create a new bucket
#'
#' Create a new bucket in your project
#'
#' @param name Globally unique name of bucket to create
#' @param projectId A valid Google project id
#' @param location Location of bucket. See details
#' @param storageClass Type of bucket
#' @param predefinedAcl Apply predefined access controls to bucket
#' @param predefinedDefaultObjectAcl Apply predefined access controls to objects
#' @param projection Properties to return. Default noAcl omits acl properties
#'
#' @details
#'   \href{https://cloud.google.com/storage/docs/bucket-locations}{See here for details on location options}
#'
#' todo: acl, cors, lifecycle, logging, versioning, website
#'
#' @family bucket functions
#' @export
gcs_create_bucket <-
  function(name,
           projectId,
           location = "US",
           storageClass = c("STANDARD",
                            "NEARLINE",
                            "DURABLE_REDUCED_AVAILABILITY"),
           predefinedAcl = c("authenticatedRead",
                             "private",
                             "projectPrivate",
                             "publicRead",
                             "publicReadWrite"),
           predefinedDefaultObjectAcl = c("authenticatedRead",
                                          "private",
                                          "projectPrivate",
                                          "publicRead",
                                          "publicReadWrite"),
           projection = c("noAcl","full")){

  projection    <- match.arg(projection)
  predefinedAcl <- match.arg(predefinedAcl)
  storageClass  <- match.arg(storageClass)
  predefinedDefaultObjectAcl <- match.arg(predefinedDefaultObjectAcl)

  testthat::expect_is(projectId, "character")
  testthat::expect_is(name, "character")
  testthat::expect_is(location, "character")
  testthat::expect_is(projection, "character")

  pars_args <- list(project = projectId,
                    predefinedAcl = predefinedAcl,
                    predefinedDefaultObjectAcl = predefinedDefaultObjectAcl,
                    projection = projection)
  pars_args <- rmNullObs(pars_args)

  bb <-
    googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/b",
                                   "POST",
                                   pars_args = pars_args)

  body <- list(
    name = name,
    location = location,
    storageClass = storageClass
  )

  body <- rmNullObs(body)

  req <- bb(the_body = body)

  if(req$status_code == 200){
    myMessage("Bucket created successfully:", name, " in ", location, level = 3)
    out <- structure(req$content, class = "gcs_bucket")
  } else {
    myMessage("Bucket creation failed:", name, " in ", location, level = 3)
    out <- FALSE
  }

  out

}


#' Delete a bucket
#'
#' Delete the bucket, and all its objects
#'
#' @param bucket Name of the bucket
#' @param ifMetagenerationMatch Delete only if metageneration matches
#' @param ifMetagenerationNotMatch Delete only if metageneration does not match
#'
#' @import testthat
#' @family bucket functions
#' @export
gcs_delete_bucket <- function(bucket,
                              ifMetagenerationMatch = NULL,
                              ifMetagenerationNotMatch = NULL){

  testthat::expect_is(bucket, "character")

  pars_args <- list(ifMetagenerationMatch=ifMetagenerationMatch,
                    ifMetagenerationNotMatch=ifMetagenerationNotMatch)
  pars_args <- rmNullObs(pars_args)

  bb <-
    googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/",
                                   "DELETE",
                                   path_args = list(b = bucket),
                                   pars_args = pars_args)

  ## suppress warnings of no JSON content detected
  res <- suppressWarnings(bb())

  if(res$status_code == "204"){
    myMessage("Bucket ", bucket, " deleted successfully.", level = 3)
    out <- TRUE
  } else {
    myMessage("Bucket ", bucket, " NOT deleted", level = 3)
    out <- FALSE
  }

  out

}


#' Update a bucket
#'
#' Update a buckets metadata using PATCH semantics
#'
#' @param bucket Name of the bucket
#' @param ifMetagenerationMatch Return only if metageneration matches
#' @param ifMetagenerationNotMatch Return only if metageneration does not match
#' @param predefinedAcl Apply predefined access controls to bucket
#' @param predefinedDefaultObjectAcl Apply predefined access controls to objects
#' @param projection Properties to return. Default noAcl omits acl properties
#'
#' @details
#'   \href{https://cloud.google.com/storage/docs/bucket-locations}{See here for details on location options}
#'
#' todo: acl, cors, lifecycle, logging, versioning, website
#'
#' @import testthat
#' @family bucket functions
#' @export
gcs_update_bucket <-
  function(bucket = gcs_get_global_bucket(),
           ifMetagenerationMatch = NULL,
           ifMetagenerationNotMatch = NULL,
           predefinedAcl = NULL,
           predefinedDefaultObjectAcl = NULL,
           projection = c("noAcl","full")){

    projection    <- match.arg(projection)

    if(!is.null(predefinedAcl)){
      stopifnot(predefinedAcl %in% c("authenticatedRead",
                                     "private",
                                     "projectPrivate",
                                     "publicRead",
                                     "publicReadWrite"))
    }

    if(!is.null(predefinedDefaultObjectAcl)){
      stopifnot(predefinedDefaultObjectAcl %in% c("authenticatedRead",
                                                  "private",
                                                  "projectPrivate",
                                                  "publicRead",
                                                  "publicReadWrite"))

    }

    testthat::expect_is(bucket, "character")

    pars_args <- list(ifMetagenerationMatch=ifMetagenerationMatch,
                      ifMetagenerationNotMatch=ifMetagenerationNotMatch,
                      predefinedAcl = predefinedAcl,
                      predefinedDefaultObjectAcl = predefinedDefaultObjectAcl,
                      projection = projection)
    pars_args <- rmNullObs(pars_args)

    bb <-
      googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/",
                                     "PATCH",
                                     path_args = list(b = bucket),
                                     pars_args = pars_args)

    body <- list(

    )

    body <- rmNullObs(body)

    req <- bb(the_body = body)

    if(req$status_code == 200){
      myMessage("Bucket update successfully", level = 3)
      out <- structure(req$content, class = "gcs_bucket")
    } else {
      myMessage("Bucket update failed", level = 3)
      out <- FALSE
    }

    out

  }
