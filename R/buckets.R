## store bucket name
.gcs_env <- new.env(parent = emptyenv())

#' Is a bucket
#' @noRd
#' @import assertthat
is.gcs_bucket <- function(x){
  assert_that(is.string(x))
  inherits(x, "gcs_bucket")
}

#' Makes a bucket its bucket name or returns the name
#' @noRd
#' @import assertthat
as.bucket_name <- function(x){
  
  if(is.gcs_bucket(x)){
    out <- x$name
  } else if(is.string(x) && x != ""){ 
    x <- gsub("^gs://", "", x) #remove prefix if one exists
    out <- x
  } else {
    stop("Bucket name is invalid", call. = FALSE)
  }

  assert_that(is.string(out))

  out
}

#' Set global bucket name
#'
#' Set a bucket name used for this R session
#'
#' @param bucket bucket name you want this session to use by default, or a bucket object
#'
#' @details
#'   This sets a bucket to a global environment value so you don't need to
#' supply the bucket argument to other API calls.
#'
#' @return The bucket name (invisibly)
#'
#' @family bucket functions
#' @import assertthat
#' @export
gcs_global_bucket <- function(bucket){

  bucket <- as.bucket_name(bucket)

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
#' @param prefix Filter results to names beginning with this prefix
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
#' @return \code{data.frame} of buckets
#'
#' @examples
#'
#' \dontrun{
#'
#' buckets <- gcs_list_buckets("your-project")
#'
#' ## use the name of the bucket to get more meta data
#' bucket_meta <- gcs_get_bucket(buckets$name[[1]])
#'
#' }
#'
#' @family bucket functions
#' @import assertthat
#' @export
gcs_list_buckets <- function(projectId,
                             prefix = "",
                             projection = c("noAcl","full"),
                             maxResults = 1000,
                             detail = c("summary","full")){

  projection <- match.arg(projection)
  detail     <- match.arg(detail)

  assert_that(is.string(projectId),
              is.string(prefix),
              is.count(maxResults))

  parse_lb <- function(x){
    if(is.null(x$items)){
      myMessage("No buckets found in ", projectId, level = 3)
      return(data.frame())
    }
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
  
  if(nrow(out) < 1) return(NULL)

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
#' @param bucket Name of a bucket, or a bucket object returned by \link{gcs_create_bucket}
#' @param ifMetagenerationMatch Return only if metageneration matches
#' @param ifMetagenerationNotMatch Return only if metageneration does not match
#' @param projection Properties to return. Default noAcl omits acl properties
#'
#' @examples
#'
#' \dontrun{
#'
#' buckets <- gcs_list_buckets("your-project")
#'
#' ## use the name of the bucket to get more meta data
#' bucket_meta <- gcs_get_bucket(buckets$name[[1]])
#'
#' }
#'
#' @return A bucket resource object
#' @family bucket functions
#' @import assertthat
#' @export
gcs_get_bucket <- function(bucket = gcs_get_global_bucket(),
                           ifMetagenerationMatch = NULL,
                           ifMetagenerationNotMatch = NULL,
                           projection = c("noAcl","full")){

  projection <- match.arg(projection)

  bucket <- as.bucket_name(bucket)

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
#' @param versioning Set if the bucket supports versioning of its objects
#' @param lifecycle A list of \link{gcs_create_lifecycle} objects
#'
#' @details
#'   \href{https://cloud.google.com/storage/docs/locations}{See here for details on location options}
#'
#' @family bucket functions
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
#' @export
gcs_create_bucket <-
  function(name,
           projectId,
           location = "US",
           storageClass = c("MULTI_REGIONAL",
                            "REGIONAL",
                            "STANDARD",
                            "NEARLINE",
                            "COLDLINE",
                            "DURABLE_REDUCED_AVAILABILITY"),
           predefinedAcl = c("projectPrivate",
                             "authenticatedRead",
                             "private",
                             "publicRead",
                             "publicReadWrite"),
           predefinedDefaultObjectAcl = c("bucketOwnerFullControl",
                                          "bucketOwnerRead",
                                          "authenticatedRead",
                                          "private",
                                          "projectPrivate",
                                          "publicRead"),
           projection = c("noAcl","full"),
           versioning = FALSE,
           lifecycle = NULL){

  projection                 <- match.arg(projection)
  predefinedAcl              <- match.arg(predefinedAcl)
  storageClass               <- match.arg(storageClass)
  predefinedDefaultObjectAcl <- match.arg(predefinedDefaultObjectAcl)

  assert_that(
    is.string(projectId),
    is.string(name),
    is.string(location),
    is.flag(versioning)
  )

  pars_args <- list(project = projectId,
                    predefinedAcl = predefinedAcl,
                    predefinedDefaultObjectAcl = predefinedDefaultObjectAcl,
                    projection = projection)
  pars_args <- rmNullObs(pars_args)

  bb <- gar_api_generator("https://www.googleapis.com/storage/v1/b",
                          "POST",
                          pars_args = pars_args)

  body <- list(
    name = name,
    location = location,
    storageClass = storageClass,
    versioning = list(
      enabled = versioning
    ),
    lifecycle = make_lifecycle_list(lifecycle)
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
#' @param bucket Name of the bucket, or a bucket object
#' @param ifMetagenerationMatch Delete only if metageneration matches
#' @param ifMetagenerationNotMatch Delete only if metageneration does not match
#' @param force_delete If the bucket contains objects it will prevent deletion, including objects in a versioned bucket that previously existed.  Setting this to TRUE will force deletion of those objects before deleting the bucket itself.
#' @family bucket functions
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
#' @export
gcs_delete_bucket <- function(bucket,
                              ifMetagenerationMatch = NULL,
                              ifMetagenerationNotMatch = NULL,
                              force_delete = FALSE){

  bucket <- as.bucket_name(bucket)
  
  if(isTRUE(force_delete)){
    gcs_delete_bucket_objects(bucket, include_versions=TRUE)
  }

  pars_args <- list(ifMetagenerationMatch=ifMetagenerationMatch,
                    ifMetagenerationNotMatch=ifMetagenerationNotMatch)
  pars_args <- rmNullObs(pars_args)

  bb <- gar_api_generator("https://www.googleapis.com/storage/v1/",
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

#' Delete all objects in a bucket
#' @export
#' @param include_versions Whether to include all historic versions of the objects to delete
#' @rdname gcs_delete_bucket
#' @seealso \link{gcs_delete_object}
#' @import assertthat
gcs_delete_bucket_objects <- function(bucket,
                                      include_versions = FALSE){
  myMessage("Deleting all objects in bucket:", bucket)
  bucket <- as.bucket_name(bucket)
  assert_that(is.flag(include_versions))
  if(isFALSE(include_versions)){
    include_versions <- NULL
  }
  os <- gcs_list_objects(bucket, 
                         versions = include_versions, 
                         detail = "full")
  
  safe_delete <- function(x, bucket, version = NULL){
    tryCatch({
      gcs_delete_object(x, bucket = bucket, generation = version)
    }, error = function(ex) {
      NULL
    })
  }
  
  if(isTRUE(include_versions)){
    mapply(safe_delete, 
           x = os$name, version = os$generation, 
           MoreArgs = list(bucket = bucket))
  } else {
    lapply(os$name, safe_delete, bucket = bucket)
  }
  

  TRUE
}

#' Create a lifecycle condition
#'
#' Use this to set rules for how long objects last in a bucket in \link{gcs_create_bucket}
#'
#' @param age Age in days before objects are deleted
#' @param createdBefore Deletes all objects before this date
#' @param numNewerVersions Deletes all newer versions of this object
#' @param isLive If TRUE deletes all live objects, if FALSE deletes all archived versions
#'
#' \code{numNewerVersions} and \code{isLive} works only for buckets with object versioning
#'
#' For multiple conditions, pass this object in as a list.
#'
#'
#' @seealso
#'
#' Lifecycle documentation \url{https://cloud.google.com/storage/docs/lifecycle}
#'
#' @export
#' @import assertthat
#' @family bucket functions
#' @examples 
#' \dontrun{
#'   lifecycle <- gcs_create_lifecycle(age = 30)
#'   
#'   gcs_create_bucket("your-bucket-lifecycle",
#'                      projectId = "your-project",
#'                      location = "EUROPE-NORTH1",
#'                      storageClass = "REGIONAL",
#'                      lifecycle = list(lifecycle))
#' 
#' 
#' }
gcs_create_lifecycle <- function(age = NULL,
                                 createdBefore = NULL,
                                 numNewerVersions = NULL,
                                 isLive = NULL){

  if(!is.null(age)){
    assert_that(is.numeric(age))
  }

  if(!is.null(createdBefore)){
    createdBefore <- as.character(as.Date(createdBefore, format = "%Y-%m-%d"))
    if(is.na(createdBefore)){
      stop("Problem with createdBefore date, converted to ", createdBefore)
    }
    assert_that(is.character(createdBefore))
  }

  if(!is.null(numNewerVersions)){
    assert_that(is.numeric(numNewerVersions))
  }

  if(!is.null(isLive)){
    assert_that(is.logical(isLive))
  }

  rule <- list(
    action = list(
      type = "Delete"
    ),
    condition = list(
      age = age,
      createdBefore = createdBefore,
      numNewerVersions = numNewerVersions,
      isLive = isLive
    )
  )

  rule <- rmNullObs(rule)

  structure(
    rule, class = c("list","gcs_lifecycle")
  )

}

make_lifecycle_list <- function(lifecycle){

  if(!is.null(lifecycle)){

    assertthat::assert_that(
      is.list(lifecycle),
      all(vapply(lifecycle,
                 function(x) inherits(x, "gcs_lifecycle"),
                 logical(1)))
      )

    out <- list(
      list(
        rule = lifecycle
      )

        )
  } else {
    out <- NULL
  }

  out
}
