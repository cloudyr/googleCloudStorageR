#' Upload a file of arbitrary type
#'
#' Upload up to 5TB
#'
#' @param file data.frame, list, R object or filepath (character) to upload file
#' @param bucket bucketname you are uploading to
#' @param type MIME type, guessed from file extension if NULL
#' @param name What to call the file once uploaded. Default is the filepath
#' @param object_function If not NULL, a \code{function(input, output)}
#' @param object_metadata Optional metadata for object created via \link{gcs_metadata_object}
#' @param predefinedAcl Specify user access to object. Default is 'private'. Set to 'bucketLevel' for buckets with bucket level access enabled.
#' @param upload_type Override automatic decision on upload type
#'
#' @details
#'
#' When using \code{object_function} it expects a function with two arguments:
#' \itemize{
#'   \item \code{input} The object you supply in file to write from
#'   \item \code{output} The filename you write to
#'  }
#'
#'
#' By default the \code{upload_type} will be 'simple' if under 5MB, 'resumable' if over 5MB.  Use \link{gcs_upload_set_limit} to modify this boundary - you may want it smaller on slow connections, higher on faster connections.
#'   'Multipart' upload is used if you provide a \code{object_metadata}.
#'
#' If \code{object_function} is NULL and \code{file} is not a character filepath,
#'   the defaults are:
#'
#' \itemize{
#'   \item file's class is \code{data.frame} - \link[utils]{write.csv}
#'   \item file's class is \code{list} - \link[jsonlite]{toJSON}
#'  }
#'
#' If \code{object_function} is not NULL and \code{file} is not a character filepath,
#'   then \code{object_function} will be applied to the R object specified
#'   in \code{file} before upload. You may want to also use \code{name} to ensure the correct
#'   file extension is used e.g. \code{name = 'myobject.feather'}
#'
#' If \code{file} or \code{name} argument contains folders e.g. \code{/data/file.csv} then
#'   the file will be uploaded with the same folder structure e.g. in a \code{/data/} folder.
#' Use \code{name} to override this.
#'
#' @section scopes:
#'
#' Requires scopes \code{https://www.googleapis.com/auth/devstorage.read_write}
#'   or \code{https://www.googleapis.com/auth/devstorage.full_control}
#'
#' @return If successful, a metadata object
#'
#' @examples
#'
#' \dontrun{
#'
#' ## set global bucket so don't need to keep supplying in future calls
#' gcs_global_bucket("my-bucket")
#'
#' ## by default will convert dataframes to csv
#' gcs_upload(mtcars)
#'
#' ## mtcars has been renamed to mtcars.csv
#' gcs_list_objects()
#'
#' ## to specify the name, use the name argument
#' gcs_upload(mtcars, name = "my_mtcars.csv")
#'
#' ## when looping, its best to specify the name else it will take
#' ## the deparsed function call e.g. X[[i]]
#' my_files <- list.files("my_uploads")
#' lapply(my_files, function(x) gcs_upload(x, name = x))
#'
#' ## you can supply your own function to transform R objects before upload
#' f <- function(input, output){
#'   write.csv2(input, file = output)
#' }
#'
#' gcs_upload(mtcars, name = "mtcars_csv2.csv", object_function = f)
#' 
#' # upload to a bucket with bucket level ACL set
#' gcs_upload(mtcars, predefinedAcl = "bucketLevel")
#' 
#' # modify boundary between simple and resumable uploads
#' # default 5000000L is 5MB
#' gcs_upload_set_limit(1000000L)
#' }
#'
#'
#'
#' @importFrom httr add_headers upload_file
#' @importFrom utils URLencode write.csv
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom googleAuthR gar_api_generator
#' @importFrom tools file_ext
#' @import assertthat
#'
#' @export
gcs_upload <- function(file,
                       bucket = gcs_get_global_bucket(),
                       type = NULL,
                       name = deparse(substitute(file)),
                       object_function = NULL,
                       object_metadata = NULL,
                       predefinedAcl = c(
                         "private",
                         "bucketLevel",
                         "authenticatedRead",
                         "bucketOwnerFullControl",
                         "bucketOwnerRead",
                         "projectPrivate",
                         "publicRead",
                         "default"
                       ),
                       upload_type = c("simple","resumable")){
  
  bucket        <- as.bucket_name(bucket)
  predefinedAcl <- match.arg(predefinedAcl)
  upload_type   <- match.arg(upload_type)
  
  ## no leading slashes
  name <- gsub("^/","", URLencode(name, reserved = TRUE))
  
  ## method dispatch for custom function
  if(!is.null(object_function)){
    class(file) <- c("gcs_cf", class(file))
  }
  
  ## so jsonlite::toJSON works
  if(!is.null(object_metadata)) class(object_metadata) <- "list"
  
  ## no caching
  googleAuthR::gar_cache_setup(invalid_func = function(req){FALSE})
  
  # hack to get around method dispatch class for file
  gcs_upload_s3(file = file,
                bucket = bucket,
                type = type,
                name = name,
                object_function = object_function,
                object_metadata = object_metadata,
                predefinedAcl = predefinedAcl,
                upload_type = upload_type)
  
  
}

gcs_upload_s3 <- function(file,
                          bucket,
                          type,
                          name,
                          object_function,
                          object_metadata,
                          predefinedAcl,
                          upload_type){
  
  UseMethod("gcs_upload_s3")
}


gcs_upload_s3.character <- function(file,
                                    bucket,
                                    type,
                                    name,
                                    object_function,
                                    object_metadata,
                                    predefinedAcl,
                                    upload_type){
  myMessage("gcs_upload.character", level = 1)
  
  assert_that(is.readable(file))
  
  temp <- file
  ## get rid of " marks
  name <- gsub("%22","", name)
  
  do_upload(name = name,
            bucket = bucket,
            predefinedAcl = predefinedAcl,
            object_metadata = object_metadata,
            temp = temp,
            type = type,
            upload_type = upload_type)
}

gcs_upload_s3.gcs_cf <- function(file,
                                 bucket,
                                 type,
                                 name,
                                 object_function,
                                 object_metadata,
                                 predefinedAcl,
                                 upload_type){
  myMessage("gcs_upload.gcs_cf", level = 1)

  assert_that(is.function(object_function))
  
  if(all(names(formals(object_function)) != c("input","output"))){
    stop("object_function should carry only two arguments - 'input' and 'output'")
  }
  
  ## take file extension of name, fix #91
  temp <- tempfile(fileext = paste0(".",tools::file_ext(name)))
  on.exit(unlink(temp))
  
  tryCatch({
    object_function(input = file, output = temp)
  }, error = function(ex) {
    stop("object_function error: ", ex$message)
  }) 
  
  if(!is.readable(temp)){
    stop("Can not read file created by passed object_function()")
  }
  
  do_upload(name = name,
            bucket = bucket,
            predefinedAcl = predefinedAcl,
            object_metadata = object_metadata,
            temp = temp,
            type = type,
            upload_type = upload_type)
  
  
}


gcs_upload_s3.data.frame <- function(file,
                                     bucket,
                                     type,
                                     name,
                                     object_function,
                                     object_metadata,
                                     predefinedAcl,
                                     upload_type){
  myMessage("gcs_upload.data.frame", level = 1)
  
  temp <- tempfile(fileext = ".csv")
  on.exit(unlink(temp))
  
  write.csv(file, file = temp)
  
  name <- if(!grepl("\\.csv$", name)) paste0(name,".csv") else name
  
  do_upload(name = name,
            bucket = bucket,
            predefinedAcl = predefinedAcl,
            object_metadata = object_metadata,
            temp = temp,
            type = type,
            upload_type = upload_type)
  
}


gcs_upload_s3.list <- function(file,
                               bucket,
                               type,
                               name,
                               object_function,
                               object_metadata,
                               predefinedAcl,
                               upload_type){
  myMessage("gcs_upload.list", level = 1)
  
  temp <- tempfile(fileext = ".json")
  on.exit(unlink(temp))
  
  write(toJSON(file), temp)
  
  name <- if(!tools::file_ext(name) == "json") paste0(name,".json") else name
  
  do_upload(name = name,
            bucket = bucket,
            predefinedAcl = predefinedAcl,
            object_metadata = object_metadata,
            temp = temp,
            type = type,
            upload_type = upload_type)
  
}


gcs_upload_s3.default <- function(file,
                                  bucket,
                                  type,
                                  name,
                                  object_function,
                                  object_metadata,
                                  predefinedAcl,
                                  upload_type){
  
  stop("Unsupported object type passed in argument file: specify filepath or define a
         write function using argument object_function", call. = FALSE)
}


#' @param upload_limit Upload limit in bytes
#' @rdname gcs_upload
#' @export
gcs_upload_set_limit <- function(upload_limit = 5000000L){
  assert_that(
    is.scalar(upload_limit),
    is.integer(upload_limit)
  )
  myMessage("Setting upload limit for simple uploads to ", upload_limit)
  options(googleCloudStorageR.upload_limit = upload_limit)
}

do_upload <- function(name,
                      bucket,
                      predefinedAcl,
                      object_metadata,
                      temp,
                      type,
                      upload_type){
  
  myMessage("File size detected as ",
            format_object_size(file.size(temp), "auto"), level = 3)
  
  # default is 5MB simple upload limit
  UPLOAD_LIMIT <- getOption("googleCloudStorageR.upload_limit", 5000000L)
  assert_that(is.scalar(UPLOAD_LIMIT))
  
  if(upload_type == "resumable" || file.size(temp) > UPLOAD_LIMIT){
    do_resumable_upload(name = name,
                        bucket = bucket,
                        predefinedAcl = predefinedAcl,
                        object_metadata = object_metadata,
                        temp = temp,
                        type = type)
  } else if(!is.null(object_metadata)){
    do_multipart_upload(name = name,
                        bucket = bucket,
                        predefinedAcl = predefinedAcl,
                        object_metadata = object_metadata,
                        temp = temp,
                        type = type)
  } else {
    do_simple_upload(name = name,
                     bucket = bucket,
                     predefinedAcl = predefinedAcl,
                     object_metadata = object_metadata,
                     temp = temp,
                     type = type)
  }
  
}

do_simple_upload <- function(name,
                             bucket,
                             predefinedAcl,
                             object_metadata,
                             temp,
                             type){
  ## simple upload <5MB
  bb <- httr::upload_file(temp, type = type)
  myMessage("Simple upload", level = 2)
  
  pars_args <- list(uploadType="media",
                    name=name)
  
  if(!predefinedAcl %in% c("default","bucketLevel")){
    pars_args[["predefinedAcl"]] <- predefinedAcl
  }
  
  up <-
    gar_api_generator("https://www.googleapis.com/upload/storage/v1",
                      "POST",
                      path_args = list(b = bucket,
                                       o = ""),
                      pars_args = pars_args)
  
  req <- up(the_body = bb)
  
  structure(req$content, class = "gcs_objectmeta")
  
}

do_multipart_upload <- function(name,
                                bucket,
                                predefinedAcl,
                                object_metadata,
                                temp,
                                type){
  
  ## multipart upload
  myMessage("Multi-part upload", level = 2)
  if(!is.null(object_metadata$name)){
    name <- object_metadata$name
  }
  
  temp2 <- tempfile(fileext = ".json")
  on.exit(unlink(temp2))
  
  ## http://stackoverflow.com/questions/31080363/how-to-post-multipart-related-content-with-httr-for-google-drive-api
  writeLines(toJSON(object_metadata, auto_unbox = TRUE), temp2)
  
  bb <- list(
    metadata = upload_file(temp2, type = "application/json; charset=UTF-8"),
    content = upload_file(temp, type = type)
  )
  
  pars_args <- list(uploadType="multipart",
                    name=name)
  
  if(!predefinedAcl %in% c("default","bucketLevel")){
    pars_args[["predefinedAcl"]] <- predefinedAcl
  }
  
  the_url <- sprintf("https://storage.googleapis.com/upload/storage/v1/b/%s/o",
                     bucket)
  up <-
    gar_api_generator(the_url,
                      "POST",
                      pars_args = pars_args,
                      checkTrailingSlash = FALSE,
                      customConfig = list(
                        encode = "multipart"
                      ))
  
  req <- up(the_body = bb)
  
  structure(req$content, class = "gcs_objectmeta")
  
}


do_resumable_upload <- function(name,
                                bucket,
                                predefinedAcl,
                                object_metadata,
                                temp,
                                type){
  myMessage("Resumable upload", level = 2)
  
  pars_args <- list(uploadType="resumable",
                    name=name)
  if(!predefinedAcl %in% c("default","bucketLevel")){
    pars_args[["predefinedAcl"]] <- predefinedAcl
  }
  
  up <-
    googleAuthR::gar_api_generator("https://www.googleapis.com/upload/storage/v1",
                                   "POST",
                                   path_args = list(b = bucket,
                                                    o = ""),
                                   pars_args = pars_args,
                                   customConfig = list(
                                     add_headers("X-Upload-Content-Type" = type),
                                     add_headers("X-Upload-Content-Length" = file.size(temp))
                                     
                                   ))
  
  # suppress empty JSON content warning (#120)
  req <- suppressWarnings(up(the_body = object_metadata))
    
  ## extract the upload URL
  if(req$status_code == 200){
    
    upload_url <- req$headers$location
    myMessage("Found resumeable upload URL: ", upload_url, level = 3)
    
  } else {
    stop("Couldn't find resumeable upload URL", call. = FALSE)
  }
  
  up2 <- PUTme(upload_url,
               body = upload_file(temp, type = type))
  
  if(up2$status_code %in% c(200,201)){
    
    out <- structure(fromJSON(content(up2, as ="text")), 
                     class = "gcs_objectmeta")
  } else {

    myMessage("File upload failed, trying to resume...", level = 3)

    retry <- 3
    while(retry > 0){
      myMessage(sprintf("Retry %s of 3", retry), level = 3)
      Sys.sleep(10)
      try <- gcs_retry_upload(upload_url = upload_url, file = temp, type = type)
      if(inherits(try, "gcs_objectmeta")){
        # success
        return(try)
      } else {
        retry <- retry - 1
      }
    }
    
    if(inherits(out, "gcs_upload_retry")){
      myMessage("All attempts failed.
                Returning gcs_upload_retry object for you to use in gcs_retry_upload() later.", level = 3)
      return(try)
    } else {
      stop("Unknown class of object returned on retry")
    }
    
  }
  
}





#' Retry a resumeable upload
#'
#' Used internally in \link{gcs_upload}, you can also use this
#'   for failed uploads within one week of generating the upload URL
#'
#' @param retry_object A object of class \code{gcs_upload_retry}.
#' @param upload_url As created in a failed upload via \link{gcs_upload}
#' @param file The file location to upload
#' @param type The file type, guessed if NULL
#'
#' Either supply a retry object, or the upload_url, file and type manually yourself.
#'
#' The function will first check to see how much has been uploaded already, then try to send up
#'   the remaining bytes.
#'
#' @return If successful, an object metadata object, if not an gcs_upload_retry object.
#' @import assertthat
#' @export
gcs_retry_upload <- function(retry_object=NULL, upload_url=NULL, file=NULL, type = NULL){

  if(is.null(retry_object)){
    if(any(is.null(upload_url), is.null(file), is.null(type))){
      stop("Must supply either retry_object or all of upload_url, file and type", 
           call. = FALSE)
    }
  } else {

    assert_that(inherits(retry_object, "gcs_upload_retry"))

    upload_url <- retry_object$upload_url
    file <- retry_object$file
    type <- retry_object$type
  }

  size <- as.numeric(file.size(file))
  upload_status <- PUTme(upload_url,
                         httr::add_headers("Content-Range" = paste0("*/",size)))

  if(upload_status$status_code %in% c(200,201)){
    myMessage("Upload complete", level = 3)
    return(structure(jsonlite::fromJSON(content(upload_status$content, as ="text")),
                            class = "gcs_objectmeta"))
  } else if(upload_status$status_code == 308){
    myMessage("Upload incomplete", level = 3)

    range <- upload_status$headers$range
    ## split range in format '0-42' to find position of next byte
    new_byte <- as.numeric(strsplit(range, "-")[[1]][[2]])

    ## make remaining file to send up
    temp3 <- tempfile()
    on.exit(unlink(temp3))

    con <- file(file, open = "rb")
    discard_file <- readBin(con, what = "raw", n = new_byte)
    upload_file <- readBin(con, what = "raw", n = new_byte + 1)
    close(con)
    rm(discard_file)

    content_range <- paste0("bytes ",
                            (new_byte + 1),"-",size - 1,
                            "/",size)

    up_resume <- PUTme(upload_url,
                       httr::add_headers("Content-Range" = content_range),
                       httr::content_type(type),
                       body = upload_file)

    if(up_resume$status_code %in% c(200,201)){
      myMessage("Upload complete", level = 3)
      out <- structure(jsonlite::fromJSON(content(up_resume, as ="text")),
                       class = "gcs_objectmeta")
    } else {
      out <- structure(
        list(
          upload_url = upload_url,
          file = file,
          type = type,
          updated = Sys.time(),
          size = size,
          remaining = new_byte + 1,
          content_range = content_range
        ),
        class = "gcs_upload_retry"
      )
    }

  } else {
    stop("Couldn't get upload status")
  }

  out
}

#' @noRd
#' @importFrom httr PUT verbose
PUTme <- function(...){
  if(getOption("googleAuthR.verbose") < 3){
    PUT(..., verbose())
  } else {
    PUT(...)
  }
}
