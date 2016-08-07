#' Save an R session to the Google Cloud
#'
#' If objects to be saved are for use with only R consider using this
#'   function which saves in a compressed format.
#'
#' @param ... Names of objects to be saved (as symbols or character strings)
#' @param list A character vector containing the names of objects to be saved
#' @param bucket Bucket to store objects in
#' @param folder The namespace for the files to be stored in the cloud
#' @param keepCache Whether to also keep the files locally
#'
#' @details
#'
#' \code{gcs_save(bucket = "your_bucket")} will save all objects in the workspace
#'   to \code{.RData} folder on Google Cloud Storage within \code{your_bucket}.
#'
#' If multiple objects, they will be uploaded separately in a folder within the bucket
#'   using \code{.rds} format.
#'
#' Bear in mind this will overwrite any data with the same name in folder.
#'
#' @family R session data functions
#' @return TRUE if successful
#' @export
gcs_save <- function(...,
                     list = ls(all.names = TRUE, envir = parent.frame()),
                     bucket,
                     folder = ".RData",
                     keepCache = FALSE){

  ## from save()
  names <- as.character(substitute(list(...)))[-1L]
  if (!length(list) && !length(names))
    stop("nothing specified to be save()d")
  nlist <- c(list, names)

  if(keepCache){
    fold <- getwd()
  } else {
    fold <- tempdir()
    on.exit(unlink(fold))
  }

  pb <- txtProgressBar(min = 1, max = length(nlist))
  for(i in seq_along(nlist)){
    setTxtProgressBar(pb, i)
    obj_name <- nlist[[i]]
    filepath <- file.path(fold,paste0(obj_name,".rds"))
    saveRDS(get(nlist[[i]]), file = file.path(fold,paste0(obj_name,".rds")))
    gcs_upload(filepath, bucket, name = file.path(".RData",obj_name))
  }
  close(pb)

  TRUE

}


#' Load an R session from the Google Cloud
#'
#' Load R objects that have been saved using \link{gcs_save}
#'
#' @param bucket Bucket the stored objects are in
#' @param folder The namespace for the files stored
#' @param keepCache Whether to also keep the files locally

#'
#' @family R session data functions
#' @return TRUE if successful
#' @export
gcs_load <- function(bucket, folder = ".RData", keepCache = FALSE){

}
