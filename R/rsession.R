#' Save an R session to the Google Cloud
#'
#' If objects to be saved are for use with only R consider using this
#'   function which saves in a compressed format.
#'
#' @param ... Names of objects to be saved (as symbols or character strings)
#' @param list A character vector containing the names of objects to be saved
#' @param bucket Bucket to store objects in
#' @param folder The namespace for the files to be stored in the cloud
#'
#' @details
#'
#' \code{gcs_save(bucket = "your_bucket")} will save all objects in the workspace
#'   to \code{.RData} folder on Google Cloud Storage within \code{your_bucket}.
#'
#' It will not include objects in R's \code{.RData} file, to avoid duplication.
#' To upload \code{.RData}, rename it.
#'
#' Restore the objects using \code{gcs_load(bucket = "your_bucket")}
#'
#' Objects will be uploaded separately in a folder within the bucket
#'   using \code{.rds} format.
#'
#' This will overwrite any data with the same name in folder.
#'
#' @family R session data functions
#' @return TRUE if successful
#' @export
gcs_save <- function(...,
                     list = ls(all.names = TRUE, envir = parent.frame()),
                     bucket = gcs_get_global_bucket(),
                     folder = ".RData"){

  ## from save()
  names <- as.character(substitute(list(...)))[-1L]
  if (!length(list) && !length(names))
    stop("nothing specified to be gcs_save()d")
  nlist <- c(list, names)
  ## don't include existing .RData objects
  nlist <- setdiff(nlist, nlist[grepl("^\\.RData", nlist)])

  fold <- tempdir()
  on.exit(unlink(fold))

  pb <- txtProgressBar(min = 1, max = length(nlist))
  for(i in seq_along(nlist)){
    setTxtProgressBar(pb, i)
    obj_name <- paste0(nlist[[i]],".rds")
    upload_name <- file.path(".RData",obj_name)
    filepath <- file.path(fold,obj_name)

    saveRDS(get(nlist[[i]]), file = filepath)

    gcs_upload(filepath,
               bucket,
               name = URLencode(upload_name, reserved = TRUE))

  }
  close(pb)
  unlink(fold)

  TRUE

}


#' Load an R session from the Google Cloud
#'
#' Load R objects that have been saved using \link{gcs_save}
#'
#' @param bucket Bucket the stored objects are in
#' @param folder The namespace for the files stored
#'
#' @family R session data functions
#' @return TRUE if successful
#' @export
gcs_load <- function(bucket = gcs_get_global_bucket(), folder = ".RData"){

  objs <- gcs_list_objects(bucket)
  session_objs <- objs$name[grepl(paste0("^\\",folder),objs$name)]

  pb <- txtProgressBar(min = 1, max = length(session_objs))
  for(i in seq_along(session_objs)){
    setTxtProgressBar(pb, i)
    obj_name <- paste0(session_objs[[i]])
    local_name <- gsub(paste0("^",folder,"/"),"",obj_name)
    dl <- gcs_get_object(obj_name,
                         bucket=bucket,
                         saveToDisk = local_name)
    if(!dl) stop("Problem downloading object ", obj_name)
    obj <- readRDS(local_name)
    eee <- globalenv()
    eee[[gsub("\\.rds$","",local_name)]] <- obj
    unlink(local_name)
  }
  close(pb)

  TRUE
}
