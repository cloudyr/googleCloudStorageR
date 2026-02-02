#' Read a file from a bucket
#' @description
#' This reads a file from a bucket into the current \R session.
#'
#' @param file The file in the bucket
#' @param bucket Name of the bucket
#' @param read_fun Function to convert the given file to an R object.
#' The first argument of this function must be the file name.
#' @param tempdir Temporary directory where the file will be downloaded to 
#' before it is read by import_function()
#' @param ... Other arguments passed on to import_function()
#'
#' @details
#' This function is a wrapper around \code{\link{gcs_get_object}} that allows to 
#' read a file from a bucket into the current R session. This is done either 
#' directly by establishing a raw connection, or by first downloading the file 
#' to a temporary location at \code{\link[base]{tempdir}}, followed by reading 
#' it with \code{read_fun}.
#'
#' @return An \R object
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1)
#'
#' gcs_write(df, file = "df.rds", write_fun = saveRDS)
#' gcs_read(file = "df.rds", read_fun = readRDS)
#'
#' gcs_write(df, file = "df.csv", write_fun = readr::write_csv)
#' gcs_read(file = "df.csv", read_fun = readr::read_csv)
#' }
#'
#' @importFrom httr content
#' @export
gcs_read <- function(
  file,
  bucket = gcs_get_global_bucket(),
  read_fun,
  temp_file = FALSE,
  ...
) {
  if (isFALSE(temp_file)) {
    # read file directly into the R session via a raw connection
    obj <- gcs_get_object(
      object_name = file,
      bucket = bucket,
      parseObject = FALSE
    )
    cont <- content(x = obj, as = "raw")
    con <- rawConnection(cont)

    # .rds files, if written by saveRDS or readr::write_rds
    # (with default arguments), are either gzipped (saveRDS)
    # or uncompressed (write_rds) and in both cases gzcon
    # can uncompress them
    if (tools::file_ext(file) %in% c("rds", "RDS")) {
      con <- gzcon(con)
      tryCatch(
        res <- read_fun(con, ...),
        error = function(e) {
          stop(
            "There was an error when reading the file ",
            file,
            ". ",
            "This usually happens when the .rds file was saved with another compression than gzip or none. ",
            "Try gcs_read(read_fun = readRDS, temp_file = TRUE).",
            call. = FALSE
          )
        }
      )
    } else {
      res <- read_fun(con, ...)
    }
  } else if (isTRUE(temp_file)) {
    # first download to a temporary file, then load the file
    temp_file <- tempfile(fileext = paste0(".", tools::file_ext(file)))
    gcs_get_object(file, bucket = bucket, saveToDisk = temp_file)
    res <- read_fun(temp_file, ...)
    message("deleting temporary file ", temp_file)
    unlink(temp_file)
  } else {
    stop("temp_file must be TRUE or FALSE.")
  }

  res
}

#' Write an R object to a file in a bucket
#' @description
#' This exports an R object into a file and uploads it to a bucket.
#'
#' @param x The R object to write
#' @param file Target file in the bucket to write
#' @param bucket Name of the bucket
#' @param write_fun The function to export the object  \code{x} into a file.
#' Must be a function with at least two arguments where the first
#' argument is the R object to save and the second the name of the file
#' to write.
#' @param ... Other arguments passed on to write_fun()
#'
#' @details
#' This function is a wrapper around \code{\link{gcs_upload}}. The given \R object
#' is first written to a temporary file located in \code{\link[base]{tempdir}}, 
#' followed by uploading it.
#'
#' @return description
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1)
#' gcs_write(df, "df.csv", write_fun = write.csv)
#' gcs_write(df, "df.rds", write_fun = saveRDS)
#' }
#'
#' @export
gcs_write <- function(
  x,
  file,
  bucket = gcs_get_global_bucket(),
  write_fun,
  ...
) {
  # define temporary file
  temp_file <- tempfile(fileext = paste0(".", tools::file_ext(file)))

  # write object to temp file
  tryCatch(
    write_fun(x, temp_file, ...),
    error = function(e) {
      message("An internal error occurred: ", conditionMessage(e))
      stop(
        "Does write_fun have the R object as first and the file to write to as second argument?",
        call. = FALSE
      )
    }
  )

  # upload temp file to bucket
  gcs_upload(file = temp_file, bucket = bucket, name = file)

  # remove temp file after upload
  unlink(temp_file)
}
