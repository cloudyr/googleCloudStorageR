#' @export
print.gcs_bucket <- function(x, ...){
  cat("Bucket:         ", x$name, "\n")
  cat("Project Number: ", x$projectNumber, "\n")
  cat("Location:       ", x$location, "\n")
  cat("Class:          ", x$storageClass, "\n")
  cat("Created:        ", as.character(as.POSIXct(x$timeCreated, format = "%Y-%m-%dT%H:%M:%S")), "\n")
  cat("Updated:        ", as.character(as.POSIXct(x$updated, format = "%Y-%m-%dT%H:%M:%S")), "\n")
  cat("Meta-generation:", x$metageneration, "\n")
  cat("eTag:           ", x$etag, "\n")
}

#' @export
print.gcs_objectmeta <- function(x, ...){
  cat("Name:           ", x$name, "\n")
  cat("Type:           ", x$contentType, "\n")
  cat("Size:           ", utils:::format.object_size(as.numeric(x$size), "auto"), "\n")
  cat("Media URL       ", x$mediaLink, "\n")
  cat("Bucket:         ", x$bucket, "\n")
  cat("ID:             ", x$id, "\n")
  cat("MD5 Hash:       ", x$md5Hash, "\n")
  cat("Class:          ", x$storageClass, "\n")
  cat("Created:        ", as.character(as.POSIXct(x$timeCreated, format = "%Y-%m-%dT%H:%M:%S")), "\n")
  cat("Updated:        ", as.character(as.POSIXct(x$updated, format = "%Y-%m-%dT%H:%M:%S")), "\n")
  cat("Generation:     ", x$generation, "\n")
  cat("Meta Generation:", x$metageneration, "\n")
  cat("eTag:           ", x$etag, "\n")
  cat("crc32c:         ", x$crc32c, "\n")
}
