#' @export
print.gcs_bucket <- function(x, ...){
  cat("==Google Cloud Storage Bucket==\n")
  cat("Bucket:         ", x$name, "\n")
  cat("Project Number: ", x$projectNumber, "\n")
  cat("Location:       ", x$location, "\n")
  cat("Class:          ", x$storageClass, "\n")
  cat("Created:        ", as.character(timestamp_to_r(x$timeCreated)), "\n")
  cat("Updated:        ", as.character(timestamp_to_r(x$updated)), "\n")
  cat("Meta-generation:", x$metageneration, "\n")
  cat("eTag:           ", x$etag, "\n")
}

#' @export
print.gcs_objectmeta <- function(x, ...){
  cat("==Google Cloud Storage Object==\n")
  cat("Name:           ", x$name, "\n")
  cat("Type:           ", x$contentType, "\n")
  cat("Size:           ", utils:::format.object_size(as.numeric(x$size), "auto"), "\n")
  cat("Media URL       ", x$mediaLink, "\n")
  cat("Bucket:         ", x$bucket, "\n")
  cat("ID:             ", x$id, "\n")
  cat("MD5 Hash:       ", x$md5Hash, "\n")
  cat("Class:          ", x$storageClass, "\n")
  cat("Created:        ", as.character(timestamp_to_r(x$timeCreated)), "\n")
  cat("Updated:        ", as.character(timestamp_to_r(x$updated)), "\n")
  cat("Generation:     ", x$generation, "\n")
  cat("Meta Generation:", x$metageneration, "\n")
  cat("eTag:           ", x$etag, "\n")
  cat("crc32c:         ", x$crc32c, "\n")
}

#' @export
print.gcs_object_access <- function(x, ...){
  cat("==Google Cloud Storage Object Access Control==\n")
  if(!is.null(x$email)){
  cat("Email:            ", x$email, "\n")
  }
  if(!is.null(x$domain)){
  cat("Domain:           ", x$domain, "\n")
  }
  cat("Object:           ", x$object, "\n")
  cat("Entity:           ", x$entity, "\n")
  if(!is.null(x$entityId)){
  cat("EntityId:         ", x$entityId, "\n")
  }
  if(!is.null(x$projectTeam$projectNumber)){
  cat("Project Number:   ", x$projectTeam$projectNumber, "\n")
  }
  if(!is.null(x$projectTeam$team)){
  cat("Project Team:     ", x$projectTeam$team, "\n")
  }
  cat("Role:             ", x$role, "\n")
  cat("Bucket:           ", x$bucket, "\n")
  cat("ID:               ", x$id, "\n")
  cat("Generation:       ", x$generation, "\n")
  cat("eTag:             ", x$etag, "\n")
}
