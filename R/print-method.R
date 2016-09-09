#' @export
print.gcs_bucket <- function(x, ...){
  cat("==Google Cloud Storage Bucket==\n")
  cat0("Bucket:         ", x$name)
  cat0("Project Number: ", x$projectNumber)
  cat0("Location:       ", x$location)
  cat0("Class:          ", x$storageClass)
  cat0("Created:        ", as.character(timestamp_to_r(x$timeCreated)))
  cat0("Updated:        ", as.character(timestamp_to_r(x$updated)))
  cat0("Versioning:     ", x$versioning$enabled)
  cat0("Meta-generation:", x$metageneration)
  cat0("eTag:           ", x$etag)

  if(!is.null(x$lifecycle)){
    cat("Lifecycle Rules: \n")
    print(x$lifecycle$rule)
  }

  if(!is.null(x$acl)){
    cat("\nAccess Control List:  \n")
    print(x$acl[,c("entity","role","projectTeam")])
  }
}

#' @export
print.gcs_objectmeta <- function(x, ...){
  cat("==Google Cloud Storage Object==\n")
  cat0("Name:           ", x$name)
  cat0("Type:           ", x$contentType)
  cat0("Size:           ", format_object_size(as.numeric(x$size), "auto"))
  cat0("Media URL:      ", x$mediaLink)
  cat0("Download URL:   ", gcs_download_url(x$name, x$bucket))
  cat0("Bucket:         ", x$bucket)
  cat0("ID:             ", x$id)
  cat0("MD5 Hash:       ", x$md5Hash)
  cat0("Class:          ", x$storageClass)
  cat0("Created:        ", as.character(timestamp_to_r(x$timeCreated)))
  cat0("Updated:        ", as.character(timestamp_to_r(x$updated)))
  cat0("Generation:     ", x$generation)
  cat0("Meta Generation:", x$metageneration)
  cat0("eTag:           ", x$etag)
  cat0("crc32c:         ", x$crc32c)
}

#' @export
print.gcs_object_access <- function(x, ...){
  cat("==Google Cloud Storage Object Access Control==\n")
  cat0("Email:            ", x$email)
  cat0("Domain:           ", x$domain)
  cat0("Object:           ", x$object)
  cat0("Entity:           ", x$entity)
  cat0("EntityId:         ", x$entityId)
  cat0("Project Number:   ", x$projectTeam$projectNumber)
  cat0("Project Team:     ", x$projectTeam$team)
  cat0("Role:             ", x$role)
  cat0("Bucket:           ", x$bucket)
  cat0("ID:               ", x$id)
  cat0("Generation:       ", x$generation)
  cat0("eTag:             ", x$etag)
}

#' @export
print.gar_Object <- function(x, ...){
  cat("==Google Cloud Storage Object Metadata for Upload==\n")
  cat0("Name:               ", x$name)
  cat0("Cache-Control:      ", x$cacheControl)
  cat0("Content-Disposition:", x$contentDisposition)
  cat0("Content-Encoding:   ", x$contentEncoding)
  cat0("crc32c:             ", x$crc32c)
  cat0("MD5 hash            ", x$md5Hash)
  cat0("Metadata:           ", vapply(names(x$metadata),
                                    function(y) paste(y, x$metadata[[y]], collapse = " \n", sep ="="),
                                    character(1))
       )
}

#' @export
print.gcs_upload_retry <- function(x, ...){
  cat("==Google Cloud Storage Upload Retry Object==\n")
  cat0("File Location:    ", x$file)
  cat0("Retry Upload URL: ", x$upload_url)
  cat0("Updated:          ", as.character(x$updated))
  cat0("Type:             ", x$type)
  cat0("File Size:        ", format_object_size(as.numeric(x$size), "auto"))
  cat0("Upload Byte:      ", x$remaining)
  cat0("Upload remaining: ", x$content_range)
}
