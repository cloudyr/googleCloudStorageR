#' Update Google Cloud Storage ObjectAccessControls
#'
#' Requires scopes
#'   https://www.googleapis.com/auth/devstorage.full_control
#'   https://www.googleapis.com/auth/cloud-platform
#'
#' @param bucket Google Cloud Storage bucket
#' @param object Object to update
#' @param entity entity to update or add, such as an email
#' @param entity_type what type of entity
#' @param role Access permission for entity
#'
#' @seealso \href{https://cloud.google.com/storage/docs/json_api/v1/objectAccessControls/insert}{objectAccessControls on Google API reference}
#'
#' @return TRUE if successful
#' @export
gcs_update_acl <- function(bucket,
                           object,
                           entity,
                           entity_type = c("user",
                                           "group",
                                           "domian",
                                           "project",
                                           "allUsers",
                                           "allAuthenticatedUsers"),
                           role = c("READER","OWNER")){

  entity_type <- match.arg(entity_type)
  role <- match.arg(role)

  stopifnot(inherits(bucket, "character"),
            inherits(object, "character"),
            inherits(entity, "character"))

  accessControls <- list(
    entity = paste0(entity_type,"-",entity),
    role = role
  )

  insert <-
    googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1",
                                   "POST",
                                   path_args = list(b = bucket,
                                                    o = object,
                                                    acl = ""))

  req <- insert(path_arguments = list(b = bucket, o = object),
                the_body = accessControls)

  if(req$status_code == 200){
    myMessage("Access updated")
    out <- TRUE
  } else {
    stop("Error setting access")
  }

  out

}
