
#' Change access to an object in a bucket
#'
#' Updates Google Cloud Storage ObjectAccessControls
#'
#' @param object_name Object to update
#' @param bucket Google Cloud Storage bucket
#' @param entity entity to update or add, such as an email
#' @param entity_type what type of entity
#' @param role Access permission for entity
#'
#' @details
#'
#' An \code{entity} is an identifier for the \code{entity_type}.
#'
#' \itemize{
#'   \item \code{entity="user"} may have \code{userId} or \code{email}
#'   \item \code{entity="group"} may have \code{groupId} or \code{email}
#'   \item \code{entity="domain"} may have \code{domain}
#'   \item \code{entity="project"} may have \code{team-projectId}
#'  }
#'
#' For example:
#'
#' \itemize{
#'   \item \code{entity="user"} could be \code{jane@doe.com}
#'   \item \code{entity="group"} could be \code{example@googlegroups.com}
#'   \item \code{entity="domain"} could be \code{example.com} which is a Google Apps for Business domain.
#'  }
#'
#'
#' @seealso \href{https://cloud.google.com/storage/docs/json_api/v1/objectAccessControls/insert}{objectAccessControls on Google API reference}
#'
#' @return TRUE if successful
#' @family Access control functions
#' @export
gcs_update_acl <- function(object_name,
                           bucket,
                           entity = "",
                           entity_type = c("user",
                                           "group",
                                           "domain",
                                           "project",
                                           "allUsers",
                                           "allAuthenticatedUsers"),
                           role = c("READER","OWNER")){

  entity_type <- match.arg(entity_type)
  role <- match.arg(role)

  stopifnot(inherits(bucket, "character"),
            inherits(object_name, "character"),
            inherits(entity, "character"))

  if(entity == "" && !(entity_type %in% c("allUsers","allAuthenticatedUsers"))){
    stop("Must supply non-empty entity argument")
  }

  accessControls <- list(
    entity = build_entity(entity, entity_type),
    role = role
  )

  insert <-
    googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1",
                                   "POST",
                                   path_args = list(b = bucket,
                                                    o = object_name,
                                                    acl = ""))

  req <- insert(path_arguments = list(b = bucket, o = object_name),
                the_body = accessControls)

  if(req$status_code == 200){
    myMessage("Access updated")
    out <- TRUE
  } else {
    stop("Error setting access")
  }

  out

}

#' Check the access control settings for an object for one entity
#'
#' Returns the default object ACL entry for the specified entity on the specified bucket.
#'
#' @param object_name Name of the object
#' @param bucket Name of a bucket
#' @param entity The entity holding the permission. Not needed for entity_type \code{allUsers} or \code{allAuthenticatedUsers}
#' @param entity_type The type of entity
#' @param generation If present, selects a spcfic revision of the object
#'
#' @importFrom googleAuthR gar_api_generator
#' @family Access control functions
#' @export
gcs_get_object_access <- function(object_name,
                                  bucket,
                                  entity = "",
                                  entity_type = c("user",
                                                  "group",
                                                  "domain",
                                                  "project",
                                                  "allUsers",
                                                  "allAuthenticatedUsers"),
                                  generation = NULL){

  entity_type <- match.arg(entity_type)
  entity <- build_entity(entity, entity_type)

  if(is.null(generation)){
    pa <- NULL
  } else {
    pa <- list(generation = generation)
  }

  url <- sprintf("https://www.googleapis.com/storage/v1/b/%s/o/%s/acl/%s",
                 bucket, object_name, entity)
  # storage.objectAccessControls.get
  f <- gar_api_generator(url, "GET",
                         pars_args = pa,
                         data_parse_function = function(x) x)
  f()

}

build_entity <- function(entity, entity_type){
  if(entity_type %in% c("allUsers","allAuthenticatedUsers")){
    ee <- entity_type
  } else {
    ee <- paste0(entity_type,"-",entity)
  }

  ee
}
