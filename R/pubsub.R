#' Create a pub/sub notification for a bucket
#'
#' Add a notification configuration that sends notifications for all supported events.
#'
#' @param topic The pub/sub topic name
#' @param project The project-id that has the pub/sub topic
#' @param bucket The bucket for notifications
#' @param event_types What events to activate, leave at default for all
#'
#' @details
#'
#' Cloud Pub/Sub notifications allow you to track changes to your Cloud Storage objects.
#' As a minimum you wil need: the Cloud Pub/Sub API activated for the project;
#' sufficient permissions on the bucket you wish to monitor;
#' sufficient permissions on the project to receive notifications;
#' an existing pub/sub topic;
#' have given your service account at least \code{pubsub.publisher} permission.
#'
#' @seealso \url{https://cloud.google.com/storage/docs/reporting-changes}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' project <- "myproject"
#' bucket <- "mybucket"
#'
#' # get the email to give access
#' gcs_get_service_email(project)
#'
#' # once email has access, create a new pub/sub topic for your bucket
#' gcs_create_pubsub("gcs_r", project, bucket)
#'
#' }
#'
#'
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family pubsub functions
gcs_create_pubsub <- function(topic,
                              project,
                              bucket = gcs_get_global_bucket(),
                              event_types = NULL){

  bucket <- as.bucket_name(bucket)
  assert_that(
    is.string(topic),
    is.string(project)
  )

  the_body <- list(
    topic = sprintf("projects/%s/topics/%s", project, topic),
    payload_format = "JSON_API_V1",
    event_types = event_types
  )

  the_body <- rmNullObs(the_body)

  the_url <- sprintf("https://www.googleapis.com/storage/v1/b/%s/notificationConfigs",
                     bucket)

  api <- gar_api_generator(the_url, "POST", data_parse_function = function(x) x)

  api(the_body = the_body)
}

#' Get the email of service account associated with the bucket
#'
#' Use this to get the right email so you can give it \code{pubsub.publisher} permission.
#'
#' @param project The project name containing the bucket
#'
#' @details
#'
#' This service email can be different from the email in the service JSON.  Give this
#' \code{pubsub.publisher} permission in the Google cloud console.
#'
#' @export
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
#' @family pubsub functions
gcs_get_service_email <- function(project){

  assert_that(is.string(project))

  the_url <- sprintf("https://www.googleapis.com/storage/v1/projects/%s/serviceAccount",
                    project)

  api <- gar_api_generator(the_url, "GET",
                           data_parse_function = function(x){x$email_address})

  api()

}

#' List pub/sub notifications for a bucket
#'
#' List notification configurations for a bucket.
#'
#' @param bucket The bucket for notifications
#'
#' @details
#'
#' Cloud Pub/Sub notifications allow you to track changes to your Cloud Storage objects.
#' As a minimum you wil need: the Cloud Pub/Sub API activated for the project;
#' sufficient permissions on the bucket you wish to monitor;
#' sufficient permissions on the project to receive notifications;
#' an existing pub/sub topic; have given your service account at least \code{pubsub.publisher} permission.
#'
#' @seealso \url{https://cloud.google.com/storage/docs/reporting-changes}
#'
#' @export
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family pubsub functions
gcs_list_pubsub <- function(bucket = gcs_get_global_bucket()){

  bucket <- as.bucket_name(bucket)


  the_url <- sprintf("https://www.googleapis.com/storage/v1/b/%s/notificationConfigs",
                     bucket)

  api <- gar_api_generator(the_url, "GET", 
                           data_parse_function = function(x) x$items)

  api()
}

#' Delete pub/sub notifications for a bucket
#'
#' Delete notification configurations for a bucket.
#'
#' @param config_name The ID of the pubsub configuration
#' @param bucket The bucket for notifications
#'
#' @details
#'
#' Cloud Pub/Sub notifications allow you to track changes to your Cloud Storage objects.
#' As a minimum you wil need: the Cloud Pub/Sub API activated for the project;
#' sufficient permissions on the bucket you wish to monitor;
#' sufficient permissions on the project to receive notifications;
#' an existing pub/sub topic; have given your service account at least \code{pubsub.publisher} permission.
#'
#' @seealso \url{https://cloud.google.com/storage/docs/reporting-changes}
#'
#' @export
#' 
#' @return TRUE if successful
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family pubsub functions
gcs_delete_pubsub <- function(config_name,
                              bucket = gcs_get_global_bucket()){

  bucket <- as.bucket_name(bucket)

  assert_that(is.string(config_name))

  the_url <- sprintf("https://www.googleapis.com/storage/v1/b/%s/notificationConfigs/%s",
                     bucket, config_name)
  
  
  # to avoid some warning messages due to empty body
  options(googleAuthR.rawResponse = TRUE)
  on.exit(options(googleAuthR.rawResponse = FALSE))
  api <- suppressMessages(suppressWarnings(
    gar_api_generator(the_url, "DELETE")
    ))

  res <- api()
  
  if(res$status_code != 204){
    stop("Error deleting ", config_name)
  }
  
  TRUE
}
