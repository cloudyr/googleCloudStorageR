#' Compose up to 32 objects into one
#' 
#' This merges objects stored on Cloud Storage into one object.
#' 
#' @param objects A character vector of object names to combine
#' @param destination Name of the new object.
#' @param bucket The bucket where the objects sit
#' 
#' @return Object metadata
#' @family object functions
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
#' @export
#' @seealso \href{Compose objects}{https://cloud.google.com/storage/docs/json_api/v1/objects/compose}
#' 
#' @examples 
#' 
#' \dontrun{
#'  gcs_global_bucket("your-bucket")
#'  objs <- gcs_list_objects()
#' 
#' }
gcs_compose_objects <- function(objects,
                                destination,
                                bucket = gcs_get_global_bucket()){
  
  assert_that(
    is.character(objects),
    is.string(destination),
    length(objects) <= 32
  )
  
  objects <- unlist(lapply(objects, URLencode, reserved = TRUE))
  destination <- URLencode(destination, reserved = TRUE)
  
  body <- list(
    kind = "storage#composeRequest",
    sourceObjects = list(
      objects
    ),
    destination = list(
      kind = "storage#object",
      name = destination,
      bucket = bucket
    )
  )
  
  ob <- gar_api_generator("https://www.googleapis.com/storage/v1",
                         "POST",
                         path_args = list(b = bucket,
                                          o = destination,
                                          compose = ""),
                         data_parse_function = function(x) x)
  
  ob(the_body = body)
  
}


