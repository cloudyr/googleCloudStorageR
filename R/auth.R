#' Authenticate with Google Cloud Storage API 
#'
#' @param json_file Authentication json file you have downloaded from your Google Project
#'
#' @details
#'
#' The best way to authenticate is to use an environment argument pointing at your authentication file.
#'
#' Set the file location of your download Google Project JSON file in a \code{GCS_AUTH_FILE} argument
#'
#' Then, when you load the library you should auto-authenticate
#'
#' However, you can authenticate directly using this function pointing at your JSON auth file.
#'
#' @examples
#'
#' \dontrun{
#' library(googleCloudStorageR)
#' gcs_auth("location_of_json_file.json")
#' }
#' @importFrom googleAuthR gar_auth_service
#' @export
gcs_auth <- function(json_file){

  set_scopes()

  gar_auth_service(json_file = json_file)
}

set_scopes <- function(){
  required_scopes <- c("https://www.googleapis.com/auth/devstorage.full_control",
                       "https://www.googleapis.com/auth/devstorage.read_write",
                       "https://www.googleapis.com/auth/cloud-platform")
  
  op <- getOption("googleAuthR.scopes.selected")
  if(is.null(op)){
    options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.full_control")
  } else if(!any(op %in% required_scopes)){
    myMessage("Adding https://www.googleapis.com/auth/devstorage.full_control to scopes", level = 3)
    options(googleAuthR.scopes.selected = c(op, "https://www.googleapis.com/auth/devstorage.full_control"))
  }
}
