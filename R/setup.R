#' Help set-up googleCloudStorageR
#' 
#' Use this to make a wizard to walk through set-up steps
#' 
#' @seealso \href{https://code.markedmondson.me/googleCloudStorageR/articles/googleCloudStorageR.html}{Setup documentation on googleCloudStorageR website}
#' 
#' @details 
#' 
#' This function assumes you have at least a Google Cloud Platform project setup, from which it can generate the necessary authentication keys and set up authentication.
#' 
#' It uses \link[googleAuthR]{gar_setup_menu} to create the wizard.  You will need to have owner access to the project you are using.
#' 
#' After each menu option has completed, restart R and reload the library and this function to continue to the next step.
#' 
#' Upon successful set-up, you should see a message similar to \code{Successfully auto-authenticated via /xxxx/googlecloudstorager-auth-key.json} and \code{Set default bucket name to 'xxxx'} when you load the library via \code{library(googleCloudStorageR)}
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(googleCloudStorageR)
#' gcs_setup()
#' 
#' }
#' 
#' @export
#' @import googleAuthR 
gcs_setup <- function(){
  
  op <- gar_setup_menu(
    c("Create and download JSON service account key",
      "Setup auto-authentication (JSON service account key)",
      "Setup default bucket"), 
    package_name = "googleCloudStorageR"
  )
  
  session_user <- gar_setup_check_session()
  
  gar_setup_menu_do(op,
                    trigger = 1,
                    do_function = gar_setup_auth_key,
                    session_user = session_user,
                    email = "",
                    roles = "roles/storage.admin",
                    file = "googlecloudstorager-auth-key.json",
                    default_key = "googlecloudstorager")
  
  
  gar_setup_menu_do(op,
        trigger = 2,
        do_function = gar_setup_env_check,
        env_arg = "GCS_AUTH_FILE",
        set_to = gar_setup_get_authenv(session_user = session_user,
                                       env_arg = "GCS_AUTH_FILE"),
        edit_option = TRUE,
        session_user = session_user)
  
  gar_setup_menu_do(op,
                    trigger = 3,
                    do_function = gar_setup_env_check,
                    env_arg = "GCS_DEFAULT_BUCKET",
                    edit_option = TRUE,
                    set_to = get_bucket_setup(),
                    session_user = session_user)
  
}

#' @noRd
#' @return NULL if no changes, ENV_ARG="blah" if change
get_bucket_setup <- function(){
  
  if(!nzchar(Sys.getenv("GCS_AUTH_FILE"))){
    cli::cli_alert_info("You need to setup the auth environment argument before configuring a bucket.  Rerun the wizard once it is setup")
    return(NULL)
  }
  
  the_project <- readline("Which project-id shall the default bucket be in? :")
  
  bucket <- usethis::ui_yeah("Do you want to setup a Cloud Storage bucket?")
  if(bucket){
    has_bucket <- usethis::ui_yeah("Do you have an existing Cloud Storage bucket you want to use?")
    if(has_bucket){
      cli::cli_alert_info(paste("Fetching your buckets under the project-id: ",
                                the_project))
      gcs_auth(Sys.getenv("GCS_AUTH_FILE"))
      bucks <- tryCatch(gcs_list_buckets(the_project),
                        error = function(err){
                          cli::cli_alert_danger("Could not fetch a list of your buckets - {err$message}")
                          return(NULL)
                        })
      print(bucks[ , c("name", "location")])
      the_bucket <- readline("What is the name of your bucket? e.g. my-bucket-name: ")
      check_bucket <- tryCatch(
        gcs_get_bucket(the_bucket),
        error = function(err){
          cli::cli_alert_danger("Could not get bucket: {err$message}")
          return(NULL)
        })
      if(!is.null(check_bucket$kind) && check_bucket$kind == "storage#bucket"){
        cli::cli_alert_info("Validated Cloud Storage bucket")
        return(paste0("GCS_DEFAULT_BUCKET=", the_bucket))
      } else {
        cli::cli_alert_danger("Invalid bucket: {the_bucket}")
        return(NULL)
      }
      
    } else {
      make_bucket <- usethis::ui_yeah("Do you want to make a new Cloud Storage bucket?")
      if(make_bucket){
        make_bucket_name <- readline(
          paste("What name will the bucket be? It will be created in your project: ",
                the_project)
        )
        new_bucket <- gcs_create_bucket(
          make_bucket_name, projectId = the_project
        )
        if(!is.null(new_bucket$kind) && new_bucket$kind == "storage#bucket"){
          cli::cli_alert_success("Successfully created bucket {make_bucket_name}")
          return(paste0("GCS_DEFAULT_BUCKET=", new_bucket))
        }
        
      } else {
        cli::cli_ul("No bucket set")
      }
    }
  }
  
  cli::cli_alert_danger("You will need to set a bucket in functions when no default bucket set, or use gcs_global_bucket()")
  
  NULL
}