#' List buckets
#'
#' @param projectId Project containing buckets to list
#' @param prefix Filter results to names begnning with this prefix
#' @param projection Properties to return. Default noAcl omits acl properties
#' @param maxResults Max number of results
#'
#' @export
gcs_list_buckets <- function(projectId,
                             prefix = "",
                             projection = c("noAcl","full"),
                             maxResults = 1000){

  projection <- match.arg(projection)

  testthat::expect_is(projectId, "character")
  testthat::expect_is(prefix, "character")
  testthat::expect_is(projection, "character")
  testthat::expect_is(maxResults, "numeric")

  ## simple upload <5MB
  lb <-
    googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/b",
                                   "GET",
                                   pars_args = list(project=projectId,
                                                    prefix=prefix,
                                                    projection=projection))
  req <- lb()

  req$content$items

}
