## Google Cloud Storage API host, used by default
.default_storage_host <- "https://storage.googleapis.com"

#' Get the Google Cloud Storage API host to use for requests
#'
#' Uses the \code{STORAGE_EMULATOR_HOST} environment variable if set, otherwise
#' uses the default host (the real Google Cloud Storage API).
#'
#' @return The host to use for requests (includes scheme, host and port)
get_storage_host <- function() {
  Sys.getenv("STORAGE_EMULATOR_HOST", .default_storage_host)
}

#' Check if the Google Cloud Storage API is emulated
#'
#' @return TRUE if the Google Cloud Storage API is emulated, FALSE otherwise
is.storage_emulated <- function() {
  !is.na(Sys.getenv("STORAGE_EMULATOR_HOST", unset = NA))
}
