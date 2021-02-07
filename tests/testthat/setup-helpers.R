skip_if_no_token <- function() {
  skip_on_cran()
  skip_on_travis()
}

if(file.exists("/workspace/test_auth.json")){
  gcs_auth(json_file="/workspace/test_auth.json")
} else if(file.exists("/Users/mark/dev/auth/keys/googlecloudstorager-tests.json")) {
  gcs_auth(json_file="/Users/mark/dev/auth/keys/googlecloudstorager-tests.json")
} else {
  message("No authentication file found for testing")
}