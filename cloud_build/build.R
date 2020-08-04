library(googleCloudRunner)

cr_deploy_pkgdown(
  "cloudyr/googleCloudStorageR",
  secret = "github-ssh",
  create_trigger = "inline"
)

cr_deploy_packagetests(
  steps = cr_buildstep_secret("googlecloudstorager-tests", "test_auth.json"),
  cloudbuild_file = "cloud_build/cloudbuild-tests.yml",
  env = c("NOT_CRAN=true", 
          "GCS_DEFAULT_PROJECT=$PROJECT_ID",
          "GCS_DEFAULT_BUCKET=mark-edmondson-public-files",
          "GCS_AUTH_FILE=test_auth.json"),
  codecov_token = "$_CODECOV_TOKEN",
  create_trigger = "inline",
  trigger_repo = cr_buildtrigger_repo("cloudyr/googleCloudStorageR")
)
