library(googleCloudRunner)

cr_deploy_pkgdown(
  "cloudyr/googleCloudStorageR",
  secret = "github-ssh",
  create_trigger = "inline"
)
