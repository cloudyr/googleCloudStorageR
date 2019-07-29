context("googleCloudStorageR")

test_that("Authentication", {
  googleAuthR::skip_if_no_env_auth(
    "GCS_AUTH_FILE"
  )
  expect_true(file.exists(Sys.getenv("GCS_AUTH_FILE")))
  
  # auto-auth
  expect_true(googleAuthR::gar_has_token())
  
  gcs_auth(Sys.getenv("GCS_AUTH_FILE"))
  
  # manual auth
  expect_true(googleAuthR::gar_has_token())
})

test_that("Buckets", {
  b <- gcs_list_buckets("iih-tools-analytics")
  expect_s3_class(b, "data.frame")
  expect_true(
    all(names(b) %in% c("name","storageClass","location","updated"))
  )
})