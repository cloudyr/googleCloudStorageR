test_that("Bucket List", {
  skip_if_no_token()
  
  proj <- Sys.getenv("GCS_DEFAULT_PROJECT")
  expect_true(proj != "")
  b <- gcs_list_buckets(proj)
  
  expect_s3_class(b, "data.frame")
  expect_true(
    all(names(b) %in% c("name","storageClass","location","updated"))
  )
  
  no_buckets <- gcs_list_buckets("me-gtm-monitoring")
  expect_true(is.null(no_buckets))
})



test_that("Bucket Operations", {
  skip_if_no_token()
  
  buck <- Sys.getenv("GCS_DEFAULT_BUCKET")
  expect_true(nzchar(buck))
  expect_equal(buck, "mark-edmondson-public-files")
  expect_true(buck == gcs_get_global_bucket())
  
  b <- gcs_get_bucket(buck)
  print(b)
  expect_equal(b$kind, "storage#bucket")
  
  new_bucket <- gcs_create_bucket("blahblahblahfffff",
                                  projectId = Sys.getenv("GCS_DEFAULT_PROJECT"))
  expect_equal(new_bucket$kind, "storage#bucket")
  
})

test_that("We can set global bucket names", {
  skip_on_cran()
  skip_if_no_token()
  buck <- Sys.getenv("GCS_DEFAULT_BUCKET")
  
  bucket <- gcs_global_bucket(buck)
  expect_true(bucket == buck)
})


test_that("We can make a bucket with lifecycle and versioning set",{
  skip_if_no_token()
  
  lc1 <- gcs_create_lifecycle(age = 365)
  lc2 <- gcs_create_lifecycle(numNewerVersions = 3)
  
  life_cyclebucket <- gcs_create_bucket("blahblahblahffflifecycle",
                                        projectId = Sys.getenv("GCS_DEFAULT_PROJECT"),
                                        lifecycle = list(lc1, lc2),
                                        versioning = TRUE,
                                        predefinedAcl = "authenticatedRead")
  
  expect_equal(life_cyclebucket$kind, "storage#bucket")
  expect_equal(life_cyclebucket$lifecycle$rule$action$type, c("Delete","Delete"))
  expect_equal(life_cyclebucket$lifecycle$rule$condition$age, c(365,NA))
  expect_equal(life_cyclebucket$lifecycle$rule$condition$numNewerVersions, c(NA,3))
  expect_true(life_cyclebucket$versioning$enabled)
  
})

test_that("Versioning buckets", {
  skip_if_no_token()
  
  buck <- Sys.getenv("GCS_DEFAULT_BUCKET")
  
  # start with false
  gcs_version_bucket(buck, action = "disable")
  
  expect_false(gcs_version_bucket(buck, action = "status"))
  expect_true(gcs_version_bucket(buck, action = "enable"))
  expect_true(gcs_version_bucket(buck, action = "status"))  
  expect_false(gcs_version_bucket(buck, action = "disable"))  
  expect_false(gcs_version_bucket(buck, action = "status"))
  
  df <- gcs_version_bucket(buck, action = "list")
  expect_equal(class(df), "data.frame")
})