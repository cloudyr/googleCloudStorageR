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

test_that("Bucket List", {
  googleAuthR::skip_if_no_env_auth(
    "GCS_DEFAULT_PROJECT"
  )
  
  proj <- Sys.getenv("GCS_DEFAULT_PROJECT")
  expect_true(proj != "")
  b <- gcs_list_buckets(proj)
  
  expect_s3_class(b, "data.frame")
  expect_true(
    all(names(b) %in% c("name","storageClass","location","updated"))
  )
})

test_that("Bucket Operations", {
  googleAuthR::skip_if_no_env_auth(
    "GCS_DEFAULT_BUCKET"
  )
  
  buck <- Sys.getenv("GCS_DEFAULT_BUCKET")
  expect_true(buck != "")
  expect_true(buck == gcs_get_global_bucket())
  
  b <- gcs_get_bucket(buck)
  
  expect_equal(b$kind, "storage#bucket")
})

test_that("Object Operations", {
  googleAuthR::skip_if_no_env_auth(
    "GCS_DEFAULT_BUCKET"
  )
  
  buck <- Sys.getenv("GCS_DEFAULT_BUCKET")
  
  bs <- gcs_list_objects(bucket = buck)
  expect_s3_class(bs, "data.frame")
  expect_true(
    all(names(bs) %in% c("name","size","updated"))
  )
  
  bs2 <- gcs_list_objects(bucket = buck, detail = "more")
  expect_s3_class(bs2, "data.frame")
  expect_true(
    all(names(bs2) %in% c("name", 
                          "size", 
                          "bucket", 
                          "contentType", 
                         "timeCreated", 
                         "updated", 
                         "storageClass"))
  )
  
  bs3 <- gcs_list_objects(bucket = buck, detail = "full")
  expect_s3_class(bs3, "data.frame")
  expect_true(
    all(names(bs3) %in% c('id', 'selfLink', 'name', 'bucket', 'generation', 
                          'metageneration', 'timeCreated', 'updated', 
                          'storageClass', 'timeStorageClassUpdated', 
                          'size', 'md5Hash', 'mediaLink', 'crc32c', 
                          'etag', 'contentType', 
                          'componentCount', 'contentLanguage'))
  )
  
  mtcars <- gcs_get_object("mtcars.csv", bucket = buck)
  expect_s3_class(mtcars, "data.frame")
  
  mtcars_meta <- gcs_get_object("mtcars.csv", bucket = buck, meta = TRUE)

})