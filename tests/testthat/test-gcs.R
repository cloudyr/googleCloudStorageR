context("googleCloudStorageR")

skip_if_no_token <- function() {
  testthat::skip_if_not(googleAuthR::gar_has_token(), "No token")
}

if (gargle:::secret_can_decrypt("googleCloudStorageR")) {
  json <- gargle:::secret_read("googleCloudStorageR", "tests_auth.json")
  gcs_auth(json_file = rawToChar(json))
}

test_that("Authentication", {
  skip_if_no_token()
  
  # manual auth
  expect_true(googleAuthR::gar_has_token())
})

test_that("Bucket List", {
  skip_if_no_token()
  
  proj <- Sys.getenv("GCS_DEFAULT_PROJECT")
  expect_true(proj != "")
  b <- gcs_list_buckets(proj)
  
  expect_s3_class(b, "data.frame")
  expect_true(
    all(names(b) %in% c("name","storageClass","location","updated"))
  )
})

test_that("Bucket Operations", {
  skip_if_no_token()
  
  buck <- Sys.getenv("GCS_DEFAULT_BUCKET")
  expect_true(buck != "")
  expect_true(buck == gcs_get_global_bucket())
  
  b <- gcs_get_bucket(buck)
  
  expect_equal(b$kind, "storage#bucket")
})

test_that("Object Operations", {
  skip_if_no_token()
  
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
  expect_equal(mtcars_meta$kind, "storage#object")

})

test_that("Signed URLs", {
  obj1 <- gcs_get_object(
    "LT08/PRE/015/013/LT80150132013127LGN01/LT80150132013127LGN01_MTL.txt",
    meta = TRUE,
    bucket = "gcp-public-data-landsat"
  )
  signed <- gcs_signed_url(obj1)

  temp1 <- tempfile()
  on.exit(unlink(temp1))

  download.file(signed, destfile = temp1)
  expect_true(file.exists(temp1))
  
  obj2 <- gcs_get_object(
    "mtcars.csv",
    meta = TRUE,
    bucket = Sys.getenv("GCS_DEFAULT_BUCKET")
  )
  signed <- gcs_signed_url(obj2)
  
  temp2 <- tempfile()
  on.exit(unlink(temp2))
  
  download.file(signed, destfile = temp2)
  expect_true(file.exists(temp2))
  
})

test_that("Versioning buckets", {
  
  buck <- Sys.getenv("GCS_DEFAULT_BUCKET")
  
  # start with false
  gcs_version_bucket(buck, action = "disable")
  
  expect_false(gcs_version_bucket(buck, action = "status"))
  expect_true(gcs_version_bucket(buck, action = "enable"))
  expect_true(gcs_version_bucket(buck, action = "status"))  
  expect_false(gcs_version_bucket(buck, action = "disable"))  
  expect_false(gcs_version_bucket(buck, action = "status"))
  
  expect_error(gcs_version_bucket(buck, action = "list"),
               "`timeDeleted` property not found")
})