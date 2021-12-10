test_that("We can delete a bucket", {
  skip_on_cran()
  skip_if_no_token()
  
  deleted <- gcs_delete_bucket("blahblahblahfffff")
  expect_true(deleted)
  
})

test_that("We can delete all test files", {
  skip_on_cran()
  skip_if_no_token()
  
  expect_true(gcs_delete_object("a_list.json"))
  expect_true(gcs_delete_object("mtcars"))
  expect_true(gcs_delete_object("mtcars.csv"))
  expect_true(gcs_delete_object("test_mtcars.csv"))
  expect_true(gcs_delete_object("mtcars_meta.csv"))
  expect_true(gcs_delete_object("example.R"))
  expect_true(gcs_delete_object("gcs_save_test.RData"))
  expect_true(gcs_delete_object(".RData_test"))
  expect_true(gcs_delete_object("mtcars2.csv"))
})

test_that("We can delete the lifecycle bucket", {
  skip_on_cran()
  skip_if_no_token()
  
  deleted <- gcs_delete_bucket("blahblahblahffflifecycle")
  expect_true(deleted)
  
})

test_that("We can create and delete a versioned bucket",{
  skip_on_cran()
  skip_if_no_token()
  proj <- Sys.getenv("GCS_DEFAULT_PROJECT")
  bucket <- paste0("gcsr-test-bucket-version-",format(Sys.time(), format="%Y%m%d%H%M"))
  bbs <- gcs_create_bucket(bucket, projectId = proj, versioning = TRUE)
  
  tmp <- tempfile()
  writeLines("first", tmp)
  head_first <- gcs_upload(
    file = tmp,
    bucket = bucket,
    object_metadata = gcs_metadata_object(
       metadata = list("custom" = "first-meta"),
       object_name = "x"
  ))
  
  v1 <- head_first$generation
  writeLines("second", tmp)
  head_second <- gcs_upload(
    file = tmp,
    bucket = bucket,
    object_metadata = gcs_metadata_object(
      metadata = list("custom" = "second-meta"),
      object_name = "x"
    ))
  v2 <- head_second$generation

  unlink(tmp)
  gcs_get_object(object_name = "x", bucket = bucket, 
                 saveToDisk = tmp, generation  = v1)
  expect_equal(readLines(tmp), "first")
  gcs_get_object(object_name = "x", bucket = bucket, 
                 saveToDisk = tmp, generation  = v2, 
                 overwrite = TRUE)
  expect_equal(readLines(tmp), "second")
  
  gcs_delete_bucket(bucket, force_delete = TRUE)
  
  buckets <- gcs_list_buckets(proj)
  expect_false(bucket %in% buckets$name)
  
})
