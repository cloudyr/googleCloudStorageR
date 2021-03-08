test_that("Object Operations", {
  skip_if_no_token()
  
  upload <- gcs_upload(mtcars, bucket = "blahblahblahfffff", name = "mtcars.csv")
  expect_equal(class(upload), "gcs_objectmeta")
  print(upload)
  del <- gcs_delete_object("mtcars.csv", 
                           bucket = "blahblahblahfffff")
  expect_true(del)
  
  buck <- gcs_get_global_bucket()
  
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
  expect_snapshot(names(bs3))
  
  gcs_upload(mtcars, bucket = buck, name="mtcars.csv")
  
  mtcars <- suppressWarnings(gcs_get_object("mtcars.csv", bucket = buck))
  expect_s3_class(mtcars, "data.frame")
  
  mtcars_meta <- gcs_get_object("mtcars.csv", bucket = buck, meta = TRUE)
  expect_equal(mtcars_meta$kind, "storage#object")
  
  copy <-  gcs_copy_object("mtcars.csv","mtcars2.csv", 
                           source_bucket = buck,
                           destination_bucket = buck)
  expect_equal(copy$kind, "storage#rewriteResponse")
  expect_equal(copy$resource$kind, "storage#object")
  expect_equal(copy$resource$name, "mtcars2.csv")

})

test_that("Compose objects", {
  skip_on_cran()
  skip_if_no_token()
  
  comp <- gcs_compose_objects(c("mtcars.csv","test_mtcars.csv"),
                              destination = "composed_mtcars.csv",
                              bucket = gcs_get_global_bucket())
  expect_equal(comp$kind, "storage#object")
  expect_equal(comp$componentCount, 2)
  
})
