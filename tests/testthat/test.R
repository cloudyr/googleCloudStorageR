library(googleAuthR)
library(testthat)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.full_control")
library(googleCloudStorageR)

context("Auth")

test_that("We can login", {

  ## requires pre-auth at /tests/testthat/.httr-oauth
  expect_is(googleAuthR::gar_auth(), "Token2.0")

})

test_that("We can set global bucket names", {

  bucket <- gcs_global_bucket("mark-edmondson-public-files")
  expect_true(bucket == "mark-edmondson-public-files")
})

context("Uploading")

upload_test <- function(filename){

  googleAuthR::gar_auth()
  # write.csv(mtcars, file = filename)
  gcs_upload(filename)
}

upload_obj <- function(obj){

  googleAuthR::gar_auth()
  gcs_upload(obj)
}

test_that("We can upload a file", {

  upload <- upload_test("test_mtcars.csv")
  expect_equal(upload$kind, "storage#object")

})

test_that("We can upload an data.frame", {

  upload <- upload_obj(mtcars)
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "obj.csv")
})

test_that("We can upload an list", {

  upload <- upload_obj(list(a = 1, b = 3, c = list(a = 3, g = 5)))
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "obj.json")
})

test_that("We can upload using a custom function", {

  f <- function(input, output) write.csv(input, row.names = FALSE, file = output)

  upload <- gcs_upload(mtcars,
                       object_function = f,
                       type = "text/csv")
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$size, "1303")
  expect_equal(upload$name, "mtcars")
})

test_that("We can upload using resumable", {

  upload <- gcs_upload(mtcars, upload_type = "resumable")
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "mtcars.csv")
})

test_that("We can upload with metadata", {

  meta <- gcs_metadata_object("mtcars_meta.csv", metadata = list(blah = 1,bo = 2))
  upload <- gcs_upload(mtcars, object_metadata = meta)

  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "mtcars_meta.csv")
})

context("Downloading")

test_that("We can download a file to disk", {

  worked <- gcs_get_object("mtcars_meta.csv", saveToDisk = "mtcars.csv")
  expect_true(worked)
  unlink("mtcars.csv")

})

test_that("We can download a file directly", {

  dl <- gcs_get_object("mtcars_meta.csv")
  expect_s3_class(dl, "data.frame")

})

context("Access Control")

test_that("We can set access control level for user", {

  acl <- gcs_update_acl("mtcars.csv", entity = "joe@blogs.com")
  expect_true(acl)

})

test_that("We can set access control level for public", {

  acl <- gcs_update_acl("mtcars.csv", entity_type = "allUsers")
  expect_true(acl)

})

test_that("We can create a download URL", {

  durl <- gcs_download_url("test_mtcars.csv")
  expect_equal(durl,
               "https://storage.cloud.google.com/mark-edmondson-public-files/test_mtcars.csv")

})

test_that("We can see access control for allUsers,", {

  acl <- gcs_get_object_access("mtcars.csv",
                               entity_type = "allUsers")
  expect_equal(acl$kind, "storage#objectAccessControl")
  expect_equal(acl$role, "READER")
  expect_equal(acl$entity, "allUsers")
})

test_that("We can see access control for single test user,", {

  acl <- gcs_get_object_access("mtcars.csv",
                               entity = "joe@blogs.com")
  expect_equal(acl$kind, "storage#objectAccessControl")
  expect_equal(acl$role, "READER")
  expect_equal(acl$entity, "user-joe@blogs.com")
})

context("Meta data")

test_that("We can see object meta data", {

  meta_obj <- gcs_get_object("mtcars.csv", meta = TRUE)
  expect_equal(meta_obj$kind, "storage#object")
})

context("R session functions")

test_that("We can save the R session", {
  a <- 1
  b <- "test"
  saved <- gcs_save()
  expect_true(saved)
})

test_that("We can reload the R session", {

  a <- 1
  b <- "test"
  saved <- gcs_save()
  expect_true(saved)
  rm(a,b)

  loaded <- gcs_load()
  expect_true(loaded)
  a <- get("a")
  b <- get("b")
  expect_true(a == 1)
  expect_true(b == "test")
})

context("Deleting")

test_that("We can delete all test files", {

  expect_true(gcs_delete_object("obj.json"))
  expect_true(gcs_delete_object("obj.csv"))
  expect_true(gcs_delete_object("mtcars"))
  expect_true(gcs_delete_object("mtcars.csv"))
  expect_true(gcs_delete_object("mtcars_meta.csv"))

})

