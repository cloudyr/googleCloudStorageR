library(googleAuthR)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.full_control")
library(googleCloudStorageR)

context("Auth")

test_that("We can login", {

  ## requires pre-auth at /tests/testthat/.httr-oauth
  expect_is(googleAuthR::gar_auth(), "Token2.0")

})

context("Uploading")

upload_test <- function(filename){

  googleAuthR::gar_auth()
  # write.csv(mtcars, file = filename)
  gcs_upload(filename, "mark-edmondson-public-files")
}

upload_obj <- function(obj){

  googleAuthR::gar_auth()
  gcs_upload(obj, "mark-edmondson-public-files")
}

test_that("We can upload a file", {

  upload <- upload_test("test_mtcars.csv")
  expect_equal(upload$kind, "storage#object")

})

test_that("We can upload an data.frame", {

  upload <- upload_obj(mtcars)
  expect_equal(upload$kind, "storage#object")

})

test_that("We can upload an list", {

  upload <- upload_obj(list(a = 1, b = 3, c = list(a = 3, g = 5)))
  expect_equal(upload$kind, "storage#object")

})

context("Downloading")

download_test <- function(direct = NULL){

  googleAuthR::gar_auth()
  filename <- "test_mtcars.csv"
  gcs_get_object(filename, "mark-edmondson-public-files", saveToDisk = direct)
}

test_that("We can download a file to disk", {

  worked <- download_test("dl_mtcars.csv")
  expect_true(worked)

})

test_that("We can download a file directly", {

  dl <- download_test()
  expect_s3_class(dl, "data.frame")

})

context("Access Control")

test_that("We can set access control level for user", {

  acl <- gcs_update_acl("test_mtcars.csv", "mark-edmondson-public-files", "joe@blogs.com")
  expect_true(acl)

})

test_that("We can set access control level for public", {

  acl <- gcs_update_acl("test_mtcars.csv", "mark-edmondson-public-files", entity_type = "allUsers")
  expect_true(acl)

})

test_that("We can create a download URL", {

  durl <- gcs_download_url("test_mtcars.csv", "mark-edmondson-public-files")
  expect_equal(durl,
               "https://storage.cloud.google.com/mark-edmondson-public-files/test_mtcars.csv")

})

test_that("We can see access control for allUsers,", {

  acl <- gcs_get_object_access("test_mtcars.csv", "mark-edmondson-public-files",
                               entity_type = "allUsers")
  expect_equal(acl$kind, "storage#objectAccessControl")
  expect_equal(acl$role, "READER")
  expect_equal(acl$entity, "allUsers")
})

test_that("We can see access control for single test user,", {

  acl <- gcs_get_object_access("test_mtcars.csv", "mark-edmondson-public-files",
                               entity = "joe@blogs.com")
  expect_equal(acl$kind, "storage#objectAccessControl")
  expect_equal(acl$role, "READER")
  expect_equal(acl$entity, "user-joe@blogs.com")
})

context("Meta data")

test_that("We can see object meta data", {

  meta_obj <- gcs_get_object("test_mtcars.csv", "mark-edmondson-public-files", meta = TRUE)
  expect_equal(meta_obj$kind, "storage#object")
})
