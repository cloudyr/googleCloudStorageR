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
  write.csv(mtcars, file = filename)
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
  testthat::expect_s3_class(dl, "data.frame")

})
