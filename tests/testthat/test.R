library(testthat)
library(googleCloudStorageR)

context("Auth")

test_that("We can login", {
  skip_on_cran()
  ## requires pre-auth at /tests/testthat/.httr-oauth
  ## or setting environment var GCS_AUTH_FILE and TRAVIS_GCS_AUTH_FILE
  expect_is(gcs_auth(), "Token2.0")

})

context("Buckets")

test_that("We can set global bucket names", {
  skip_on_cran()
  bucket <- gcs_global_bucket("mark-edmondson-public-files")
  expect_true(bucket == "mark-edmondson-public-files")
})

test_that("We can list buckets", {
  skip_on_cran()
  bb <- gcs_list_buckets("mark-edmondson-gde")
  expect_s3_class(bb, "data.frame")

})

test_that("We can get a bucket", {
  skip_on_cran()
  bb <- gcs_get_bucket()
  print(bb)

  expect_equal(bb$kind, "storage#bucket")

})

test_that("We can create a bucket", {
  skip_on_cran()
  new_bucket <- gcs_create_bucket("blahblahblahfffff",
                                  projectId = "mark-edmondson-gde")
  expect_equal(new_bucket$kind, "storage#bucket")

})

test_that("We can delete a bucket", {
  skip_on_cran()
  deleted <- gcs_delete_bucket("blahblahblahfffff")
  expect_true(deleted)

})

test_that("We can make a bucket with lifecycle and versioning set",{
  skip_on_cran()
  lc1 <- gcs_create_lifecycle(age = 365)
  lc2 <- gcs_create_lifecycle(numNewerVersions = 3)

  life_cyclebucket <- gcs_create_bucket("blahblahblahffflifecycle",
                                        projectId = "mark-edmondson-gde",
                                        lifecycle = list(lc1, lc2),
                                        versioning = TRUE,
                                        predefinedAcl = "authenticatedRead")

  expect_equal(life_cyclebucket$kind, "storage#bucket")
  expect_equal(life_cyclebucket$lifecycle$rule$action$type, c("Delete","Delete"))
  expect_equal(life_cyclebucket$lifecycle$rule$condition$age, c(365,NA))
  expect_equal(life_cyclebucket$lifecycle$rule$condition$numNewerVersions, c(NA,3))
  expect_true(life_cyclebucket$versioning$enabled)

})

test_that("We can delete the lifecycle bucket", {
  skip_on_cran()
  deleted <- gcs_delete_bucket("blahblahblahffflifecycle")
  expect_true(deleted)

})

context("Uploading")

test_that("We can upload a file", {
  skip_on_cran()
  upload <- gcs_upload("test_mtcars.csv")
  print(upload)
  expect_equal(upload$kind, "storage#object")

})

test_that("We can upload a data.frame", {
  skip_on_cran()
  upload <- gcs_upload(mtcars)
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "mtcars.csv")
})

test_that("We can upload an list", {
  skip_on_cran()
  a_list <- list(a = 1, b = 3, c = list(a = 3, g = 5))
  upload <- gcs_upload(a_list)
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "a_list.json")
})

test_that("We can upload using a custom function", {
  skip_on_cran()
  f <- function(input, output) write.csv(input, row.names = FALSE, file = output)

  upload <- gcs_upload(mtcars,
                       object_function = f,
                       type = "text/csv")
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$size, "1303")
  expect_equal(upload$name, "mtcars")
})

test_that("We can upload using resumable", {
  skip_on_cran()
  upload <- gcs_upload(mtcars, upload_type = "resumable")
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "mtcars.csv")
})

test_that("We can upload with metadata", {
  skip_on_cran()
  meta <- gcs_metadata_object("mtcars_meta.csv", metadata = list(blah = 1,bo = 2))
  upload <- gcs_upload(mtcars, object_metadata = meta)
  print(upload)

  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "mtcars_meta.csv")
})

context("Downloading")

test_that("We can download a file to disk", {
  skip_on_cran()
  worked <- gcs_get_object("mtcars_meta.csv", saveToDisk = "mtcars.csv")
  expect_true(worked)
  unlink("mtcars.csv")

})

test_that("We can download a file directly", {
  skip_on_cran()
  dl <- gcs_get_object("mtcars_meta.csv")
  expect_s3_class(dl, "data.frame")

})

context("Access Control")

test_that("We can set access control level for user", {
  skip_on_cran()
  acl <- gcs_update_object_acl("mtcars.csv", entity = "joe@blogs.com")
  expect_true(acl)

})

test_that("We can set access control level for public", {
  skip_on_cran()
  acl <- gcs_update_object_acl("mtcars.csv", entity_type = "allUsers")
  expect_true(acl)

})

test_that("We can create a download URL", {
  skip_on_cran()
  durl <- gcs_download_url("test_mtcars.csv")
  expect_equal(durl,
               "https://storage.cloud.google.com/mark-edmondson-public-files/test_mtcars.csv")

})

test_that("We can see access control for allUsers,", {
  skip_on_cran()
  acl <- gcs_get_object_acl("mtcars.csv",
                            entity_type = "allUsers")
  print(acl)
  expect_equal(acl$kind, "storage#objectAccessControl")
  expect_equal(acl$role, "READER")
  expect_equal(acl$entity, "allUsers")
})

test_that("We can see access control for single test user,", {
  skip_on_cran()
  acl <- gcs_get_object_acl("mtcars.csv",
                            entity = "joe@blogs.com")
  expect_equal(acl$kind, "storage#objectAccessControl")
  expect_equal(acl$role, "READER")
  expect_equal(acl$entity, "user-joe@blogs.com")
})

context("Meta data")

test_that("We can see object meta data", {
  skip_on_cran()
  meta_obj <- gcs_get_object("mtcars.csv", meta = TRUE)
  print(meta_obj)
  expect_equal(meta_obj$kind, "storage#object")
})

context("R session functions")

test_that("We can save an R object list", {
  skip_on_cran()
  cc <- 3
  d <- "test1"
  saved <- gcs_save("cc","d", file = "gcs_save_test.RData")
  expect_true(saved)
  unlink("gcs_save_test.RData")
})

test_that("We can load an R object list", {
  skip_on_cran()
  saved <- gcs_load(file = "gcs_save_test.RData")
  on.exit(unlink("gcs_save_test.RData"))
  expect_true(saved)
  cc <- get("cc")
  d <- get("d")
  expect_true(cc == 3)
  expect_true(d == "test1")

})

test_that("We can save the R session", {
  skip_on_cran()
  a <- 1
  b <- "test"
  saved <- gcs_save_image(file = ".RData_test")
  expect_true(saved)
  unlink(".RData_test")
})


test_that("We can load the R session", {
  skip_on_cran()
  loaded <- gcs_load(file = ".RData_test", envir = parent.frame())
  on.exit(unlink(".RData_test"))
  expect_true(loaded)
  a <- get("a", envir = parent.frame())
  b <- get("b", envir = parent.frame())
  expect_true(a == 1)
  expect_true(b == "test")

})

context("Source files")

test_that("We can upload a source file", {
  skip_on_cran()
  cat("x <- 'hello world!'\nx", file = "example.R")
  up <- gcs_upload("example.R")
  expect_true(up$kind == "storage#object")
  unlink("example.R")

})

test_that("We can source the uploaded file", {
  skip_on_cran()
  gcs_source("example.R")
  expect_true(exists("x"))

})

context("Deleting")

test_that("We can delete all test files", {
  skip_on_cran()
  expect_true(gcs_delete_object("a_list.json"))
  expect_true(gcs_delete_object("mtcars"))
  expect_true(gcs_delete_object("mtcars.csv"))
  expect_true(gcs_delete_object("test_mtcars.csv"))
  expect_true(gcs_delete_object("mtcars_meta.csv"))
  expect_true(gcs_delete_object("example.R"))
  expect_true(gcs_delete_object("gcs_save_test.RData"))
  expect_true(gcs_delete_object(".RData_test"))
})


