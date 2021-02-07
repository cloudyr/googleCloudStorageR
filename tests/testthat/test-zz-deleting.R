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
