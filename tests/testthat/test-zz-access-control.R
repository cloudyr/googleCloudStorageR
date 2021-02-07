test_that("We can set access control level for user", {
  skip_on_cran()
  skip_if_no_token()
  
  acl <- gcs_update_object_acl("mtcars.csv", entity = "joe@blogs.com")
  expect_true(acl)
  print(acl)
  
})

test_that("We can set access control level for public", {
  skip_on_cran()
  skip_if_no_token()
  
  acl <- gcs_update_object_acl("mtcars.csv", entity_type = "allUsers")
  expect_true(acl)
  
})

test_that("We can see access control for allUsers", {
  skip_on_cran()
  skip_if_no_token()
  
  gcs_upload(mtcars, bucket = gcs_get_global_bucket(), name = "mtcars.csv")
  
  expect_true(gcs_update_object_acl("mtcars.csv", 
                        bucket = gcs_get_global_bucket(),
                        entity_type = "allUsers"))
  
  acl <- gcs_get_object_acl("mtcars.csv",
                            entity_type = "allUsers")
  expect_equal(acl$kind, "storage#objectAccessControl")
  expect_equal(acl$role, "READER")
  expect_equal(acl$entity, "allUsers")
})

test_that("We can see access control for single test user", {
  skip_on_cran()
  skip_if_no_token()
  
  expect_true(gcs_update_object_acl("mtcars.csv", 
                                    bucket = gcs_get_global_bucket(),
                                    entity = "joe@blogs.com",
                                    entity_type = "user"))
  
  acl <- gcs_get_object_acl("mtcars.csv",
                            entity = "joe@blogs.com")
  expect_equal(acl$kind, "storage#objectAccessControl")
  expect_equal(acl$role, "READER")
  expect_equal(acl$entity, "user-joe@blogs.com")
})

test_that("We can get bucket access data", {
  skip_on_cran()
  skip_if_no_token()
  
  buck_meta <- gcs_get_bucket(projection = "full")
  
  acl <- gcs_get_bucket_acl(entity_type = "project",
                            entity = gsub("project-","",buck_meta$acl$entity[[1]]))
  expect_equal(acl$kind, "storage#bucketAccessControl")
  
})
