context("setup")

skip_if_no_token <- function() {
  testthat::skip_if_not(googleAuthR::gar_has_token(), "No token")
}

if (gargle:::secret_can_decrypt("googleCloudStorageR")) {
  json <- gargle:::secret_read("googleCloudStorageR", "tests_auth.json")
  gcs_auth(json_file = rawToChar(json))
  tmp <- tempfile(fileext = ".json")
  write(rawToChar(json), file = tmp)
  Sys.setenv("GCS_AUTH_FILE" = tmp)
}

context("Authentication")

test_that("Authentication", {
  skip_if_no_token()
  
  # manual auth
  expect_true(googleAuthR::gar_has_token())
})

context("Buckets")

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
  print(b)
  expect_equal(b$kind, "storage#bucket")
  
  new_bucket <- gcs_create_bucket("blahblahblahfffff",
                                  projectId = Sys.getenv("GCS_DEFAULT_PROJECT"))
  expect_equal(new_bucket$kind, "storage#bucket")

})

test_that("We can set global bucket names", {
  skip_on_cran()
  skip_if_no_token()
  buck <- Sys.getenv("GCS_DEFAULT_BUCKET")
  
  bucket <- gcs_global_bucket(buck)
  expect_true(bucket == buck)
})


test_that("We can make a bucket with lifecycle and versioning set",{
  skip_if_no_token()
  
  lc1 <- gcs_create_lifecycle(age = 365)
  lc2 <- gcs_create_lifecycle(numNewerVersions = 3)
  
  life_cyclebucket <- gcs_create_bucket("blahblahblahffflifecycle",
                                        projectId = Sys.getenv("GCS_DEFAULT_PROJECT"),
                                        lifecycle = list(lc1, lc2),
                                        versioning = TRUE,
                                        predefinedAcl = "authenticatedRead")
  
  expect_equal(life_cyclebucket$kind, "storage#bucket")
  expect_equal(life_cyclebucket$lifecycle$rule$action$type, c("Delete","Delete"))
  expect_equal(life_cyclebucket$lifecycle$rule$condition$age, c(365,NA))
  expect_equal(life_cyclebucket$lifecycle$rule$condition$numNewerVersions, c(NA,3))
  expect_true(life_cyclebucket$versioning$enabled)
  
})

test_that("Versioning buckets", {
  skip_if_no_token()
  
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

context("Objects")

test_that("Object Operations", {
  skip_if_no_token()
  
  upload <- gcs_upload(mtcars, bucket = "blahblahblahfffff", name = "mtcars.csv")
  expect_equal(class(upload), "gcs_objectmeta")
  print(upload)
  del <- gcs_delete_object("mtcars.csv", bucket = "blahblahblahfffff")
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
  expect_true(
    all(names(bs3) %in% c('id', 'selfLink', 'name', 'bucket', 'generation', 
                          'metageneration', 'timeCreated', 'updated', 
                          'storageClass', 'timeStorageClassUpdated', 
                          'size', 'md5Hash', 'mediaLink', 'crc32c', 
                          'etag', 'contentType', 
                          'componentCount', 'contentLanguage'))
  )
  
  gcs_upload(mtcars, bucket = buck, name="mtcars.csv")
  
  mtcars <- suppressWarnings(gcs_get_object("mtcars.csv", bucket = buck))
  expect_s3_class(mtcars, "data.frame")
  
  mtcars_meta <- gcs_get_object("mtcars.csv", bucket = buck, meta = TRUE)
  expect_equal(mtcars_meta$kind, "storage#object")

})

context("Downloads")

test_that("Signed URLs", {
  skip_if_no_token()
  
  obj1 <- gcs_get_object(
    "LT08/PRE/015/013/LT80150132013127LGN01/LT80150132013127LGN01_MTL.txt",
    meta = TRUE,
    bucket = "gcp-public-data-landsat"
  )
  signed <- gcs_signed_url(obj1)

  temp1 <- tempfile()
  on.exit(unlink(temp1))

  download.file(signed, destfile = temp1, quiet = TRUE)
  expect_true(file.exists(temp1))
  
  gcs_upload(mtcars, 
             bucket = Sys.getenv("GCS_DEFAULT_BUCKET"),
             name = "mtcars.csv")
  
  obj2 <- gcs_get_object(
    "mtcars.csv",
    meta = TRUE,
    bucket = Sys.getenv("GCS_DEFAULT_BUCKET")
  )
  signed <- gcs_signed_url(obj2)
  
  temp2 <- tempfile()
  on.exit(unlink(temp2))
  
  download.file(signed, destfile = temp2, quiet = TRUE)
  expect_true(file.exists(temp2))
  
})

context("Uploads")

test_that("Uploads", {
  skip_if_no_token()
  
  write.csv(mtcars, file = "test_mtcars.csv")
  on.exit(unlink("test_mtcars.csv"))

  upload <- gcs_upload("test_mtcars.csv")
  expect_equal(upload$kind, "storage#object")
  
  upload <- gcs_upload(mtcars)
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "mtcars.csv")
  
  a_list <- list(a = 1, b = 3, c = list(a = 3, g = 5))
  upload <- gcs_upload(a_list)
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "a_list.json")
  
  f <- function(input, output) write.csv(input, row.names = FALSE, file = output)
  
  upload <- gcs_upload(mtcars,
                       object_function = f,
                       type = "text/csv")
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$size, "1303")
  expect_equal(upload$name, "mtcars")
  
  
  upload <- suppressWarnings(gcs_upload(mtcars, upload_type = "resumable"))
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "mtcars.csv")
  
  meta <- gcs_metadata_object("mtcars_meta.csv", metadata = list("Content-Language" = "en", blah = 2))
  print(meta)
  upload <- gcs_upload(mtcars, object_metadata = meta)
  
  expect_equal(upload$kind, "storage#object")
  expect_equal(upload$name, "mtcars_meta.csv")
})


test_that("We can supply our own parseFunction to downloads", {
  skip_on_cran()
  skip_if_no_token()
  
  f <- function(object){
    out <- suppressWarnings(httr::content(object, encoding = "UTF-8"))
    message("Object parsed to class: ", paste(class(out), collapse = " "))
    out
  }
  
  gcs_upload(mtcars, name = "mtcars_meta.csv")
  
  dl <- gcs_get_object("mtcars_meta.csv", parseFunction = f)
  expect_s3_class(dl, "data.frame")
  print(dl)
})

test_that("We can download via a gs:// link", {
  skip_on_cran()
  skip_if_no_token()
  
  gcs_upload(mtcars, name = "mtcars_meta.csv")
  meta <- gcs_get_object("mtcars_meta.csv", meta = TRUE)
  dl <- suppressWarnings(gcs_get_object("mtcars_meta.csv"))
  gs <- suppressWarnings(gcs_get_object(paste0("gs://",meta$bucket,"/", meta$name)))
  
  expect_equal(dl, gs)
  
})

context("Compose")

test_that("Compose objects", {
  skip_on_cran()
  skip_if_no_token()
  
  comp <- gcs_compose_objects(c("mtcars.csv","test_mtcars.csv"),
                      destination = "composed_mtcars.csv",
                      bucket = gcs_get_global_bucket())
  expect_equal(comp$kind, "storage#object")
  expect_equal(comp$componentCount, 2)
  
})

context("Access Control")

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

test_that("We can create a download URL", {
  skip_on_cran()
  skip_if_no_token()
  
  gcs_upload(mtcars, bucket = gcs_get_global_bucket(), name = "mtcars_meta.csv")
  
  durl <- gcs_download_url("mtcars_meta.csv")
  expect_equal(durl,
               sprintf("https://storage.cloud.google.com/%s/mtcars_meta.csv",
                       gcs_get_global_bucket()))
  
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

context("R session functions")

test_that("We can save an R object list", {
  skip_on_cran()
  skip_if_no_token()
  
  cc <- 3
  d <- "test1"
  saved <- gcs_save("cc","d", file = "gcs_save_test.RData")
  expect_equal(saved$kind, "storage#object")

})

test_that("We can load an R object list", {
  skip_on_cran()
  skip_if_no_token()
  
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
  skip_if_no_token()
  
  a <- 1
  b <- "test"
  saved <- gcs_save_image(file = ".RData_test")
  expect_equal(saved$kind, "storage#object")
  unlink(".RData_test")
})


context("Source files")

test_that("We can upload a source file", {
  skip_on_cran()
  skip_if_no_token()
  
  cat("x <- 'hello world!'\nx", file = "example.R")
  up <- gcs_upload("example.R")
  expect_true(up$kind == "storage#object")
  unlink("example.R")
  
})



test_that("We can load the R session", {
  skip_on_cran()
  skip_if_no_token()
  
  loaded <- gcs_load(file = ".RData_test", envir = parent.frame())
  on.exit(unlink(".RData_test"))
  expect_true(loaded)
  a <- get("a", envir = parent.frame())
  b <- get("b", envir = parent.frame())
  expect_true(a == 1)
  expect_true(b == "test")
  
})

test_that("We can source the uploaded file", {
  skip_on_cran()
  skip_if_no_token()
  
  gcs_source("example.R")
  expect_true(exists("x"))
  
})

context("Deleting")

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
})

test_that("We can delete the lifecycle bucket", {
  skip_on_cran()
  skip_if_no_token()
  
  deleted <- gcs_delete_bucket("blahblahblahffflifecycle")
  expect_true(deleted)
  
})


context("Pubsub")

test_that("Pubsub operations", {
  skip_on_cran()
  skip_if_no_token()
  
  email <- gcs_get_service_email(Sys.getenv("GCS_DEFAULT_PROJECT"))
  expect_true(grepl("@", email))
  expect_true(grepl("gserviceaccount\\.com", email))
  
  ps <- gcs_create_pubsub("gcs_r", 
                          project = Sys.getenv("GCS_DEFAULT_PROJECT"),
                          bucket = gcs_get_global_bucket())
  expect_equal(ps$kind, "storage#notification")
  
  ps_df <- gcs_list_pubsub(bucket = gcs_get_global_bucket())
  expect_true(all(ps_df$kind %in% "storage#notification"))
  
  expect_true(gcs_delete_pubsub(ps$id))
  
  
})

