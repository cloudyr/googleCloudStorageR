test_that("Signed URLs", {
  skip_if_no_token()
  
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
  
  # upload to bucketLevel Acl
  bl <- gcs_upload(mtcars, bucket = "mark-bucketlevel-acl",
                   predefinedAcl = "bucketLevel")
  expect_equal(bl$kind, "storage#object")
  
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

test_that("We can create a download URL", {
  skip_on_cran()
  skip_if_no_token()
  
  gcs_upload(mtcars, bucket = gcs_get_global_bucket(), name = "mtcars_meta.csv")
  
  durl <- gcs_download_url("mtcars_meta.csv")
  expect_equal(durl,
               sprintf("https://storage.cloud.google.com/%s/mtcars_meta.csv",
                       gcs_get_global_bucket()))
  
})
