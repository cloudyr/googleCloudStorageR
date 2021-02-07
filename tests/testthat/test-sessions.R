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

test_that("Load and Save All",{
  skip_if_no_token()
  
  here <- getwd()
  message("These files: ", paste(list.files(here, full.names = TRUE), 
                                 collapse = "\n"))
  
  saved <- gcs_save_all(here)
  expect_equal(saved$kind, "storage#object")
  saved_files <- gcs_list_objects(prefix = here)
  expect_equal(nrow(saved_files), 1)
  
  loaded <- gcs_load_all(here, exdir = "load_all")
  expect_true(loaded)
  
  # we have some files
  expect_true(length(list.files("load_all", pattern = ".R$")) > 1)
  
  on.exit(unlink("load_all", recursive = TRUE))
  
  expect_true(gcs_delete_all(here))
  gone <- gcs_list_objects(prefix = here)
  expect_equal(nrow(gone), 0)
  
})

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
