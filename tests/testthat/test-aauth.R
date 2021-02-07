test_that("Authentication", {
  skip_if_no_token()
  
  # manual auth
  expect_true(googleAuthR::gar_has_token())
})

test_that("Expected env vars present",{
  skip_if_no_token()
  expect_true(nzchar(Sys.getenv("GCS_DEFAULT_BUCKET")))
  expect_true(nzchar(Sys.getenv("GCS_DEFAULT_PROJECT")))
  expect_true(nzchar(Sys.getenv("GCS_AUTH_FILE")))
  
})