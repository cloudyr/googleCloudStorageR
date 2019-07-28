context("Authentication")

test_that("Authentication", {
  googleAuthR::skip_if_no_env_auth(
    "GCS_AUTH_FILE"
  )
  expect_true(file.exists(Sys.getenv("GCS_AUTH_FILE")))
  expect_true(googleAuthR::gar_has_token())
})

test_that("Download", {
  ''
})