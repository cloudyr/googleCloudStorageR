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

