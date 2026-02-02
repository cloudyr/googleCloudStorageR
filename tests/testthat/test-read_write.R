create_test_bucket <- function() {
  bucket <- paste0(sample(letters, 20, replace = T), collapse = "")
  gcs_create_bucket(
    name = bucket,
    projectId = Sys.getenv("GCS_DEFAULT_PROJECT")
  )
  bucket
}

test_that("writing object to file creates file in bucket", {
  skip_if_no_token()
  bucket <- create_test_bucket()

  df <- data.frame(x = 1)
  gcs_write(df, "df.csv", bucket = bucket, write_fun = write.csv)

  expect_true("df.csv" %in% gcs_list_objects(bucket = bucket)$name)
})

test_that("Custom error message when write_fun has input and output switched", {
  skip_if_no_token()
  bucket <- create_test_bucket()

  df <- data.frame(x = 1)

  write_fun_wrong <- function(input, output) {
    write.csv(output, input)
  }

  expect_error(
    gcs_write(df, "df.csv", bucket = bucket, write_fun = write_fun_wrong),
    "Does write_fun have the R object as first and the file to write to as second argument?"
  )
})

test_that("writing and then reading an .rds file gives the same object", {
  skip_if_no_token()
  bucket <- create_test_bucket()

  # write and then read .rds file
  res <- qr(mtcars)
  gcs_write(res, file = "res.rds", bucket = bucket, write_fun = saveRDS)
  res2 <- gcs_read(file = "res.rds", bucket = bucket, read_fun = readRDS)

  expect_identical(res2, res)
})

test_that("reading .rds file that was stored with another compression than gzip gets custom error", {
  skip_if_no_token()
  bucket <- create_test_bucket()

  res <- qr(mtcars)
  gcs_write(
    x = res,
    file = "res.rds",
    bucket = bucket,
    write_fun = saveRDS,
    compress = "bzip2"
  )

  expect_error(
    gcs_read(file = "res.rds", bucket = bucket, read_fun = readRDS),
    "There was an error when reading the file"
  )
})

test_that("reading .rds file that was stored with another compression via tempfile works", {
  skip_if_no_token()
  bucket <- create_test_bucket()

  res <- qr(mtcars)
  gcs_write(
    x = res,
    file = "res.rds",
    bucket = bucket,
    write_fun = saveRDS,
    compress = "bzip2"
  )
  res2 <- gcs_read(
    file = "res.rds",
    bucket = bucket,
    read_fun = readRDS,
    temp_file = TRUE
  )

  expect_identical(res2, res)
})
