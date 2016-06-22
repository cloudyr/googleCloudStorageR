library(googleCloudStorageR)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.full_control")
googleAuthR::gar_auth()

proj <- "your-project"
bucket <- "your-bucket"

buckets <- gcs_list_buckets(proj)
bucket_info <- gcs_get_bucket(bucket)

objects <- gcs_list_objects(bucket)

gcs_get_object(bucket, objects$name[[1]], saveToDisk = "csv_downloaded.csv")
