# 0.2.0

* Fix bug where you can't rename objects via name in `gcs_upload`
* Add `gcs_save` to store R session data in cloud
* Add `gcs_load` to restore session data stored with `gcs_save`
* Fix resetting of `options(googleAuthR.rawResponse = TRUE)` when using `gcs_get_object`
* Add URLencoding to `object_name` in `gcs_get_object` etc.
* `gcs_global_bucket` and `gcs_get_global_bucket` to set global bucket name
* Add options to set environment variables for API keys, default buckets etc.
* Add print S3 methods for objects, buckets and ACL
* Add option to use your own write function for R object file uploads in `gcs_upload`
* Add option to include object metadata in uploads via `gcs_metadata_object`
* Add resumable uploads to `gcs_upload` to allow uploads over 5MB, limit now 5TB
* Add retry uploads via `gcs_retry_upload`
* Add delete object via `gcs_delete_object`
* Add `gcs_source` to source .R files from GCS
* Add versioning and lifecycle management to create and update bucket functions

# googleCloudStorageR 0.1.0

Initial release

* `gcs_create_bucket`
* `gcs_download_url`
* `gcs_get_bucket`
* `gcs_get_object`
* `gcs_get_object_access`
* `gcs_list_buckets`
* `gcs_list_objects`
* `gcs_parse_download`
* `gcs_update_acl`
* `gcs_update_bucket`
* `gcs_upload`



