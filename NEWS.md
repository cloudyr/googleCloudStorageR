# 0.1.0.9000

* Fix bug where you can't rename objects via name in `gcs_upload`
* Add `gcs_save` to store R session data in cloud
* Add `gcs_load` to restore session data stored with `gcs_save`
* Fix resetting of `options(googleAuthR.rawResponse = TRUE)` when using `gcs_get_object`
* Add URLencoding to `object_name` in `gcs_get_object`
* `gcs_global_bucket` and `gcs_get_global_bucket` to set global bucket name

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



