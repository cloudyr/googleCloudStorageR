# 0.3.0

* Correct metadata upload (#55 - thanks AndrewMarritt)
* Let `gcs_object_metaname` not require name so it can be reused (#56 - thanks seandavi)
* Add support for `gs://` style URLs for object names (#57 - thanks seandavi)
* Add what the public URL would be to an objects print method (#59 - thanks mwhitaker)
* Add check to `gcs_get_bucket()` to only expect length 1 character vectors for bucket name. (#60)
* Add paging for `gcs_object_list` (#58 - thanks @G3rtjan)
* Add `saveToDisk` option to `gcs_load` (#52 - thanks @tomsing1)
* Supply your own parse function to `gcs_get_object()` (#63 - thanks @nkeriks)
* Support for `prefix` and `delimiter` in `gcs_object_list` to filter objects listed (#68)
* Default permissions on new buckets lets you write to them afterwards `projectPrivate (#62)
* `gcs_get_object` now supports downloads over 2GB (#69)
* Add support for signed URLs (#54 - thanks seandavi)

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



