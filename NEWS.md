# googleCloudStorage 0.7.0.9000

* Fix bug where retry was not attempted on `gcs_upload()`
* Add some custom error classes `http_400`, `http_404` etc.
* Allow listing of more than 1000 buckets (#163 - thanks @hidekoji)

# googleCloudStorage 0.7.0

* Fix url encoding issue for multipart uploads (#139 - thanks @jomatt)
* Add downloading a folder example to `gcs_get_object()` #144
* Improve message feedback for downloading objects
* `gcs_upload()` will now retry with a warning if using legacy ACL settings for a bucket with uniform bucket-level access enabled - will retry with `predefinedAcl='bucketLevel'`
* Add `gcs_parse_rds()` to help parsing RDS objects for `gcs_get_object()` #146
* Add ability to request non-current versions of objects in `gcs_get_object()` via `generation` parameter.
* Add `gcs_delete_bucket_objects()` to help delete all objects from a bucket, including older object generation versions (#152)

# googleCloudStorageR 0.6.0

* Let users auth via an email again via `gcs_auth`
* correct file.copy behaviour in `gcs_load_all()` (#112) - thanks @jasonmhoule
* Allow GCS uploads if ACL has been set at bucket level (#111)
* Let users set the boundary between simple and resumable upload limit via new option `option(googleCloudStorageR.upload_limit)` or `gcs_upload_set_limit()` - default is 5000000L or 5MB (#120)
* Suppress warning for when resumable uploads are requesting the upload URL (#120)
* Fix ability to upload meta data with objects (#108)
* If file has no filesize will message without an error (#123)
* If project has no buckets will give NULL and not an error (#100)
* Fix `gcs_save_all()` to also accept bucket level ACL (#129)
* Fix listing buckets for projects `gcs_list_buckets()` (#100)
* Add `gcs_setup()` to help first time setup

# googleCloudStorageR 0.5.1

* Fix `gcs_save_all()` to use new `zip` for filepaths (Thanks @caewok) #107
* fix unable to return object metadata (#105)
* fix signature mismatch if object name contains / (#102)
* document `gcs_version_bucket()` - thanks @j450h1 ! (#96)


# googleCloudStorageR 0.5.0

* Refactor authentication to favour json file and fix auth hangs (#87)
* Prevent error if bucket name is empty string (#90)
* `gcs_upload()` will use file extension of `name` in its temporary file (#91)
* Add `gcs_copy_object()`
* Add `gcs_compose_objects()`
* Refactor `gcs_list_objects()` to googleAuthR > 0.7 `gar_api_page()`

# googleCloudStorageR 0.4.0

## Major changes

* Update Bucket storageClass to include `MULTI_REGIONAL`, `REGIONAL`, and `COLDLINE`
* Fix bug where `gcs_load` wouldn't work if file name not ".RData"
* Add `gcs_first` and `gcs_last` to autosave your file workspace to GCS
* Better message feedback on file sizes
* Add `gcs_save_all` and `gcs_load_all` which will zip, save/load and upload/download a directory
* Add use of `_gcssave.yaml` file to control `gcs_first/last` behaviour
* Allow passing a bucket object to functions that expect a bucket name (#76)
* remove now unsupported travis environment argument
* Add support for subscribing bucket changes to Google Pub/Sub

# googleCloudStorageR 0.3.0

## Major changes

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

# googleCloudStorageR 0.2.0

## Major changes

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

## Major changes

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



