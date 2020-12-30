# googleCloudStorageR

## Mission Statement

`googleCloudStorageR` is an R library for interacting with the Google Cloud Storage JSON API ([api docs](https://cloud.google.com/storage/docs/json_api/)).

Please read [Get Started](articles/googleCloudStorageR.html) for an overview of its features, or browse the [function reference](reference/index.html). 

## Installation

<!-- badges: start -->
[![CRAN](http://www.r-pkg.org/badges/version/googleCloudStorageR)](http://cran.r-project.org/package=googleCloudStorageR)
![CloudBuild](https://badger-ewjogewawq-ew.a.run.app/build/status?project=mark-edmondson-gde&id=b645a814-94ae-4154-90fb-dad20f815ad9)
[![codecov.io](http://codecov.io/github/cloudyr/googleCloudStorageR/coverage.svg?branch=master)](http://codecov.io/github/cloudyr/googleCloudStorageR?branch=master)
[![Downloads](https://cranlogs.r-pkg.org/badges/googleCloudStorageR)](https://www.r-pkg.org/pkg/googleCloudStorageR)
<!-- badges: end -->

This package is on CRAN:

```R
# latest stable version
install.packages("googleCloudStorageR")
```

Or, to pull a potentially unstable version directly from GitHub:

```R
if(!require("remotes")){
    install.packages("remotes")
}
remotes::install_github("cloudyr/googleCloudStorageR")
```

---
[![cloudyr project logo](http://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
