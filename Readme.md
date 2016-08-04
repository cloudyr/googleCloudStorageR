# googleCloudStorageR

R library for interacting with google cloud storage

## Setup

As Google Cloud Storage charges you for storage [(prices here)](https://cloud.google.com/storage/pricing) and the default project has no credit card, to use you will need your own Google Project with a credit card added.  This can be done in the [Google API Console](https://console.developers.google.com)

For local use

1. Click 'Create a new Client ID', and choose "Installed Application".
2. Note your Client ID and secret.
3. Modify these options after googleAuthR has been loaded:

        options("googleAuthR.client_id" = "YOUR_CLIENT_ID")
        options("googleAuthR.client_secret" = "YOUR_CLIENT_SECRET")

For Shiny use

1. Click 'Create a new Client ID', and choose "Web Application".
2. Note your Client ID and secret.
3. Add the URL of where your Shiny app will run, with no port number. e.g. `https://mark.shinyapps.io/searchConsoleRDemo/`
4. And/Or also put in localhost or 127.0.0.1 with a port number for local testing. Remember the port number you use as you will need it later to launch the app e.g. `http://127.0.0.1:1221`
5. In your Shiny script modify these options:

        options("googleAuthR.webapp.client_id" = "YOUR_CLIENT_ID")
        options("googleAuthR.webapp.client_secret" = "YOUR_CLIENT_SECRET")

6. To run the app locally specifying the port number you used in step 4 e.g. `shiny::runApp(port=1221)` or set a shiny option to default to it: `options(shiny.port = 1221)` and launch via the `RunApp` button in RStudio.
7. Running on your Shiny Server will work only for the URL from step 3.

### Activate API

1. Click on "APIs"
2. Select and activate the Cloud Storage JSON API 
3. Set the `googleAuthR`option for Google Cloud storage scope:

        options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.full_control")

## Demo downloading objects from Google Cloud storage

```r
library(googleCloudStorageR)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.full_control")
googleAuthR::gar_auth()

proj <- "your-project"
bucket <- "your-bucket"

buckets <- gcs_list_buckets(proj)
bucket_info <- gcs_get_bucket(bucket)

objects <- gcs_list_objects(bucket)

gcs_get_object(bucket, objects$name[[1]], saveToDisk = "csv_downloaded.csv")
```

## Demo uploading via Shiny

```r
library("shiny")
library("googleAuthR")
library("googleCloudStorageR")
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.full_control")

## you need to start Shiny app on port 1221
## as thats what the default googleAuthR project expects for OAuth2 authentication

## options(shiny.port = 1221)
## print(source('shiny_test.R')$value) or push the "Run App" button in RStudio

shinyApp(
  ui = shinyUI(
      fluidPage(
        googleAuthR::loginOutput("login"),
        fileInput("picture", "picture"),
        textInput("filename", label = "Name on Google Cloud Storage",value = "myObject"),
        actionButton("submit", "submit"),
        textOutput("meta_file")
      )
  ),
  server = shinyServer(function(input, output, session){

    access_token <- reactiveAccessToken(session)

    output$login <- googleAuthR::renderLogin(session, access_token())

    meta <- eventReactive(input$submit, {

      message("Uploading to Google Cloud Storage")
      with_shiny(gcs_upload,  # from googleCloudStorageR
                 file = input$picture$datapath,
                 bucket = "gogauth-test",  # enter your bucket name here
                 type = input$picture$type,
                 name = input$filename,
                 shiny_access_token = access_token())

    })

    output$meta_file <- renderText({
      validate(
        need(meta(), "Upload file")
      )

      str(meta())

      paste("Uploaded: ", meta()$name)

    })

  })
)
```


## Installation ##

[![CRAN](http://www.r-pkg.org/badges/version/googleCloudStorageR)](http://cran.r-project.org/package=googleCloudStorageR)
[![Build Status](https://travis-ci.org/cloudyr/googleCloudStorageR.png?branch=master)](https://travis-ci.org/cloudyr/googleCloudStorageR)
[![codecov.io](http://codecov.io/github/cloudyr/googleCloudStorageR/coverage.svg?branch=master)](http://codecov.io/github/cloudyr/googleCloudStorageR?branch=master)

This package is not yet on CRAN. To install the latest development version you can install from the cloudyr drat repository:

```R
# latest stable version
install.packages("googleCloudStorageR", repos = c(getOption("repos"), "http://cloudyr.github.io/drat"))
```

Or, to pull a potentially unstable version directly from GitHub:

```R
if(!require("ghit")){
    install.packages("ghit")
}
ghit::install_github("cloudyr/googleCloudStorageR")
```


---
[![cloudyr project logo](http://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
