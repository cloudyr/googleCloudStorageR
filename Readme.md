# googleCloudStorageR

R library for interacting with the Google Cloud Storage JSON API ([api docs](https://cloud.google.com/storage/docs/json_api/)).

## Setup

Google Cloud Storage charges you for storage [(prices here)](https://cloud.google.com/storage/pricing).

You can use your own Google Project with a credit card added to create buckets, where the charges will apply.  This can be done in the [Google API Console](https://console.developers.google.com)

## Examples

### Downloading objects from Google Cloud storage

Once you have a Google project and created a bucket with an object in it, you can download it as below:

```r
library(googleCloudStorageR)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.full_control")

## optional, if you want to use your own Google project
# options("googleAuthR.client_id" = "YOUR_CLIENT_ID")
# options("googleAuthR.client_secret" = "YOUR_CLIENT_SECRET")

googleAuthR::gar_auth()

## get your project name from the API console
proj <- "your-project"

## get bucket info
buckets <- gcs_list_buckets(proj)
bucket <- "your-bucket"
bucket_info <- gcs_get_bucket(bucket)

## get object info
objects <- gcs_list_objects(bucket)

## save directly to an R object (warning, don't run out of RAM if its a big object)
## the download type is guessed into an appropriate R object
parsed_download <- gcs_get_object(objects$name[[1]], bucket)

## if you want to do your own parsing, set parseObject to FALSE
## use httr::content() to parse afterwards
raw_download <- gcs_get_object(objects$name[[1]], 
                               bucket, 
                               parseObject = FALSE)

## save directly to a file in your working directory
## parseObject has no effect, it is a httr::content(req, "raw") download
gcs_get_object(objects$name[[1]], bucket, saveToDisk = "csv_downloaded.csv")


                               
```

### Uploading via a Shiny app

The library is also compatible with Shiny authentication flows, so you can create Shiny apps that lets users log in and download their own data.  

An example of that is shown below:

```r
library("shiny")
library("googleAuthR")
library("googleCloudStorageR")
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.full_control")
## optional, if you want to use your own Google project
# options("googleAuthR.client_id" = "YOUR_CLIENT_ID")
# options("googleAuthR.client_secret" = "YOUR_CLIENT_SECRET")

## you need to start Shiny app on port 1221
## as thats what the default googleAuthR project expects for OAuth2 authentication

## options(shiny.port = 1221)
## print(source('shiny_test.R')$value) or push the "Run App" button in RStudio

shinyApp(
  ui = shinyUI(
      fluidPage(
        googleAuthR::googleAuthUI("login"),
        fileInput("picture", "picture"),
        textInput("filename", label = "Name on Google Cloud Storage",value = "myObject"),
        actionButton("submit", "submit"),
        textOutput("meta_file")
      )
  ),
  server = shinyServer(function(input, output, session){

    access_token <- shiny::callModule(googleAuth, "login")

    meta <- eventReactive(input$submit, {

      message("Uploading to Google Cloud Storage")
      
      # from googleCloudStorageR
      with_shiny(gcs_upload,  
                 file = input$picture$datapath,
                 # enter your bucket name here
                 bucket = "gogauth-test",  
                 type = input$picture$type,
                 name = input$filename,
                 shiny_access_token = access_token())

    })

    output$meta_file <- renderText({
      
      req(meta())

      str(meta())

      paste("Uploaded: ", meta()$name)

    })

  })
)
```

## Explanation of Google Project access

`googleCloudStorageR` has its own Google project which is used to call the Google Cloud Storage API, but does not have access to the objects or buckets in your Google Project unless you give permission for the library to access your own buckets during the OAuth2 authentication process.  

No other user, including the owner of the Google Cloud Storage API project has access unless you have given them access, but you may want to change to use your own Google Project (that could or could not be the same as the one that holds your buckets) - this is outlined below:

### For local use

1. Click 'Create a new Client ID', and choose "Installed Application".
2. Note your Client ID and secret.
3. Modify these options after googleAuthR has been loaded:

        options("googleAuthR.client_id" = "YOUR_CLIENT_ID")
        options("googleAuthR.client_secret" = "YOUR_CLIENT_SECRET")

### For Shiny use

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
