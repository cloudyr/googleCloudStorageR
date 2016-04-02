# googleCloudStorageR

R library for interacting with google cloud storage

## Demo running it in Shiny

```r
library(shiny)
library(googleAuthR)
library(googleCloudStorageR)
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
