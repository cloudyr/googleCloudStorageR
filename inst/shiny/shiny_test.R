library(shiny)
library(googleAuthR) ## > 0.2.0.9000
library(googleCloudStorageR)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/devstorage.full_control")

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

    buckets <- reactive({
      validate(
        need(access_token(), "Authorise")
      )

      # with_shiny(gcs_list_buckets)


    })

    meta <- eventReactive(input$submit, {

      message("Uploading to Google Cloud Storage")
      with_shiny(gcs_upload,
                 file = input$picture$datapath,
                 bucket = "gogauth-test",
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
