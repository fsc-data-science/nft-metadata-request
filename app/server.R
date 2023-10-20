library(shiny)
source("global.R")

server <- function(input, output, session) {

  # Build Query ----
  built_query <- reactive({
    buildQuery(input$tokenaddress, input$chain)
  })

  observe({
    bq <<- built_query()
  })
  
  # Submit Query ----
  submit_query <- reactive({
    submitQuery(input$tokenaddress, input$chain, input$discord_handle)
  })
  
  observe({
    sq <<- submit_query()
  })
  
  queryoutput <- eventReactive(eventExpr = input$runquery, {
    
    withProgress(message = 'Querying Flipside', value = 0, {
      
      incProgress(3/5, detail = paste("Checking NFT Data"))
      auto_paginate_query(query = built_query(), api_key = api.key, data_source = "data-science")    

    })
  }, 
  ignoreInit = TRUE, 
  ignoreNULL = TRUE)
  
  tbl_data <- reactive({
    table.data <- queryoutput()
    table.data$X__row_index <- NULL
    table.data
  })
  
  output$queryresult <- renderReactable({
    td <- tbl_data()
    
      result_tbl <- data.frame(
      "Traits Available" = ifelse(td$traits_count > 0, "YES", "No, please request update"),
      "Description Available" = ifelse(td$description_count > 0, "YES", "No, please request update"), 
      check.names = FALSE
    )
    
    reactable(result_tbl)
  })

  output$conditional_submit_contract <- renderUI({
    td <- tbl_data()
    
    if(td$traits_count > 0 & td$description_count > 0){
      tagList(
        hr(),
        p("Looks like we have both traits & descriptions for this one! Thank you for checking.")
      )
    } else {
      dh <- input$discord_handle
      tagList(
        hr(),
        p("Please confirm your Discord Handle & Submit the Contract here:"),
        if(nchar(dh) >= 4){
          actionButton(inputId = 'submit_contract',label = "Submit Contract for Refresh", icon = icon("arrow-right"))
        } else {
          p(paste0("Your discord name is too short!"), style = "color: red;") 
        }
      )
        }
  })
  
  querysubmit <- eventReactive(eventExpr = input$submit_contract, {
    print("Submit button clicked")
    withProgress(message = 'Submitting to Flipside', value = 0, {
      incProgress(3/5, detail = paste("Submitting Contract for Refresh"))
       submit_check <- auto_paginate_query(query = submit_query(), api_key = api.key, data_source = "data-science")
      incProgress(1/5, detail = paste("Submitting Contract for Refresh"))
      incProgress(1/5, detail = paste("Done!"))
      submit_check
    })
  }, 
  ignoreInit = TRUE, 
  ignoreNULL = TRUE)
  
  output$confirm_submit <- renderUI({
    qs <- querysubmit()
    tagList(
      br(),
    p("Added ", qs[1,1], "new entry to the backlog. Expect an update on traits/descriptions soon!")
  )
    })
  
  
}