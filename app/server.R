server <- function(input, output, session) {

  # Check Query ----
  check_query <- reactive({
    checkNFT(input$tokenaddress, input$chain)
  })
  
  # Build Query ----
  built_query <- reactive({
    buildQuery(input$tokenaddress, input$chain)
  })

  observe({
    cq <<- check_query()
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
      if(check_query() == TRUE){
        return(
          data.frame(
            traits_count = NA,
            description_count = NA
          )
        )
      } else {
        incProgress(1/5, detail = paste("Looks like Flipside is missing it, checking nodes"))
      auto_paginate_query(query = built_query(), api_key = api.key, data_source = "data-science")    
      }

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
      "Traits Available" = ifelse(is.na(td$traits_count),
                                    "Flipside has it!",
                           ifelse(td$traits_count > 0, 
                                    "Node has it, we don't, please request update",
                                  "Looks like the node doesn't have it either.")
                           ),
      "Description Available" = ifelse(is.na(td$description_count), 
                                       "Flipside has it!",
                                ifelse(td$description > 0,
                                       "Node has it, we don't, please request update",
                                       "Looks like the node doesn't have it either.")), 
      check.names = FALSE
    )
    
    reactable(result_tbl)
  })

  output$conditional_submit_contract <- renderUI({
    td <- tbl_data()
    
    if(is.na(td$traits_count) & is.na(td$description_count)){
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