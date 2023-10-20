fluidPage(
  
  # App title ----
  tags$head(
    title = this.app.name,
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "sql_style.css"),
    tags$link(rel = "icon", href = "fliptrans.png"),
    tags$script(HTML("
  function copyToClipboard() {
    var content = document.getElementById('sql-code').innerText;
    var el = document.createElement('textarea');
    el.value = content;
    document.body.appendChild(el);
    el.select();
    document.execCommand('copy');
    document.body.removeChild(el);
  }
"))
  ),
  useShinyjs(),
  
  
  fluidRow(class = "titlerow",
           column(width = 3, div(id = "applogo", 
                                 a(href = "https://flipsidecrypto.xyz",
                                   img(src = "Flipside_black_logo_wordmark.svg", height = "24px"),
                                   onclick = paste0("rudderanalytics.track('", app.name.short, "_flipside')"),
                                   target = "_blank"))
           ),
           column(width = 6,
                  fluidRow(div(id = "appname", 'Request NFT Metadata'))
                  ), #end topics row
           
           column(width = 3,
                  div(id = "sidelinks",
                      a(href = "https://data.flipsidecrypto.com/", 
                        class = "data-shares-link", 
                        img(src = "Flipside_icon_white.svg", height = "14px"), 
                        "Flipside Pro",
                        onclick = paste0("rudderanalytics.track('", app.name.short, "_enterprise')"),
                        target = "_blank"),
                      a(href = "https://twitter.com/flipsidecrypto", 
                        img(src = "twitter.svg", height = "14px"),
                        style = "margin-left: 15px",
                        onclick = paste0("rudderanalytics.track('", app.name.short, "_twitter')"),
                        target = "_blank"),
                      a(href = "https://discord.com/invite/ZmU3jQuu6W", 
                        img(src = "discord.svg", height = "14px"),
                        style = "margin-left: 15px",
                        onclick = paste0("rudderanalytics.track('", app.name.short, "_discord')"),
                        target = "_blank")
                  )
           )
  ), # End title row
  
  div(class = "appbody",
      br(),
      
      fluidRow(
        column(6, 
               div(class = "section-container-l",
                   div(class = "title", 
                       "Let's check Metadata Availability:"),
                   
                   br(),
                   div(class = "title", "Options:"),
                   br(),
                   selectInput(inputId = 'chain', 
                             label = "Blockchain (more chains soon):",
                             choices = c('ethereum'
                                         # ,
                                         # 'solana',
                                         # 'polygon',
                                         # 'base',
                                         # 'optimism',
                                         # 'arbitrum'
                                         ),
                             selected =  'ethereum',
                             multiple = FALSE,
                             width = "400px"),
                   textInput(inputId = 'tokenaddress', 
                             label = "NFT Contract Address:",
                             value = "",
                             placeholder = "0x....",
                             width = "400px"),
                   textInput(inputId = 'discord_handle', 
                             label = "Your Discord Handle:",
                             value = "",
                             placeholder = "No @ needed"),
                   br(),
                   div(class = "section-container-s",
                       div(class = "title", 
                           HTML("<strong><u>Glossary</u></strong>")),
                       div(
                         h5(HTML("<strong><u>NFT</u></strong>")),
                         p("Non-Fungible Tokens are unique tokens, typically denoted with a number ID, e.g., Bored Ape Yacht Club #44.")
                       ),
                       div(
                         h5(HTML("<strong><u>Metadata</u></strong>")),
                         p("NFTs typically have collection names (e.g., Bored Ape Yacht Club) alongside a symbol, and then each ID may have traits (e.g., yellow hat) and potentially a description like the artist.")
                   ))
                   
               ) # close section container
        ), #close left column 6
        column(width = 6,
               div(class = "section-container-r",
                   
                   tabsetPanel(type = "pills",
                               # tabPanel("Write the Query", 
                               #          br(),
                               #          div(class = "query_head"),
                               #          div(uiOutput('sql'))
                               # ),
                               tabPanel("Traits & Description Scan", 
                                        br(),
                                        actionButton("runquery","Check NFT Address", icon = icon("arrow-right")),
                                        reactableOutput("queryresult"),
                                        uiOutput("conditional_submit_contract"),
                                        uiOutput('confirm_submit')
                               )
                   )
                   
               ) # close section-container
        ) # close right column 6
      ) # close main section fluidRow
  ) # close div .appbody
) # close fluid page

