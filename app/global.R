library(shiny)
library(shinyjs)
library(data.table)
library(jsonlite)
library(shroomDK)
library(reactable)
library(showtext)
library(shinyWidgets)
library(htmltools)
library(shinyBS)
library(stringr)

app.name.short = 'nft-metadata'
this.app.name = 'nft-metadata-requestor'


# gitignore api keys
api.key <- readLines("api_key.txt")

#Query API interface

PullVelocityData <- function(endpoint.url){
  raw.results <- readLines(endpoint.url)
  to.return <- data.table(jsonlite::fromJSON(raw.results))
  setnames(to.return, tolower(names(to.return)))
  return(to.return)
}

# SQL Highlight (see css)

sql_highlight <- function(sql_code) {
  # Define the SQL keywords and their associated CSS classes
  keywords <- list(
    keyword = c("SELECT", "FROM", "WHERE", "GROUP BY", "ORDER BY", "HAVING", "JOIN", "INNER JOIN", "LEFT JOIN", "RIGHT JOIN", "FULL JOIN", "OUTER JOIN", "ON", "AS", "AND", "OR", "NOT", "IS", "NULL", "IN", "BETWEEN", "EXISTS", "LIKE", "LIMIT", "DISTINCT", "LEFT"),
    func_ = c("COUNT", "SUM", "AVG", "MIN", "MAX"),
    operator = c("=", "<", ">", "<=", ">=", "<>", "!=")
  )
  
  sql_code <- gsub("<", "&lt;", sql_code)
  sql_code <- gsub(">", "&gt;", sql_code)
  
  # Replace SQL keywords with HTML span elements containing CSS classes
  for (k in names(keywords)) {
    for (w in keywords[[k]]) {
      sql_code <- gsub(paste0("\\b", w, "\\b"), paste0("<span class='", k, "'>", w, "</span>"), sql_code, ignore.case = TRUE)
    }
  }
  sql_code <- gsub("<span class='operator'><</span<span class='operator'>></span>","<",sql_code)
  
  html <- htmltools::HTML(sql_code)
  
  return(html)
}

# Query Build ----

#' Check 1: Do we have NFT already ----

checkNFT <- function(nft_address, chain){
  
  query <- {
    
    "
  with select_NFT as ( 
  select lower('__NFT_ADDRESS__') as nft_address from dual 
) 
  select * from __THECHAIN__.nft.dim_nft_metadata where contract_address = (select nft_address from select_nft) limit 5;
    "
  }
  
query <- gsub('__NFT_ADDRESS__', nft_address, x = query)
query <- gsub('__THECHAIN__', chain, x = query)

res <- tryCatch({
  temp_ <- nrow(shroomDK::auto_paginate_query(query, api_key = api.key))
  if(temp_ > 0) return(TRUE)
}, error = function(e){
    return(FALSE)
} 
)

return(res)
}

buildQuery <- function(address, chain){
  # Query won't run without internal dev API key 
query <- { "
with raw as (
    SELECT
    lower('THE_NFT_ADDRESS') as nft_address,
        1 as current_page,
        'qn_fetchNFTsByCollection' AS method,
        CONCAT(
            '{\\'id\\': 67, \\'jsonrpc\\': \\'2.0\\', \\'method\\': \\'',
            method,
            '\\',\\'params\\': [{ \\'collection\\': \\'',
            nft_address,
            '\\', \\'page\\': ',
            current_page,
            ',\\'perPage\\': 100 } ]}'
        ) AS json_request,
        node_url,
        ethereum.streamline.udf_api('POST', node_url,{}, PARSE_JSON(json_request)) as resp,
        resp:data:result:tokens[0]:collectionAddress as collection_address
    FROM
         streamline.crosschain.node_mapping

    WHERE
        chain = 'THE_CHAIN' 
      )
      ,

      flattened as (
    select 
    index, 
    value,
    value:description::string as description,
    value:traits as traits 
    from raw , lateral flatten (input=> resp:data:result:tokens)
) 
select 
sum(iff(traits = '[]' or traits is null, 0, 1)) as traits_count,
sum(iff(description = '' or description is null, 0, 1)) as description_count,
iff(description_count = 0 or traits_count is null, 'no', 'yes') as is_description_available,
iff(traits_count = 0 or description_count is null, 'no', 'yes') as are_traits_available
from flattened 
   " 
}

query <- gsub("THE_NFT_ADDRESS", address, query)
query <- gsub("THE_CHAIN", chain, query)
  
  return(query)
  
}

submitQuery <- function(address, chain = 'ethereum', discord = ""){
  # Query won't run without internal dev API key 
query <- {
  "
  insert into 
crosschain_dev.bronze_public.user_metadata
select 
'THE_NFT_ADDRESS' as nft_address,
'THE_CHAIN' as blockchain,
'THE_DISCORD_USER' as discord_user,
sysdate() as _inserted_timestamp;
  "
}  

query <- gsub("THE_NFT_ADDRESS", address, query)
query <- gsub("THE_CHAIN", chain, query)
query <- gsub("THE_DISCORD_USER", discord, query)
  
return(query)
}
