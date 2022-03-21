options(scipen=999)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)

##### Functions block

#---------
#Function1
#get twitter bearer token from .Renviron and add it to header
#to open your .Renviron file "usethis::edit_r_environ()" in console
get_auth_headers<-function(){
  #get token from the environment var
  bearer_token<-Sys.getenv('TWITTER_BEARER')
  
  if (identical(bearer_token, "")){
    stop("check your Twitter bearer_token in the .Renviron file")
  }
  headers = c(
    `Authorization` = sprintf('Bearer %s', bearer_token)
  )
  return (headers)
}

#---------
#Function2
#building a query - generic
build_query<-function(endpoint_url, params, auth_headers=auth_headers){
  
  #App rate limit: 300 requests per 15-minute window shared among all users of your app
  #App rate limit: 1 request per second shared among all users of your app
  time0<-Sys.time()
  # print(auth_headers) # debug
  response<- httr::GET(
    endpoint_url,
    httr::add_headers(auth_headers), query = params)
  
  #checking time rate
  time_dif<-as.numeric(Sys.time()- time0)
  if (time_dif<1) {(
    Sys.sleep(1)
  )
  }
  
  #check errors
  status_code<-httr::status_code(response)
  # print(paste0("Status code:", status_code))
  
  
  if(status_code(response)!=200) {
    stop(paste(" error code:", status_code ))
  }
  
  df <-jsonlite::fromJSON(httr::content(response, "text"))
  df
}

#test<-build_query("https://api.twitter.com/2/tweets/search/recent", params = list('query'="#oatmilk"), auth_headers=auth_headers)


#---------
#Function3
#----------------
# search_all functions for historical search

search_tweets<-function(
  params, auth_headers=auth_headers
){
  #collect data from Twitter using parameters
  endpoint_url <- "https://api.twitter.com/2/tweets/search/all"
  
  #check query line - later
  
  #params[["next_token"]]<- list(NULL)
  
  df.all <- data.frame()
  ntweets <- 0
  
  # due to pagination - use loop
  
  i <- 0
  MAX_REQUESTS <- 500
  
  while (i < MAX_REQUESTS) {
    i <- i + 1
    
    cat("\nIteration number", i, "\n")
    
    #300 requests per 15-minute window shared among all users of your app
    if (i%%300==0){
      cat("Waiting for 15 minutes before sending next request...\n")
      Sys.sleep(15*60+1)
      cat("Done waiting and back to collecting...\n")
    }
    
    
    # send request and parse response
    df<-build_query(endpoint_url, params, auth_headers)
    
    #merge new tweets with what is there
    df.all <- dplyr::bind_rows(df.all, df$data)
    
    # if no data is received, this is probably an error
    if (is.null(df$data)) {
      cat("No data received!")
      break
    }
    
    # count tweets
    n_newtweets <- nrow(df$data)
    ntweets <- ntweets + n_newtweets
    cat("New tweets:", n_newtweets, "; Total count:", ntweets, "\n")  
    
    # check for next token
    next_token <- df$meta$next_token #this is NULL if there are no pages left
    if (is.null(next_token)) {
      cat("next_token is empty - exiting...")
      break;
    }
    cat("next token:", next_token, "\n")
    params[["next_token"]] <- next_token
  }
  
  return(df.all)
}

#------------------
# SETUP

## STEP 1

#### Setting/checking Twitter credentials

#set your environment - there should be a line with TWITTER_BEARER=ENTER_YOUR_TOKEN
#usethis::edit_r_environ()

# this step is done within a function so you don't need to put it explicitly

#get token from the environment var
bearer_token<-Sys.getenv('TWITTER_BEARER')

#authorization header will be added automatically
auth_headers = c(
  `Authorization` = sprintf('Bearer %s', bearer_token)
)

#--------------


### Get tweets on search terms

#collect data from Twitter using parameters
params <- list(
  'query' = '#oatmilk lang:en',
  'start_time' = "2022-01-01T00:00:00.000Z",
  'expansions' = 'author_id,in_reply_to_user_id,geo.place_id',
  'tweet.fields' = 'author_id,conversation_id,created_at,geo,id,in_reply_to_user_id,lang,public_metrics,referenced_tweets,source,text',
  'user.fields' = 'id,name,username,created_at,description,public_metrics,verified',
  'place.fields' = 'full_name,id,country,country_code,geo,name,place_type'
)

data<-search_tweets(params, auth_headers=auth_headers)

#saving to csv
data%>%flatten()%>%write_csv( 
  file = "depression.csv")
```
