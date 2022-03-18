#search user profiles
options(scipen=999)
library(tidyverse)
library(httr)
#----------------------

# FUNCTIONS - need to be run first

#----------------------
# FUNCTION 1
#----------------------

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

#-----------------------
# FUNCTION 2
#-----------------------

#building a query - generic
build_query<-function(endpoint_url, params, auth_headers=get_auth_headers()){
  
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

#-------------------------------
# SEARCH USERS

#App rate limit (OAuth 2.0 App Access Token): 1500 requests per 15-minute window shared among all users of your app
#User rate limit (OAuth 2.0 user Access Token): 900 requests per 15-minute window per each authenticated user
#User rate limit (OAuth 1.0a): 900 requests per 15-minute window per each authenticated user

tweets<-read_csv("depression.csv")

endpoint_url<-"https://api.twitter.com/2/users/"
#based on Twitter API2.0 endpoint https://api.twitter.com/2/users/:id/tweets
# curl --request GET 'https://api.twitter.com/2/users/USER_ID/tweets' --header 'Authorization: Bearer XXXXXX'


users<-tweets$author_id
chunks <-  unname(split(users, (seq_along(users) - 1) %/% 100))

df.all <- data.frame()
users_not_found<-""

for (i in chunks){
  
  params <- list(
    "ids" = paste(unlist(chunks[1]), collapse=","),
    "user.fields" = "created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld"
  )

  
  response<- httr::GET(
    endpoint_url,
    httr::add_headers(headers), query = params)
  
  # send request and parse response
  df<-build_query(endpoint_url, params)
  users_not_found<-c(users_not_found, df[["errors"]][["resource_id"]])

  #merge new tweets with what is there
  df.all <- dplyr::bind_rows(df.all, df$data)
}

df.all%>%count(id)
users_not_found

df.all


  
 