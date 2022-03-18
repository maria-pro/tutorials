#search user profiles
options(scipen=999)
library(tidyverse)
library(lubridate)

tweets-read_csv("depression.csv")

tweets<-tweets%>%
  mutate(timestamp=lubridate::ymd_hms(created_at),
         day_of_week=lubridate::wday(timestamp, label=TRUE),
         day_weekday=(lubridate::wday(timestamp) %in% 2:6))
#Monday is very depressing

tweets%>%count(day_of_week, sort=TRUE)
tweets%>%count(day_weekday, sort=TRUE)
#weekdays are very very depressing or.. may be people do NOT tweet during weekends

#Offsetting dates

#time offset in months
times_offset<-12
time_window<-2 #for possible window

#add variable with window dates
tweets$timeoffset_start<-tweets$timestamp %m-% months(times_offset+time_window)
tweets$timeoffset_end<-tweets$timeoffset_start %m+% months(time_window)

#save in format for GET request
tweets$timeoffset_start<-format(tweets$timeoffset_start, format="%Y-%m-%dT00:00:00Z")
tweets$timeoffset_end<-format(tweets$timeoffset_end, format="%Y-%m-%dT00:00:00Z")
