#search user profiles
options(scipen=999)
library(tidyverse)
library(lubridate)

tweets-read_csv("depression.csv")

#----------
#adding days of the week

tweets_dates<-tweets%>%
  mutate(timestamp=lubridate::ymd_hms(created_at),
         day_of_week=lubridate::wday(timestamp, label=TRUE),
         day_weekday=(lubridate::wday(timestamp) %in% 2:6),
         year=year(created_at),
         month=month(created_at),
         hours=hour(created_at),
         part_of_day= factor(hours%/%6, labels=c("night", "morning", "day", "evening")) #roughly! #converting to a factor preserves the order (0 is the smallest in ranking )
)
#Monday is very depressing

tweets_dates%>%count(day_of_week, sort=TRUE)
tweets_dates%>%count(day_weekday, sort=TRUE)

tweets_dates%>% group_by(year)%>%
  count(day_of_week, sort=TRUE)

#most depressing year,month
tweets_dates%>%
  group_by(year, month)%>%
  summarise(n=n())%>%
  arrange(desc(n))
#weekdays are very very depressing or.. may be people do NOT tweet during weekends

#excluding replies to other users
tweets_dates%>% filter(is.na(in_reply_to_user_id))%>%
                 ggplot(aes(day_of_week, 
                            fill=as_factor(year), 
                            position = "identity"))+
  geom_bar()
#adding times 


#----------
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
