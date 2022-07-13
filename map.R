install.packages("leaflet")
library(leaflet)

library(tidyverse)

data<-read_csv("data/museum2.csv")

data%>%
  leaflet() %>%
 addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")%>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery"))%>%
  addMarkers(label = data$museum, 
             lng=data$lng,
             lat=data$lat,
           popup = data$description)

