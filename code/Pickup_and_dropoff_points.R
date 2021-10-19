library(tidyverse)
library(ggmap)

taxi_trips<-read_csv("./data/Taxi_Trips_-_2020.csv")

#Random sample of 10 000 trips
sample <- sample_n(taxi_trips, 10000)

# Insert of private API key below
register_google(key="###############################")

# Geocode of Chicago
chicago <- geocode("Chicago, IL")

#Displaying pick up Location
pick_up <- tibble(lat=sample$`Pickup Centroid Latitude`, lon=sample$`Pickup Centroid Longitude`)

ggmap(get_map(chicago, zoom=11, maptype="toner-lite"))+
  ggtitle("Pick up locations for a sample of 10 000 taxi trips")+
  geom_point(mapping=aes(x=lon, y=lat), color="black", data=pick_up, alpha=0.05)+
  theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank()) +
  theme(panel.background=element_blank())


#Displaying drop off Location
drop_off <- tibble(lat=sample$`Dropoff Centroid Latitude`, lon=sample$`Dropoff Centroid Longitude`)

ggmap(get_map(chicago, zoom=11, maptype="toner-lite"))+
  ggtitle("Drop off locations for a sample of 10 000 taxi trips")+
  geom_point(mapping=aes(x=lon, y=lat), color="black", data=drop_off, alpha=0.05)+
  theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank()) +
  theme(panel.background=element_blank())

# Geocode of Chicago
chicago <- geocode("Spain")

ggmap(get_map(chicago, zoom=9, maptype="hybrid"))+
  ggtitle("Drop off locations for a sample of 10 000 taxi trips")
  
  

