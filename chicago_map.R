#Plotting a map of Chicago

pacman::p_load(tidyverse, pacman, ggmap, janitor, sf, mapview, leaflet, rgdal, RColorBrewer)


#Register API key to google
register_google(key="AIzaSyBT6yzDKV_DGcFGK9E-cXu0zNUD4WTJOZA")

taxi_trips<-read_csv("./data/Taxi_Trips_-_2020.csv") %>%
  clean_names() %>%
  print()

#Extracting a random sample so plotting is done more quickly
sample <- taxi_trips %>%
  sample_n(10000)
colnames(sample)

trip_total_in_area <- taxi_trips %>%
  select(pickup_community_area, trip_total) %>%
  mutate(pickup_community_area = as.factor(pickup_community_area),
         trip_total = (as.numeric(pickup_community_area))) %>%
  group_by(pickup_community_area) %>%
  summarise(trip_total = sum(trip_total)) %>%
  print()

glimpse(trip_total_in_area)

#Get location of chicago
chicago <- geocode("Chicago, IL")

#Make leaflet map with chicago center
chicago_leaflet <- leaflet() %>% 
  setView(lng=chicago$lon, lat=chicago$lat, zoom=10) %>%
  addProviderTiles(providers$CartoDB.Positron) 


#Save chicago areas to use for plotting
areas <- st_read("./data/chicago_community_areas/chicago_community_areas.shp", as_tibble=T, quiet=T) %>%
  clean_names() %>%
  select(area_numbe, community, geometry) %>%
  mutate(geometry = st_geometry(geometry),
         area_numbe = as.factor(area_numbe)) %>% #if this stops working extract it to own variable 
  rename(pickup_community_area = area_numbe,
         community_name = community) %>%
  inner_join(trip_total_in_area, by="pickup_community_area") %>%
  print()

print(areas)
#Trying to color each area by trip_total
pal <- colorBin("Blues",bins=4, domain =log10(areas$trip_total), )

#Generate html labels
labels <- sprintf(
  "<strong>%s</strong><br/>%g$ trip total",
  areas$community_name, areas$trip_total
) %>% lapply(htmltools::HTML)

#Make the map
chicago_leaflet %>%
  addPolygons(data=areas,
              fillColor=~pal(log10(areas$trip_total)),
              color="lightgrey",
              weight=2,
              fillOpacity=0.8,
              highlightOptions = highlightOptions(
                weight=3,
                opacity=2,
                color="azure1",
                bringToFront=TRUE),
              label=labels,
              labelOptions=labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  
  addLegend("bottomright",
            pal = pal,
            values = log10(areas$trip_total),
            title = "Trip total",
            opacity = 0.8,
            labFormat = leaflet::labelFormat(
              transform = function(x) 10^(x)))





#Sort based on hour of the day
taxi_trips$trip_start=(paste(substr(taxi_trips$trip_start_timestamp,12,13),substr(taxi_trips$trip_start_timestamp,21,22)))

glimpse(taxi_trips)

taxi_trips %>% group_by(trip_start) %>% count()
taxi_trips$trip_start[taxi_trips$trip_start == "12 AM"] <- 1
taxi_trips$trip_start[taxi_trips$trip_start == "01 AM"] <- 2
taxi_trips$trip_start[taxi_trips$trip_start == "02 AM"] <- 3
taxi_trips$trip_start[taxi_trips$trip_start == "03 AM"] <- 4
taxi_trips$trip_start[taxi_trips$trip_start == "04 AM"] <- 5
taxi_trips$trip_start[taxi_trips$trip_start == "05 AM"] <- 6
taxi_trips$trip_start[taxi_trips$trip_start == "06 AM"] <- 7
taxi_trips$trip_start[taxi_trips$trip_start == "07 AM"] <- 8
taxi_trips$trip_start[taxi_trips$trip_start == "08 AM"] <- 9
taxi_trips$trip_start[taxi_trips$trip_start == "09 AM"] <- 10
taxi_trips$trip_start[taxi_trips$trip_start == "10 AM"] <- 11
taxi_trips$trip_start[taxi_trips$trip_start == "11 AM"] <- 12
taxi_trips$trip_start[taxi_trips$trip_start == "12 PM"] <- 13
taxi_trips$trip_start[taxi_trips$trip_start == "01 PM"] <- 14
taxi_trips$trip_start[taxi_trips$trip_start == "02 PM"] <- 15
taxi_trips$trip_start[taxi_trips$trip_start == "03 PM"] <- 16
taxi_trips$trip_start[taxi_trips$trip_start == "04 PM"] <- 17
taxi_trips$trip_start[taxi_trips$trip_start == "05 PM"] <- 18
taxi_trips$trip_start[taxi_trips$trip_start == "06 PM"] <- 19
taxi_trips$trip_start[taxi_trips$trip_start == "07 PM"] <- 20
taxi_trips$trip_start[taxi_trips$trip_start == "08 PM"] <- 21
taxi_trips$trip_start[taxi_trips$trip_start == "09 PM"] <- 22
taxi_trips$trip_start[taxi_trips$trip_start == "10 PM"] <- 23
taxi_trips$trip_start[taxi_trips$trip_start == "11 PM"] <- 24

glimpse(taxi_trips)

taxi_trips %>% group_by(trip_start, pickup_community_area) %>% count()

class(areas$geometry)


