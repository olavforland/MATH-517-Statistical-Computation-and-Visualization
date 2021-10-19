#Plotting a map of Chicago

pacman::p_load(tidyverse, pacman, ggmap, janitor, sf, mapview, leaflet, rgdal, RColorBrewer)


#Register API key to google
register_google(key="AIzaSyBT6yzDKV_DGcFGK9E-cXu0zNUD4WTJOZA")

taxi_trips <- read_csv("data/taxi_trips_2020.csv") %>%
  clean_names() %>%
  print()

#Extracting a random sample so plotting is done more quickly
sample <- taxi_trips %>%
  sample_n(10000)
colnames(sample)

trip_total_in_area <- sample %>%
  select(pickup_community_area, trip_total) %>%
  mutate(pickup_community_area = as.factor(pickup_community_area),
         trip_total = as.numeric(pickup_community_area)) %>%
  group_by(pickup_community_area) %>%
  summarise(trip_total = sum(trip_total)) %>%
  print()

#Get location of chicago
chicago <- geocode("Chicago, IL")

#Make leaflet map with chicago center
chicago_leaflet <- leaflet() %>% 
  setView(lng=chicago$lon, lat=chicago$lat, zoom=10) %>%
  addProviderTiles(providers$CartoDB.Positron) 
  

#Save chicago areas to use for plotting
areas <- st_read("data/chicago_community_areas/chicago_community_areas.shp", as_tibble=T, quiet=T) %>%
  select(area_numbe, community, geometry) %>%
  mutate(geometry = st_geometry(geometry),
         area_numbe = as.factor(area_numbe)) %>% #if this stops working extract it to own variable 
  rename(pickup_community_area = area_numbe,
         community_name = community) %>%
  inner_join(trip_total_in_area, by="pickup_community_area") %>%
  print()

#Trying to color each area by trip_total
pal <- colorNumeric("Blues", domain = areas$trip_total)


#Make the map
chicago_leaflet %>%
  addPolygons(data=areas,
              fillColor=~pal(areas$trip_total),
              color="grey",
              weight=2, 
              highlightOptions = highlightOptions(
                weight=5,
                color="#666",
                bringToFront=TRUE)) %>%
              
  addLegend("bottomright",
            pal = pal,
            values = areas$trip_total,
            title = "Trip total",
            opacity = 1)


class(areas$geometry)



#-------------------------------------------------------------------------
#Earlier methods with ggplot

ggplot(areas_geom) +
  geom_polygon(mapping=aes(x=st_coordinates(areas_geom)["X"],
                           y=st_coordinates(areas_geom)["Y"]), 
               color="grey", fill="beige")
test_chicago <- map_data("state", region="Illinois")
areas_geom

ggplot() +
  geom_sf(data = areas_geom) 

plot(areas_geom)

ggplot(chicago) +
  geom_polygon(mapping=aes(x=lon, y=lat), color="grey", fill="beige") +
  geom_polygon(data=areas_geom, mapping=aes(x=st_coordinates(areas_geom)["X"],
                                            y=st_coordinates(areas_geom)["Y"])) +
  coord_map()



