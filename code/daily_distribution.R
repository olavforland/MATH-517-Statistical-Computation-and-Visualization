#loading useful packages
pacman::p_load(pacman, party, psych, rio, tidyverse, plotly, rjson, corrr) 
p_load(janitor)

#importing taxitrips as a tibble
taxi_trips <- read_csv("data/taxi_trips_2020.csv") %>%
  clean_names() %>%
  print()

#Extracting a random sample so plotting is done more quickly
sample <- taxi_trips %>%
  sample_n(10000)
#How are the trips distributed over the day?

sample %>%
  group_by(pickup_community_area, dropoff_community_area) %>%
  summarise(total = sum(trip_total)) %>%
  mutate(total=as.factor(total)) %>%
  ggplot() +
  geom_point(mapping=aes(x=pickup_community_area, y=dropoff_community_area, size=total))

df1 %>%
  ggplot() +
  geom_histogram(mapping=aes(x=pickup_community_area), binwidth=1)

monthly_trips <- taxi_trips %>%
  select(trip_start_timestamp) %>%
  mutate(trip_start_timestamp = lubridate::mdy_hms(trip_start_timestamp)) %>%
  group_by(month = lubridate::month(trip_start_timestamp), hour = lubridate::hour(trip_start_timestamp)) %>%
  mutate(count = n()) %>%
  print()