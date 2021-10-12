#loading useful packages
pacman::p_load(pacman, party, psych, rio, tidyverse, plotly, rjson, corrr) 
p_load(janitor)

#importing taxitrips as a tibble
taxi_trips <- read_csv("data/taxi_trips_2020.csv") %>%
  clean_names() %>%
  print()

#I want to start with exploring the relationships between pickup and dropoff locations in terms of revenue
df1 <- taxi_trips %>%
  select(pickup_community_area, dropoff_community_area, trip_total) %>%
  mutate(pickup_community_area = as.numeric(pickup_community_area),
         dropoff_community_area = as.numeric(dropoff_community_area),
         trip_total=as.numeric(trip_total))

#Extracting a random sample so plotting is done more quickly
sample <- df1 %>%
  sample_n(10000)

#initial distribution of amount of taxitips -> almost all trips start from the same areas
sample %>%
  ggplot() +
  geom_bar(mapping=aes(x=pickup_community_area)) #set fill to company to see bigest acteurs
  
#Question: which areas?
trips_region <- sample %>%
  group_by(pickup_community_area) %>%
  summarize(count=n()) %>%
  drop_na() 

trips_region %>%
  top_n(n=20, count) %>%
  ggplot(mapping=aes(x=reorder(pickup_community_area, -count), y=count)) +
  geom_bar(stat="identity")

#How does this translate into revenue? (per trip)

mean_trip_region <- sample %>%
  group_by(pickup_community_area) %>%
  drop_na() %>%
  summarize(mean=mean(trip_total)) 

mean_trip_region %>%
  top_n(n=20, mean) %>%
  ggplot(mapping=aes(x=reorder(pickup_community_area, -mean), y=mean)) +
  geom_col() 
  
#Not very informative -> How is the correlation between activity in an area and revenue per trip?

mean_count <- inner_join(trips_region, mean_trip_region)
mean_count %>%
  select(count, mean) %>%
  corrr::correlate(method="pearson")

corrr::correlate(mean_count, method = "pearson")

#No correlation detected


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

