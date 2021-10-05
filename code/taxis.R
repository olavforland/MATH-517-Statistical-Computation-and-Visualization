#loading useful packages
pacman::p_load(pacman, party, psych, rio, tidyverse) 
p_load(janitor)

taxi_trips <- import("data/taxi_trips.csv") %>%
  as_tibble() %>%
  clean_names() %>% #from janitor package to remove space from names
  print()


#Extracting random sample
sample <- taxi_trips %>% 
  sample_n(10000) 
  



df1 <- sample %>%
  mutate(pickup_community_area = as.numeric(pickup_community_area)) %>%
  select(pickup_community_area, trip_total) %>%
  mutate(across(where(is.numeric), scale)) %>% #standardize
  group_by(pickup_community_area) %>%
  arrange(pickup_community_area) %>%
  summarise(total = sum(trip_total, na.rm=T)) %>%
  #hist(df1$pickup_community_area, df1$total) 
  print()
  

