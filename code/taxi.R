# Taxi discovering
setwd("~/Desktop/EPFL/Stat Vis and Comp")

data = read.csv("Taxi_Trips_-_2020.csv")

head(data)

# Which (standardised) areas generates most revenue? Why? 
# Both pickup, drop-off and Trip 
# (Combination of pickup and drop-off)

data_sample = data[sample(nrow(data), 2000), ]

# EDA
dim(data_sample) # 23 columns

head(data_sample)

summary(data_sample)
attach(data_sample)

# Study distribution of continuous variables
# We are interested in variable: Trip.Total, Fare, Tips
# See the distribution

# Trip.Total
hist(Trip.Total) # a lot of data between 0 and 100, we can't see much

# let's try to convert trip.total variable into log scale --> better to see %
hist(log(Trip.Total)) # seems normal

qqnorm(Trip.Total, pch = 1, frame = FALSE)
qqline(Trip.Total, col = "steelblue", lwd = 2)

# Since we have some 0, can't take log, check the log on the subset bigger than 0
log(data_sample[ which(Trip.Total > 0),]$Trip.Total)

qqnorm(log(data_sample[ which(Trip.Total > 0),]$Trip.Total), pch = 1, frame = FALSE)
qqline(log(data_sample[ which(Trip.Total > 0),]$Trip.Total), col = "steelblue", lwd = 2)

# Fare
hist(Fare) 

hist(log(Fare)) # better in log

# Trip.Seconds

hist(Trip.Seconds) 

hist(log(Trip.Seconds)) # better in log

# Tips

hist(Tips) 

hist(log(Tips)) # better in log

# Tolls - nothing interesting
hist(Tolls) 

hist(log(Tolls)) # better in log


# Extras - nothing interesting
hist(Extras) 

hist(log(Extras)) # better in log

# We are interested in variables : Pickup.Community.Area and Dropoff.Community.Area
pie(table(Pickup.Community.Area)) # in this sample a lot from 8 and 32

pie(table(Dropoff.Community.Area)) # in this sample a lot from 8 and 32

# First, we should consider Areas as factors 

data_sample$Pickup.Community.Area = as.factor(data_sample$Pickup.Community.Area)
data_sample$Dropoff.Community.Area = as.factor(data_sample$Dropoff.Community.Area)

# Boxplots --> Fare according to the pick up and dropoff areas
boxplot(Trip.Total~Pickup.Community.Area, data = data_sample, xlab = "Pickup.Community.Area", ylab = "Trip.Total",
        main = "boxplot of pick up areas") # some outliers

boxplot(Trip.Total~Dropoff.Community.Area, data = data_sample, xlab = "Dropoff.Community.Area", ylab = "Trip.Total",
        main = "boxplot of dropoff areas") # some outliers 

# Boxplots with taking log of the Total.Trip
boxplot(log(Trip.Total)~Pickup.Community.Area, data = data_sample, xlab = "Pickup.Community.Area", ylab = "Trip.Total",
        main = "boxplot of pick up areas") # some outliers, can see better the distribution according to the area

boxplot(log(Trip.Total)~Dropoff.Community.Area, data = data_sample, xlab = "Dropoff.Community.Area", ylab = "Trip.Total",
        main = "boxplot of dropoff areas") # some outliers 



# Boxplots Payment.Type vs Total of the trip
boxplot(Trip.Total~Payment.Type, data = data_sample, xlab = "Payment Type", ylab = "Trip.Total",
        main = "Payment Type") # some outliers

# Boxplots with taking log of the Total.Trip
boxplot(log(Trip.Total)~Payment.Type, data = data_sample, xlab = "Payment Type", ylab = "Trip.Total in log",
        main = "Payment Type") # some outliers, can see better the distribution according to the area
# In cash the total payment seems smaller


# Boxplots Company vs Total of the trip
boxplot(Trip.Total~Company, data = data_sample, xlab = "Company", ylab = "Trip.Total",
        main = "Total depending on the company") # some outliers

# Boxplots with taking log of the Total.Trip
boxplot(log(Trip.Total)~Company, data = data_sample, xlab = "Company", ylab = "Trip.Total in log",
        main = "Log of total depending on company") # some outliers, can see better the distribution according to the area
# In cash the total payment seems smaller


### Scatterplots
plot(Trip.Total, Trip.Seconds)
plot(log(Trip.Total), log(Trip.Seconds)) # correl between 2 seems strong, should consider log, more robust to outliers
plot(Trip.Total, log(Trip.Seconds))
plot(log(Trip.Total), Trip.Seconds)

pairs(~Trip.Total + Fare + Tips + Extras + Tolls + Trip.Seconds)

pairs(~ log(Trip.Total) + log(Fare) + log(Tips) + log(Extras) + log(Tolls) + log(Trip.Seconds))



## Let's combine Pick up and drop off areas
library(tidyverse)
data_sampe_pd = data_sample %>% unite("Pickup_Dropoff", Pickup.Community.Area:Dropoff.Community.Area, remove = FALSE)



data_sampe_pd$Pickup_Dropoff
sort(table(data_sampe_pd$Pickup_Dropoff)) # some areas concentrate more trips


boxplot(Trip.Total~Pickup_Dropoff, data = data_sampe_pd, xlab = "Pickup.Dropoff.Community.Area", ylab = "Trip.Total",
        main = "boxplot of pick up areas") # too many categories

boxplot(Trip.Total~Pickup_Dropoff, data = data_sampe_pd, xlab = "Pickup.Dropoff.Community.Area", ylab = "log Trip.Total",
        main = "boxplot of combin pickup and dropoff areas") # too many categoris

