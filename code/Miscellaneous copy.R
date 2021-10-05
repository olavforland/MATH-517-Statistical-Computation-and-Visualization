
v1 <- c(1, 2, 3)
v2 <- c("hei", "min", "venn")
v3 <- c(T, F, F)

df <- as.data.frame(cbind(v1, v2, v3))
print(df)

#as.numeric()

if (!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, party, psych, rio, tidyverse)
#pacman: for loading/unloading packages
#party: for decision trees
#psych: for many statistical procedures
#rio: for importing data
#tidyverse: everything

#loads manually
library(datasets)

(df <- read_csv("data/StateData.csv"))

x <- c(24, 13, 7, 5, 3, 2)
barplot(x, col="red3")

diamonds %>%
  select(clarity) %>%
  table%>%
  barplot(
    main = "Clarity of diamonds",
    sub = "Source: ggplot",
    horiz = T,
    ylab = "Clarity of diamond",
    xlab = "Frequency",
    xlim = c(0, 15000),
    border = NA,
    col = "red3"
  )

hist(diamonds$price,
     breaks=7,
     main="Histogram of price of diamonds",
     sub="Source: ggplot2::diamonds",
     xlab="Price",
     ylab="Frequency",
     border=NA,
     col="red3"
     )
  
#histogram to see the shape of my distrubution
#boxplot to cover outliers, which can have a dramatic effect on my analysis. Why?
#they let you look at distributions very quickly to localize outliers, 
#compare multiple groups on the same variable/variables on the same scale,
#check quickly if data meets assumptions of the procedures you will use

boxplot(diamonds$price)

diamonds %>%
  select(price) %>%
  boxplot(
    horizontal=T, #is nice if you compare directly to histogram, same axis
    main="Boxplot of diamond prices",
    sub="Source: ggplot2::diamonds",
    xlab="Price of diamonds",
    col="red3"
  )

diamonds %>%
  select(color, price) %>%
  boxplot(
    price~color,
    data=. ,
    col="red3"
  )

#scatterplot: look at how variables are dependant

df <- import("data/StateData.xlsx") %>%
  as_tibble() %>%
  select(state_code,
         psychRegions,
         instagram:modernDance) %>%
  mutate(psychRegions=as.factor(psychRegions)) %>%
  print()

df %>%
  select(instagram:modernDance) %>%
  plot() #this plots all associations

df %>%
  select(scrapbook:modernDance) %>%
  plot(
    main="Scatterplot",
    sub="Source",
    xlab="Searches for \"scrapbook\"",
    ylab="Searches for \"modern dance\"",
    col="gray",
    pch=20
  )
df %>%
  filter(scrapbook < 4) %>%
  lm(modernDance~scrapbook, data= .) %>%
  abline()
#this gives an upwards trend because of the outlier: although it is clearly not the case for the rest
#after adding the filter it is clear how big the differnece is
uspop %>%
  plot(
    main="US Population 1790 - 1970",
    sub="(Source: datasets::uspop)",
    xlab="Year",
    ylab="Population (in millions)"
  )
abline(v=1930, col="lightgray")
text(1930, 10, "1930", col="red3")
abline(v=1940, col="lightgrey")
text(1940, 10, "1940", col="red3")

EuStockMarkets
ts.plot(EuStockMarkets)

pacman::p_load(pacman, rio, tidyverse)

df <- import("data/StateData.xlsx") %>%
  as_tibble() %>%
  select(state_code,
         region,
         psychRegions,
         #instagram:modernDance
  ) %>%
  mutate(psychRegions = as.factor(psychRegions)) %>%
  #rename(y=psychRegions) %>%
  print()

ct <- table(df$region, df$psychRegions)
ct

p_load(magrittr)

ct %>%
  prop.table(1) %>% #is to specify row percentages
  round(2) %>%
  multiply_by(100)

df %>%
  select(instagram:modernDance) %>%
  plot()

df %>%
  select(museum, volunteering) %>%
  plot()

lm(df$volunteering ~ df$museum) %>% abline()

fit1 <- lm(df$volunteering ~ df$museum)

fit1
summary(fit1)
confint(fit1)

df <- df %>%
  select(volunteering, everything()) %>%
  print()

fit2 <- lm(df) #asumes you're using a linear model and 
               #that predicted value is first, and all others come behind it
fit2

cor(df)

df %>% 
  cor(df) %>%
  round(2)

cor.test(df$instagram, df$privacy)

p_load(Hmisc)

df %>%
  as.matrix() %>%
  rcorr()


df %>%
  summary()

df %>%
  select(entrepreneur) %>%
  summary()

fivenum(df$entrepreneur)

df %>%
  select(region) %>%
  table()

boxplot(df$entrepreneur, notch = T, horizontal = T)
boxplot.stats(df$entrepreneur)

p_load(psych)

p_help(psych, web = F)

describe(df$entrepreneur)
describe(df)

df <- df %>%
  mutate(psychRegions = as.factor(psychRegions)) %>%
  mutate(region = as.factor(region)) %>%
  print()

summary(df)

df %>%
  mutate(relaxed = recode(psychRegions, 
                          "Relaxed and Creative" = "yes",
                          "Friendly and Conventional" = "no",
                          .default = "no")) %>%
  select(state_code, psychRegions, relaxed)

df %>%
  mutate( #function for creating new variables
    socialMedia = (instagram + facebook + retweet) / 3,
    artsCraft = (museum + scrapbook + modernDance) / 3,
  ) %>%
  select(state_code, socialMedia, artsCraft)

df %>%
  mutate(
    outgoing = (volunteering + (privacy * -1)) / 2
  ) %>%
  select(state_code, outgoing, volunteering, privacy)

#check out scale and psych packages

df2 <- df %>%
  mutate(likesArt = case_when(
    museum > 1 | scrapbook > 1 | modernDance > 1 ~ "yes",
    TRUE ~ "no")) 
  
df2 %>%
  select(state_code, likesArt, museum:modernDance) %>%
  arrange(desc(likesArt)) %>%
  print(n=Inf)




df %>%
  filter(entrepreneur > 1) %>%
  print()

df %>%
  filter(region == "South") %>%
  print()













df %>%
  dist %>%
  hclust

df %>%
  filter(psychRegions == "Relaxed and Creative") %>%
  print()

df %>%
  filter(psychRegions == "Relaxed and Creative" &
         region == "South") %>%
  print()



hc %>% 
  plot(labels=df$state_code)

hc %>% rect.hclust(k=2, border="red3")
hc %>% rect.hclust(k=3, border="red3")
hc %>% rect.hclust(k=4, border="red3")
