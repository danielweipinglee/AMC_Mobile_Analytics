getwd()
setwd("/Users/Daniel/Desktop")
GF <- read.csv("Geo-Fence Analytics.csv")

install.packages("dplyr")
library("dplyr")

#1. Data Processing - Create dummy variables
GF <- mutate(GF, imp_large = ifelse(GF$imp_size == "728x90",1,0))
GF <- mutate(GF, cat_enterainment = ifelse("IAB1" %in% GF$app_topcat | "IAB-6" %in% GF$app_topcat,1,0))
GF <- mutate(GF, cat_social = ifelse(GF$app_topcat == "IAB14",1,0))
GF <- mutate(GF, cat_tech = ifelse(GF$app_topcat=="IAB19-6",1,0))
GF <- mutate(GF, os_ios = ifelse(GF$device_os == "ios",1,0))

install.packages("aspace")
library(aspace)

LATITUDE1 <- GF$device_lat
LONGITUDE1 <- GF$device_lon
LATITUDE2 <- GF$geofence_lat
LONGITUDE2 <- GF$geofence_lon

GF <- mutate(GF, distance = 6371*acos(cos(as_radians(LATITUDE1))*cos(as_radians(LATITUDE2))*
                                        cos(as_radians(LONGITUDE1)-as_radians(LONGITUDE2))+sin(as_radians(LATITUDE1))*
                                        sin(as_radians(LATITUDE2))) )

GF <- mutate(GF, distance_squared = distance^2)

GF <- mutate(GF, ln_app_review_vol = log(GF$app_review_vol))

#2. Descriptive Statistics
install.packages("pastecs")
library(pastecs)

attach(GF)
variables<-cbind(didclick, distance, imp_large, cat_entertainment, 
                 cat_social, cat_tech, os_ios, ln_app_review_vol, app_review_val)
stat.desc(variables)

correlations <- cor(variables)
round(correlations, 4)

distance_group = if_else(distance > 0 & distance <= 0.5, 1, 
                         if_else(distance <= 1, 2, 
                                 if_else(distance <= 2, 3,
                                         if_else(distance <= 4, 4,
                                                 if_else(distance <= 7, 5,
                                                         if_else(distance <= 10, 6, 7))))))

GF <- mutate(GF, distance_group)
ctr <- GF %>%
  group_by(distance_group) %>%
  summarise(click_through_rate = mean(didclick))

plot(ctr)

#3. Logistic Regression
glm(didclick~distance + distance_squared + cat_entertainment + imp_large  + cat_social + cat_tech + os_ios + ln_app_review_vol + app_review_val, data = Geo_Fence , family = "binomial")

