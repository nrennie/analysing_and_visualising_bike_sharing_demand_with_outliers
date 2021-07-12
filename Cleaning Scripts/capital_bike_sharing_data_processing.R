library(tidyverse)
library(lubridate)

#data from https://s3.amazonaws.com/capitalbikeshare-data/index.html

setwd("./Data/Raw Data")

#load data for 2017
d_2017_Q1 <- read.csv("2017Q1-capitalbikeshare-tripdata.csv")
d_2017_Q2 <- read.csv("2017Q2-capitalbikeshare-tripdata.csv")
d_2017_Q3 <- read.csv("2017Q3-capitalbikeshare-tripdata.csv")
d_2017_Q4 <- read.csv("2017Q4-capitalbikeshare-tripdata.csv")
d_2017 <- bind_rows(list(d_2017_Q1, d_2017_Q2, d_2017_Q3, d_2017_Q4))
saveRDS(d_2017, file = "d_2017.rds")

#load data for 2018
d_2018_01 <- read.csv("201801-capitalbikeshare-tripdata.csv")
d_2018_02 <- read.csv("201802-capitalbikeshare-tripdata.csv")
d_2018_03 <- read.csv("201803-capitalbikeshare-tripdata.csv")
d_2018_04 <- read.csv("201804-capitalbikeshare-tripdata.csv")
d_2018_05 <- read.csv("201805-capitalbikeshare-tripdata.csv")
d_2018_06 <- read.csv("201806-capitalbikeshare-tripdata.csv")
d_2018_07 <- read.csv("201807-capitalbikeshare-tripdata.csv")
d_2018_08 <- read.csv("201808-capitalbikeshare-tripdata.csv")
d_2018_09 <- read.csv("201809-capitalbikeshare-tripdata.csv")
d_2018_10 <- read.csv("201810-capitalbikeshare-tripdata.csv")
d_2018_11 <- read.csv("201811-capitalbikeshare-tripdata.csv")
d_2018_12 <- read.csv("201812-capitalbikeshare-tripdata.csv")
d_2018 <- bind_rows(list(d_2018_01, d_2018_02, d_2018_03, d_2018_04,d_2018_05, d_2018_06, d_2018_07, d_2018_08,d_2018_09, d_2018_10, d_2018_11, d_2018_12))
saveRDS(d_2018, file = "d_2018.rds")

#load data for 2019
d_2019_01 <- read.csv("201901-capitalbikeshare-tripdata.csv")
d_2019_02 <- read.csv("201902-capitalbikeshare-tripdata.csv")
d_2019_03 <- read.csv("201903-capitalbikeshare-tripdata.csv")
d_2019_04 <- read.csv("201904-capitalbikeshare-tripdata.csv")
d_2019_05 <- read.csv("201905-capitalbikeshare-tripdata.csv")
d_2019_06 <- read.csv("201906-capitalbikeshare-tripdata.csv")
d_2019_07 <- read.csv("201907-capitalbikeshare-tripdata.csv")
d_2019_08 <- read.csv("201908-capitalbikeshare-tripdata.csv")
d_2019_09 <- read.csv("201909-capitalbikeshare-tripdata.csv")
d_2019_10 <- read.csv("201910-capitalbikeshare-tripdata.csv")
d_2019_11 <- read.csv("201911-capitalbikeshare-tripdata.csv")
d_2019_12 <- read.csv("201912-capitalbikeshare-tripdata.csv")
d_2019 <- bind_rows(list(d_2019_01, d_2019_02, d_2019_03, d_2019_04,d_2019_05, d_2019_06, d_2019_07, d_2019_08,d_2019_09, d_2019_10, d_2019_11, d_2019_12))
saveRDS(d_2019, file = "d_2019.rds")

#load in rds data and join together
d_2017 <- readRDS("d_2017.rds")
d_2018 <- readRDS("d_2018.rds")
d_2019 <- readRDS("d_2019.rds")
OD_data <- bind_rows(list(d_2017, d_2018, d_2019))

#make OD column
OD <- apply(OD_data, 1, function(x) gsub(" ", "", paste(x[4],"-",x[6], sep="")))
OD_data$OD <- OD

#station data
#data from https://opendata.dc.gov/datasets/capital-bike-share-locations/geoservice?geometry=-78.017%2C38.734%2C-75.931%2C39.108
station_list <- unique(c(OD_data$Start.station.number, OD_data$End.station.number))
stations = read.csv("Capital_Bike_Share_Locations.csv")
station_list <- station_list[which(station_list %in% stations$TERMINAL_NUMBER)]

#save data
OD_data <- OD_data[which(OD_data$Start.station.number %in% station_list & OD_data$End.station.number %in% station_list),]
setwd("../Data")
saveRDS(OD_data, file = "OD_data.rds")

#load data and join coordinates
OD_data <- readRDS("OD_data.rds")
station_list <- sort(unique(c(OD_data$Start.station.number, OD_data$End.station.number)))
stations = read.csv("Capital_Bike_Share_Locations.csv")
s1 <- stations[which(stations$TERMINAL_NUMBER %in% station_list),c(4,5,6)]
station_data <- s1[order(s1$TERMINAL_NUMBER),]
saveRDS(station_data, file = "station_data.rds")
