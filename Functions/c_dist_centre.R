library(geosphere)

#load station coordinates
setwd("C:/Users/rennien/OneDrive - Lancaster University/PhD/Simulation/Project 4 - Bike Visualisation/Bike_Data/Stations")
station_data <- readRDS("station_data.rds")

#find spatial median
c <- geomean(station_data[,c(3,2)])
c #median???

library(ICSNP)
c <- spatial.median(station_data[,c(3,2)])
c

c_dist_centre <- function(station){
  a <- which(station_data$TERMINAL_NUMBER == station)
  lat_a <- station_data$LATITUDE[a]
  lat_b <- c[2]
  lon_a <- station_data$LONGITUDE[a]
  lon_b <- c[1]
  distance <- distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)
  return(distance)
}

#for each distance, compute distance to centre of dc
centre_distances <- sapply(station_data$TERMINAL_NUMBER, function(x) c_dist_centre(x))
station_data$centre_distances <- centre_distances

setwd("C:/Users/rennien/OneDrive - Lancaster University/PhD/Simulation/Project 4 - Bike Visualisation/Bike_Data/Stations")
saveRDS(station_data, "station_data.rds")
