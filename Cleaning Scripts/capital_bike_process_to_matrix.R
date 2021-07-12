matrix_function <- function(d1){
  d1 <- mutate(d1, hour_of_day = hour(as.POSIXct(strptime(Start.date, "%Y-%m-%d %H:%M"))))
  d1 <- mutate(d1, date_of_day = date(as.POSIXct(strptime(Start.date, "%Y-%m-%d %H:%M"))))
  d1 <- mutate(d1, count = 1)
  d1_grouped <- d1 %>% group_by(date_of_day, hour_of_day)
  d1_grouped <- d1_grouped %>% summarise(count = sum(count))
  d1_wide <- d1_grouped %>% spread(key=hour_of_day, value=count, fill=0)
  #add in blank columns for missing hours
  colnames(d1_wide)
  hours <- as.character(0:23)
  missing_hours <- hours[which(!(hours %in% colnames(d1_wide)))]
  if (length(missing_hours) > 0){
    m_df <- data.frame(matrix(0, nrow=nrow(d1_wide), ncol=length(missing_hours)))
    colnames(m_df) <- missing_hours
    d1_wide <- cbind(d1_wide, m_df)
  }
  #sort columns
  d1_wide <- d1_wide[,c("date_of_day", 0:23)]
  return(d1_wide)
}

#load data
setwd("../Data")
OD_data <- readRDS("OD_data.rds")
data_by_start_station <- split(OD_data, OD_data$Start.station.number)
matrix_function(data_by_start_station[[100]])
start_station_matrix <- lapply(data_by_start_station, function(x) matrix_function(x))
saveRDS(start_station_matrix, "start_station_matrix.rds")

#for end date
matrix_function_end <- function(d1){
  d1 <- mutate(d1, hour_of_day = hour(as.POSIXct(strptime(End.date, "%Y-%m-%d %H:%M"))))
  d1 <- mutate(d1, date_of_day = date(as.POSIXct(strptime(End.date, "%Y-%m-%d %H:%M"))))
  d1 <- mutate(d1, count = 1)
  d1_grouped <- d1 %>% group_by(date_of_day, hour_of_day)
  d1_grouped <- d1_grouped %>% summarise(count = sum(count))
  d1_wide <- d1_grouped %>% spread(key=hour_of_day, value=count, fill=0)
  #add in blank columns for missing hours
  colnames(d1_wide)
  hours <- as.character(0:23)
  missing_hours <- hours[which(!(hours %in% colnames(d1_wide)))]
  if (length(missing_hours) > 0){
    m_df <- data.frame(matrix(0, nrow=nrow(d1_wide), ncol=length(missing_hours)))
    colnames(m_df) <- missing_hours
    d1_wide <- cbind(d1_wide, m_df)
  }
  #sort columns
  d1_wide <- d1_wide[,c("date_of_day", 0:23)]
  return(d1_wide)
}

#make new data frame for each start station
data_by_end_station <- split(OD_data, OD_data$End.station.number)
matrix_function_end(data_by_end_station[[100]])
end_station_matrix <- lapply(data_by_end_station, function(x) matrix_function_end(x))
saveRDS(end_station_matrix, "end_station_matrix.rds")
