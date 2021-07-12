#data
setwd("../Data")
start_station_matrix <- readRDS("start_station_matrix.rds")
end_station_matrix <- readRDS("end_station_matrix.rds")

#functions
`%notin%` <- Negate(`%in%`)

#fix end terminal
end_station_matrix$`31718` <-  tibble()
#which(names(start_station_matrix) %notin% names(end_station_matrix))
end_station_matrix <- end_station_matrix[order(names(end_station_matrix))]

aggregate_start_end <- function(start, end){
  start_names <- colnames(start)
  #add start dates into end dates
  missing_end <- which(start$date_of_day %notin% end$date_of_day)
  if (length(missing_end) > 0){
    for (i in 1:length(missing_end)){
      k <- data.frame(list(as.Date(start$date_of_day[missing_end[i]]), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
      names(k) <- start_names
      end <- rbind(end, k)        
    }
  }
  end <- end[order(end$date_of_day),]
  #add end dates into start dates
  missing_start <- which(end$date_of_day %notin% start$date_of_day)
  if (length(missing_start) > 0){
    for (i in 1:length(missing_start)){
      k <- data.frame(list(as.Date(end$date_of_day[missing_start[i]]), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
      names(k) <- start_names
      start <- rbind(start, k)        
    }
  }
  start <- start[order(start$date_of_day),]
  #check
  #sum(start$date_of_day != end$date_of_day)
  #add by date
  agg <- start[,2:25] + end[,2:25]
  agg_df <- tibble(date_of_day=start$date_of_day, agg)
  return(agg_df)
}

#agg1 <- aggregate_start_end(start_station_matrix[[1]], end_station_matrix[[1]])
#agg1

#apply to all list elements and save
num <- length(start_station_matrix)
agg_station_matrix <- list()
for (i in 1:num){
  print(i)
  agg_station_matrix[[i]] <- aggregate_start_end(start_station_matrix[[i]], end_station_matrix[[i]])
}
names(agg_station_matrix) <- names(start_station_matrix)
saveRDS(agg_station_matrix, "agg_station_matrix.rds")

#cor mat
library(fdapace)
correlation_matrix_function <- function(station_names, times=1:24){
  output <- matrix(NA, ncol=length(station_names), nrow=length(station_names))
  colnames(output) <- station_names
  rownames(output) <- station_names
  for (i in 1:length(station_names)){
    #load ith data
    d1 <- agg_station_matrix[[i]]
    for (j in 1:length(station_names)){
      #load jth data
      d2 <- agg_station_matrix[[j]]
      if (i > j) {
        print(c(i,j))
        #get common dates
        common_dates <- intersect(d1$date_of_day, d2$date_of_day)
        if (length(common_dates) > 1){
          fcor <- mean(DynCorr(as.matrix(d1[which(d1$date_of_day %in% common_dates),2:25]), as.matrix(d2[which(d2$date_of_day %in% common_dates),2:25]), t=times))
          output[i,j] <- fcor
          output[j,i] <- fcor
        }
        else {
          output[i,j] <- 0
          output[j,i] <- 0
        }
      }
    }
  }
  return(output)
}

#correlation matrix
agg_cor_mat <- correlation_matrix_function(station_data$TERMINAL_NUMBER) 
saveRDS(agg_cor_mat, "agg_cor_mat.rds")
