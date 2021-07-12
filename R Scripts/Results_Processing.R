setwd("./Functions")
source("wrapper_function.R")

#start stations
setwd("../Data")
start_station_matrix <- readRDS("start_station_matrix.rds")
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat <- adj_mat * (1-cor_mat)
input_data <- start_station_matrix
#run
start_output <- wrapper_function(input_data, input_mat)
start_results <- start_output[[1]]
saveRDS(start_results, "start_results.rds")
start_residuals_sum <- start_output[[2]]
start_residuals_win <- start_output[[3]]
saveRDS(start_residuals_sum, "start_residuals_sum.rds")
saveRDS(start_residuals_win, "start_residuals_win.rds")


#repeat for end stations
end_station_matrix <- readRDS("end_station_matrix.rds")
end_station_matrix$`31718` <-  end_station_matrix[[576]][1,]
end_station_matrix <- end_station_matrix[order(names(end_station_matrix))]
input_data <- end_station_matrix
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat <- adj_mat * (1-cor_mat)
#run
end_output <- wrapper_function(input_data, input_mat)
end_results <- end_output[[1]]
saveRDS(end_results, "end_results.rds")
end_residuals_sum <- end_output[[2]]
end_residuals_win <- end_output[[3]]
saveRDS(end_residuals_sum, "end_residuals_sum.rds")
saveRDS(end_residuals_win, "end_residuals_win.rds")


#for aggregated stations
agg_station_matrix <- readRDS("agg_station_matrix.rds")
input_data <- agg_station_matrix
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat <- adj_mat * (1-cor_mat)
#run
agg_output <- wrapper_function(input_data, input_mat, corr_threshold = 0.15)
agg_results <- agg_output[[1]]
saveRDS(agg_results, "agg_results.rds")
agg_residuals_sum <- agg_output[[2]]
agg_residuals_win <- agg_output[[3]]
saveRDS(agg_residuals_sum, "agg_residuals_sum.rds")
saveRDS(agg_residuals_win, "agg_residuals_win.rds")


#############################################################################################################
#############################################################################################################

#load output
start_station_matrix <- readRDS("start_station_matrix.rds")
end_station_matrix <- readRDS("end_station_matrix.rds")
start_results <- readRDS("start_results.rds")
end_results <- readRDS("end_results.rds")
my_dates <- seq.Date(as.Date("2017/1/1"), as.Date("2019/12/31"), "days")

#matrix of start STATION outliers
start_station_output <- data.frame(matrix(NA, ncol=(length(names(start_station_matrix))+1), nrow=length(my_dates)))
start_station_output[,1] <- my_dates
colnames(start_station_output) <- c("date", names(start_station_matrix))
for (i in 1:length(start_results)){
  cluster_list <- start_results[[i]]
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      all_legs <- cluster_list[j,3]
      select_date <- cluster_list[j,1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)){
        if (!is.na(select_date) & !is.na(leg_list[k])){
          start_station_output[which(start_station_output$date == select_date),leg_list[k]] <- cluster_list[j,2]
        }
      }
    }
  }
}
saveRDS(start_station_output, "start_station_output.rds")

#matrix of end STATION outliers
end_station_output <- data.frame(matrix(NA, ncol=(length(names(end_station_matrix))+1), nrow=length(my_dates)))
end_station_output[,1] <- my_dates
colnames(end_station_output) <- c("date", names(end_station_matrix))
for (i in 1:length(end_results)){
  cluster_list <- end_results[[i]]
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      all_legs <- cluster_list[j,3]
      select_date <- cluster_list[j,1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)){
        if (!is.na(select_date) & !is.na(leg_list[k])){
          end_station_output[which(end_station_output$date == select_date),leg_list[k]] <- cluster_list[j,2]
        }
      }
    }
  }
}
saveRDS(end_station_output, "end_station_output.rds")

#matrix of agg STATION outliers
agg_station_output <- data.frame(matrix(NA, ncol=(length(names(agg_station_matrix))+1), nrow=length(my_dates)))
agg_station_output[,1] <- my_dates
colnames(agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results)){
  cluster_list <- agg_results[[i]]
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      all_legs <- cluster_list[j,3]
      select_date <- cluster_list[j,1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)){
        if (!is.na(select_date) & !is.na(leg_list[k])){
          agg_station_output[which(agg_station_output$date == select_date),leg_list[k]] <- cluster_list[j,2]
        }
      }
    }
  }
}
saveRDS(agg_station_output, "agg_station_output.rds")


#######################################################################################################################
#######################################################################################################################

#matrix of start CLUSTER outliers
start_cluster_output <- data.frame(matrix(NA, ncol=(length(names(start_station_matrix))+1), nrow=length(my_dates)))
start_cluster_output[,1] <- my_dates
colnames(start_cluster_output) <- c("date", names(start_station_matrix))
cluster_names <- names(start_results)
for (i in 1:length(start_results)){
  cluster_list <- start_results[[i]]
  cluster_legs <- trimws(unlist(strsplit(cluster_names[i], " ")))
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      select_date <- cluster_list[j,1]
      for (k in 1:length(cluster_legs)){
        if (!is.na(select_date) & !is.na(cluster_legs[k])){
          start_cluster_output[which(start_cluster_output$date == select_date),cluster_legs[k]] <- cluster_list[j,2]
        }
      }
    }
  }
}
saveRDS(start_cluster_output, "start_cluster_output.rds")


#matrix of end CLUSTER outliers
end_cluster_output <- data.frame(matrix(NA, ncol=(length(names(end_station_matrix))+1), nrow=length(my_dates)))
end_cluster_output[,1] <- my_dates
colnames(end_cluster_output) <- c("date", names(end_station_matrix))
cluster_names <- names(end_results)
for (i in 1:length(end_results)){
  cluster_list <- end_results[[i]]
  cluster_legs <- trimws(unlist(strsplit(cluster_names[i], " ")))
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      select_date <- cluster_list[j,1]
      for (k in 1:length(cluster_legs)){
        if (!is.na(select_date) & !is.na(cluster_legs[k])){
          end_cluster_output[which(end_cluster_output$date == select_date),cluster_legs[k]] <- cluster_list[j,2]
        }
      }
    }
  }
}
saveRDS(end_cluster_output, "end_cluster_output.rds")

#matrix of agg CLUSTER outliers
agg_cluster_output <- data.frame(matrix(NA, ncol=(length(names(agg_station_matrix))+1), nrow=length(my_dates)))
agg_cluster_output[,1] <- my_dates
colnames(agg_cluster_output) <- c("date", names(agg_station_matrix))
cluster_names <- names(agg_results)
for (i in 1:length(agg_results)){
  cluster_list <- agg_results[[i]]
  cluster_legs <- trimws(unlist(strsplit(cluster_names[i], " ")))
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      select_date <- cluster_list[j,1]
      for (k in 1:length(cluster_legs)){
        if (!is.na(select_date) & !is.na(cluster_legs[k])){
          agg_cluster_output[which(agg_cluster_output$date == select_date),cluster_legs[k]] <- cluster_list[j,2]
        }
      }
    }
  }
}
saveRDS(agg_cluster_output, "agg_cluster_output.rds")


#matrix of start STATION outliers
start_station_pos_neg <- data.frame(matrix(NA, ncol=ncol(start_station_output), nrow=nrow(start_station_output)))
start_station_pos_neg[,1] <- my_dates
colnames(start_station_pos_neg) <- colnames(start_station_output)
for (i in 1:nrow(start_station_pos_neg)){
  for (j in 2:ncol(start_station_pos_neg)){
    print(c(i,j))
    if (!is.na(start_station_output[i,j]) & start_station_output[i,j] > 0){
      #what is date
      date_s <- start_station_pos_neg[i,1]
      terminal <- names(start_station_pos_neg)[j]
      #winter
      if (months(date_s) %in% winter){
        d <- filter(start_residuals_win[[which(names(start_residuals_win) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          start_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          start_station_pos_neg[i,j] <- -1
        }
      }
      #winter
      if (months(date_s) %in% summer){
        d <- filter(start_residuals_sum[[which(names(start_residuals_sum) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          start_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          start_station_pos_neg[i,j] <- -1
        }
      }
    }
  }
}
saveRDS(start_station_pos_neg, "start_station_pos_neg.rds")

#repeat for end terminals
#matrix of start STATION outliers
end_station_pos_neg <- data.frame(matrix(NA, ncol=ncol(end_station_output), nrow=nrow(end_station_output)))
end_station_pos_neg[,1] <- my_dates
colnames(end_station_pos_neg) <- colnames(end_station_output)
for (i in 1:nrow(end_station_pos_neg)){
  for (j in 2:ncol(end_station_pos_neg)){
    print(c(i,j))
    if (!is.na(end_station_output[i,j]) & end_station_output[i,j] > 0){
      #what is date
      date_s <- end_station_pos_neg[i,1]
      terminal <- names(end_station_pos_neg)[j]
      #winter
      if (months(date_s) %in% winter){
        d <- filter(end_residuals_win[[which(names(end_residuals_win) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          end_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          end_station_pos_neg[i,j] <- -1
        }
      }
      #winter
      if (months(date_s) %in% summer){
        d <- filter(end_residuals_sum[[which(names(end_residuals_sum) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          end_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          end_station_pos_neg[i,j] <- -1
        }
      }
    }
  }
}
saveRDS(end_station_pos_neg, "end_station_pos_neg.rds")


#aggregated
agg_station_pos_neg <- data.frame(matrix(NA, ncol=ncol(agg_station_output), nrow=nrow(agg_station_output)))
agg_station_pos_neg[,1] <- my_dates
colnames(agg_station_pos_neg) <- colnames(agg_station_output)
for (i in 1:nrow(agg_station_pos_neg)){
  for (j in 2:ncol(agg_station_pos_neg)){
    print(c(i,j))
    if (!is.na(agg_station_output[i,j]) & agg_station_output[i,j] > 0){
      #what is date
      date_s <- agg_station_pos_neg[i,1]
      terminal <- names(agg_station_pos_neg)[j]
      #winter
      if (months(date_s) %in% winter){
        d <- filter(agg_residuals_win[[which(names(agg_residuals_win) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          agg_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          agg_station_pos_neg[i,j] <- -1
        }
      }
      #winter
      if (months(date_s) %in% summer){
        d <- filter(agg_residuals_sum[[which(names(agg_residuals_sum) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          agg_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          agg_station_pos_neg[i,j] <- -1
        }
      }
    }
  }
}
saveRDS(agg_station_pos_neg, "agg_station_pos_neg.rds")




