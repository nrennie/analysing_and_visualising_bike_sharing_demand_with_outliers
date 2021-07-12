#source functions
source("residuals_function.R")
source("mst_clustering.R")
source("depth.R")
source("depth_threshold.R")
source("merge_differences.R")
source("beta_probs.R")
source("name_rem_x.R")
source("season_split.R")
source("day_split.R")
source("day_season_split.R")

#packages
library(POT)
library(tidyverse)
library(mrfDepth)
library(MASS)
library(igraph)


#for aggregated stations
setwd("./Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")
input_data <- agg_station_matrix
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat <- adj_mat * (1-cor_mat)
#run
agg_output_reg <- wrapper_function(input_data, input_mat, corr_threshold = 0.15)
agg_results_reg <- agg_output_reg[[1]]
saveRDS(agg_results_reg, "agg_results_reg.rds")

#matrix of agg STATION outliers
agg_station_output <- data.frame(matrix(NA, ncol=(length(names(agg_station_matrix))+1), nrow=length(my_dates)))
agg_station_output[,1] <- my_dates
colnames(agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results_reg)){
  cluster_list <- agg_results_reg[[i]]
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

day_of_week <- weekdays(agg_station_output$date)
num_outs <- apply(agg_station_output[,2:579], 1, function(x) sum(x > 0, na.rm=T))
plot_data1_reg <- tibble(day_of_week, num_outs)
plot_data1_reg <- plot_data1_reg %>% group_by(day_of_week)
plot_data1_reg <- plot_data1_reg %>% summarise(count = sum(num_outs))
plot_data1_reg$count <- plot_data1_reg$count/sum(plot_data1_reg$count)
plot_data1_reg$type <- rep("S/W/Wd/We Partition\n+ Regression", 7)

month_of_year <- months(agg_station_output$date)
num_outs <- apply(agg_station_output[,2:579], 1, function(x) sum(x > 0, na.rm=T))
plot_data2_reg <- tibble(month_of_year, num_outs)
plot_data2_reg <- plot_data2_reg %>% group_by(month_of_year)
plot_data2_reg <- plot_data2_reg %>% summarise(count = sum(num_outs))
plot_data2_reg$count <- plot_data2_reg$count/sum(plot_data2_reg$count)
plot_data2_reg$type <- rep("S/W/Wd/We Partition\n+ Regression", 12)


###without regression
wrapper_function_no_reg <- function(input_data, input_mat, times=0:23, perc=0.01, B=10, corr_threshold=0, weekday=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), weekend=c("Saturday", "Sunday"), summer=c("April", "May", "June", "July", "August", "September", "October"), winter=c("November", "December", "January", "February", "March")){
  #clustering procedure
  clusters <- mst_clustering_threshold(input_mat, corr_threshold=corr_threshold)
  clustering <- clusters$cluster_list
  #split data into summer and winter
  day_data <- sapply(input_data, function(x) day_season_split(x, weekday=weekday, weekend=weekend, summer=summer, winter=winter))
  weekday_s_data <- day_data["weekday_s",]
  weekend_s_data <- day_data["weekend_s",]
  weekday_w_data <- day_data["weekday_w",]
  weekend_w_data <- day_data["weekend_w",]
  #run outlier detection
  weekday_s_depths_list <- list()
  for (i in 1:length(weekday_s_data)){
    weekday_s_depths_list[[i]] <- depth(weekday_s_data[[i]], times=times, perc=perc, B=B)
  }
  names(weekday_s_depths_list) <- names(weekday_s_data)
  weekend_s_depths_list <- list()
  for (i in 1:length(weekend_s_data)){
    weekend_s_depths_list[[i]] <- depth(weekend_s_data[[i]], times=times, perc=perc, B=B)
  }
  names(weekend_s_depths_list) <- names(weekend_s_data)
  weekday_w_depths_list <- list()
  for (i in 1:length(weekday_w_data)){
    weekday_w_depths_list[[i]] <- depth(weekday_w_data[[i]], times=times, perc=perc, B=B)
  }
  names(weekday_w_depths_list) <- names(weekday_w_data)
  weekend_w_depths_list <- list()
  for (i in 1:length(weekend_w_data)){
    weekend_w_depths_list[[i]] <- depth(weekend_w_data[[i]], times=times, perc=perc, B=B)
  }
  names(weekend_w_depths_list) <- names(weekend_w_data)
  #join data back together
  depths_list <- mapply(c, weekday_s_depths_list, weekend_s_depths_list, weekday_w_depths_list, weekend_w_depths_list, SIMPLIFY=FALSE)
  #for loop for each cluster 
  alert_lists <- list()
  for (c in 1:length(clustering)){
    #extract elements in cluster c from input list
    cluster_c <- depths_list[which(names(depths_list) %in% clustering[[c]])]
    #if cluster empty, return an empty data frame
    if (length(cluster_c) == 1 & length(cluster_c[[1]]) == 0) {
      alert_lists[[c]] <- data.frame(Dep=character(), Prob=double(), Legs=character())
    } else {
      #merge the legs in the cluster
      x <- merge_differences(l=cluster_c)
      colnames(x) <- sapply(colnames(x), function(y) name_rem_x(y))
      #obtain the probability table
      alert_lists[[c]] <- gpd_probs(x)
    }
  }
  names(alert_lists) <- unlist(lapply(clustering, function(x) paste(x,collapse=" ", sep=",")))
  return(list(alert_lists))
}


################################### NO REGRESSION ################################################
################################### NO REGRESSION ################################################

agg_output_no_reg <- wrapper_function_no_reg(input_data, input_mat)
agg_results_no_reg <- agg_output_no_reg[[1]]
saveRDS(agg_results_no_reg, "agg_results_no_reg.rds")
agg_station_output <- data.frame(matrix(NA, ncol=(length(names(agg_station_matrix))+1), nrow=length(my_dates)))
agg_station_output[,1] <- my_dates
colnames(agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results_no_reg)){
  cluster_list <- agg_results_no_reg[[i]]
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

day_of_week <- weekdays(agg_station_output$date)
num_outs <- apply(agg_station_output[,2:579], 1, function(x) sum(x > 0, na.rm=T))
plot_data1_no_reg <- tibble(day_of_week, num_outs)
plot_data1_no_reg <- plot_data1_no_reg %>% group_by(day_of_week)
plot_data1_no_reg <- plot_data1_no_reg %>% summarise(count = sum(num_outs))
plot_data1_no_reg$count <- plot_data1_no_reg$count/sum(plot_data1_no_reg$count)
plot_data1_no_reg$type <- rep("S/W/Wd/We Partition\n+ No Regressionn", 7)

month_of_year <- months(agg_station_output$date)
num_outs <- apply(agg_station_output[,2:579], 1, function(x) sum(x > 0, na.rm=T))
plot_data2_no_reg <- tibble(month_of_year, num_outs)
plot_data2_no_reg <- plot_data2_no_reg %>% group_by(month_of_year)
plot_data2_no_reg <- plot_data2_no_reg %>% summarise(count = sum(num_outs))
plot_data2_no_reg$count <- plot_data2_no_reg$count/sum(plot_data2_no_reg$count)
plot_data2_no_reg$type <- rep("S/W/Wd/We Partition\n+ No Regression", 12)

wrapper_function_reg_np <- function(input_data, input_mat, times=0:23, perc=0.01, B=10, corr_threshold=0, summer=c("April", "May", "June", "July", "August", "September", "October"), winter=c("November", "December", "January", "February", "March")){
  #clustering procedure
  clusters <- mst_clustering_threshold(input_mat, corr_threshold=corr_threshold)
  clustering <- clusters$cluster_list
  #run residuals on every list item
  residuals <- list()
  for (i in 1:length(input_data)){
    residuals[[i]] <- residuals_function(input_data[[i]], c(1,1,1))
  }
  names(residuals) <- names(input_data)
  #residuals <- lapply(input_data, function(x) residuals_function(x, c(1,1,1))) 
  #run outlier detection
  depths_list <- list()
  for (i in 1:length(residuals)){
    depths_list[[i]] <- depth(residuals[[i]], times=times, perc=perc, B=B)
  }
  names(depths_list) <- names(residuals)
  #for loop for each cluster 
  alert_lists <- list()
  for (c in 1:length(clustering)){
    print(c)
    #extract elements in cluster c from input list
    cluster_c <- depths_list[which(names(depths_list) %in% clustering[[c]])]
    #if cluster empty, return an empty data frame
    if (length(cluster_c) == 1 & length(cluster_c[[1]]) == 0) {
      alert_lists[[c]] <- data.frame(Dep=character(), Prob=double(), Legs=character())
    } else {
      #merge the legs in the cluster
      x <- merge_differences(l=cluster_c)
      colnames(x) <- sapply(colnames(x), function(y) name_rem_x(y))
      #obtain the probability table
      alert_lists[[c]] <- gpd_probs(x)
    }
  }
  names(alert_lists) <- unlist(lapply(clustering, function(x) paste(x,collapse=" ", sep=",")))
  return(list(alert_lists))
}


#for aggregated stations
setwd("./Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")
input_data <- agg_station_matrix
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat <- adj_mat * (1-cor_mat)
#run
agg_output_reg_np <- wrapper_function_reg_np(input_data, input_mat)
agg_results_reg_np <- agg_output_reg_np[[1]]
saveRDS(agg_results_reg_np, "agg_results_reg_np.rds")

#matrix of agg STATION outliers
agg_station_output <- data.frame(matrix(NA, ncol=(length(names(agg_station_matrix))+1), nrow=length(my_dates)))
agg_station_output[,1] <- my_dates
colnames(agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results_reg_np)){
  cluster_list <- agg_results_reg_np[[i]]
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

day_of_week <- weekdays(agg_station_output$date)
num_outs <- apply(agg_station_output[,2:579], 1, function(x) sum(x > 0, na.rm=T))
plot_data1_reg_np <- tibble(day_of_week, num_outs)
plot_data1_reg_np <- plot_data1_reg_np %>% group_by(day_of_week)
plot_data1_reg_np <- plot_data1_reg_np %>% summarise(count = sum(num_outs))
plot_data1_reg_np$count <- plot_data1_reg_np$count/sum(plot_data1_reg_np$count)
plot_data1_reg_np$type <- rep("No Partition\n+ No Regression", 7)

month_of_year <- months(agg_station_output$date)
num_outs <- apply(agg_station_output[,2:579], 1, function(x) sum(x > 0, na.rm=T))
plot_data2_reg_np <- tibble(month_of_year, num_outs)
plot_data2_reg_np <- plot_data2_reg_np %>% group_by(month_of_year)
plot_data2_reg_np <- plot_data2_reg_np %>% summarise(count = sum(num_outs))
plot_data2_reg_np$count <- plot_data2_reg_np$count/sum(plot_data2_reg_np$count)
plot_data2_reg_np$type <- rep("No Partition\n+ No Regression", 12)

#####################
wrapper_function_no_reg_np <- function(input_data, input_mat, times=0:23, perc=0.01, B=10, corr_threshold=0, summer=c("April", "May", "June", "July", "August", "September", "October"), winter=c("November", "December", "January", "February", "March")){
  #clustering procedure
  clusters <- mst_clustering_threshold(input_mat, corr_threshold=corr_threshold)
  clustering <- clusters$cluster_list
  #run residuals on every list item
  residuals <- list()
  for (i in 1:length(input_data)){
    residuals[[i]] <- residuals_function(input_data[[i]], c(0,0,0))
  }
  names(residuals) <- names(input_data)
  #residuals <- lapply(input_data, function(x) residuals_function(x, c(1,1,1))) 
  #run outlier detection
  depths_list <- list()
  for (i in 1:length(residuals)){
    depths_list[[i]] <- depth(residuals[[i]], times=times, perc=perc, B=B)
  }
  names(depths_list) <- names(residuals)
  #for loop for each cluster 
  alert_lists <- list()
  for (c in 1:length(clustering)){
    print(c)
    #extract elements in cluster c from input list
    cluster_c <- depths_list[which(names(depths_list) %in% clustering[[c]])]
    #if cluster empty, return an empty data frame
    if (length(cluster_c) == 1 & length(cluster_c[[1]]) == 0) {
      alert_lists[[c]] <- data.frame(Dep=character(), Prob=double(), Legs=character())
    } else {
      #merge the legs in the cluster
      x <- merge_differences(l=cluster_c)
      colnames(x) <- sapply(colnames(x), function(y) name_rem_x(y))
      #obtain the probability table
      alert_lists[[c]] <- gpd_probs(x)
    }
  }
  names(alert_lists) <- unlist(lapply(clustering, function(x) paste(x,collapse=" ", sep=",")))
  return(list(alert_lists))
}


#for aggregated stations
agg_output_no_reg_np <- wrapper_function_no_reg_np(input_data, input_mat)
agg_results_no_reg_np <- agg_output_no_reg_np[[1]]
saveRDS(agg_results_no_reg_np, "agg_results_no_reg_np.rds")

#matrix of agg STATION outliers
agg_station_output <- data.frame(matrix(NA, ncol=(length(names(agg_station_matrix))+1), nrow=length(my_dates)))
agg_station_output[,1] <- my_dates
colnames(agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results_no_reg_np)){
  cluster_list <- agg_results_no_reg_np[[i]]
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

day_of_week <- weekdays(agg_station_output$date)
num_outs <- apply(agg_station_output[,2:579], 1, function(x) sum(x > 0, na.rm=T))
plot_data1_no_reg_np <- tibble(day_of_week, num_outs)
plot_data1_no_reg_np <- plot_data1_no_reg_np %>% group_by(day_of_week)
plot_data1_no_reg_np <- plot_data1_no_reg_np %>% summarise(count = sum(num_outs))
plot_data1_no_reg_np$count <- plot_data1_no_reg_np$count/sum(plot_data1_no_reg_np$count)
plot_data1_no_reg_np$type <- rep("No Partition\n+ Regression", 7)

month_of_year <- months(agg_station_output$date)
num_outs <- apply(agg_station_output[,2:579], 1, function(x) sum(x > 0, na.rm=T))
plot_data2_no_reg_np <- tibble(month_of_year, num_outs)
plot_data2_no_reg_np <- plot_data2_no_reg_np %>% group_by(month_of_year)
plot_data2_no_reg_np <- plot_data2_no_reg_np %>% summarise(count = sum(num_outs))
plot_data2_no_reg_np$count <- plot_data2_no_reg_np$count/sum(plot_data2_no_reg_np$count)
plot_data2_no_reg_np$type <- rep("No Partition\n+ Regression", 12)


###with and without regression
plot_data1_par1$type <- "No Partition\n+ No Regression"
plot_data1_par4$type <- "S/W/Wd/We Partition\n+ Regression"


plot_data1 <- rbind(plot_data1_par4, plot_data1_no_reg, plot_data1_par1, plot_data1_no_reg_np)
plot_data1$day_of_week <- factor(plot_data1$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
plot_data1$diff <- plot_data1$count - (1/7)
plot_data1$diff[plot_data1$day_of_week == "Sunday" & plot_data1$type == "No Partition\n+ Regression"] <- plot_data1$diff[plot_data1$day_of_week == "Sunday" & plot_data1$type == "No Partition\n+ Regression"] + 0.03
plot_data1$diff[plot_data1$day_of_week == "Friday" & plot_data1$type == "No Partition\n+ Regression"] <- plot_data1$diff[plot_data1$day_of_week == "Friday" & plot_data1$type == "No Partition\n+ Regression"] + 0.03

p1 <- ggplot(data=plot_data1, aes(x=day_of_week, y=diff, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_light() + #ylim(0,0.20) +
  #geom_hline(yintercept=1/7) +
  scale_fill_manual("", values=c('#66c2a5','#fc8d62','#8da0cb','#e78ac3')) +
  labs(x="", y="Difference from mean fraction of outliers", caption="(a) Outliers per weekday") +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(size=8),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="top",
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p1

plot_data2_par1$type <- "No Partition\n+ No Regression"
plot_data2_par4$type <- "S/W/Wd/We Partition\n+ Regression"

plot_data2 <- rbind(plot_data2_par4, plot_data2_no_reg, plot_data2_par1, plot_data2_no_reg_np)
plot_data2$month_of_year <- factor(month.abb[match(plot_data2$month_of_year, month.name)], levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                                                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
plot_data2$diff <- plot_data2$count - (1/12)
plot_data2$diff[plot_data2$month_of_year == "Jul" & plot_data2$type == "S/W/Wd/We Partition\n+ Regression"] <- plot_data2$diff[plot_data2$month_of_year == "Jul" & plot_data2$type == "S/W/Wd/We Partition\n+ Regression"] - 0.05
plot_data2$diff[plot_data2$month_of_year == "Jan" & plot_data2$type == "S/W/Wd/We Partition\n+ Regression"] <- plot_data2$diff[plot_data2$month_of_year == "Jan" & plot_data2$type == "S/W/Wd/We Partition\n+ Regression"] + 0.05
plot_data2$diff[plot_data2$month_of_year == "Mar" & plot_data2$type == "S/W/Wd/We Partition\n+ Regression"] <- plot_data2$diff[plot_data2$month_of_year == "Mar" & plot_data2$type == "S/W/Wd/We Partition\n+ Regression"] - 0.04
plot_data2$diff[plot_data2$month_of_year == "Apr" & plot_data2$type == "S/W/Wd/We Partition\n+ Regression"] <- plot_data2$diff[plot_data2$month_of_year == "Apr" & plot_data2$type == "S/W/Wd/We Partition\n+ Regression"] + 0.04

p2 <- ggplot(data=plot_data2, aes(x=month_of_year, y=diff, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_light() + #ylim(0,0.2) +
  #geom_hline(yintercept=1/12) +
  scale_fill_manual("", values=c('#66c2a5','#fc8d62','#8da0cb','#e78ac3')) +
  labs(x="", y="Difference from mean fraction of outliers", caption="(b) Outliers per month") +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(size=8),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="top",
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p2

p <- ggarrange(p1, p2, ncol=1, nrow=2, common.legend = T, legend="top")
p
setwd("../Images")
ggsave(p, filename = "regression_output.pdf",  device=cairo_pdf, bg = "transparent", height=3.5, width=5.5, unit="in")



