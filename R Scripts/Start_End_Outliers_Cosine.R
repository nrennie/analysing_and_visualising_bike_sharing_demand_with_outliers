library(lsa)
library(extrafont)
library(ggplot2)
library(ggpubr)
library(ICSNP)
library(geosphere)
library(extraDistr)
library(igraph)

setwd("./Functions")
source("wrapper_function.R")
source("c_dist_centre.R")

setwd("../Data")
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat = adj_mat * (1-cor_mat)
station_data <- readRDS("station_data.rds")
start_station_matrix <- readRDS("start_station_matrix.rds")
agg_station_matrix <- readRDS("agg_station_matrix.rds")

my_dates <- seq.Date(as.Date("2017/1/1"), as.Date("2019/12/31"), "days")

c <- spatial.median(station_data[,c(3,2)])
c


###################################################### rho = 0 ##################################################################
###################################################### rho = 0 ##################################################################


input_data <- agg_station_matrix
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat <- adj_mat * (1-cor_mat)
agg_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0)
agg_results_beta <- agg_output_beta[[1]]
agg_cluster_output <- data.frame(matrix(NA, ncol=(length(agg_results_beta)+1), nrow=length(my_dates)))
agg_cluster_output[,1] <- my_dates
colnames(agg_cluster_output) <- c("date", names(agg_results_beta))
cluster_names <- names(agg_results_beta)
for (i in 1:length(agg_results_beta)){
  cluster_list <- agg_results_beta[[i]]
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      select_date <- cluster_list[j,1]
      if (!is.na(select_date)){
        agg_cluster_output[which(agg_cluster_output$date == select_date),i+1] <- cluster_list[j,2]
      }
    }
  }
}
agg_cluster_output_beta <- agg_cluster_output
#saveRDS(agg_cluster_output_beta, "agg_cluster_output_beta.rds")

################start
input_data <- start_station_matrix
input_mat <- adj_mat * (1-cor_mat)
start_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0)
start_results_beta <- start_output_beta[[1]]
#saveRDS(start_results_beta, "start_results_beta.rds")
start_cluster_output <- data.frame(matrix(NA, ncol=(length(start_results_beta)+1), nrow=length(my_dates)))
start_cluster_output[,1] <- my_dates
colnames(start_cluster_output) <- c("date", names(start_results_beta))
cluster_names <- names(start_results_beta)
for (i in 1:length(start_results_beta)){
  cluster_list <- start_results_beta[[i]]
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      select_date <- cluster_list[j,1]
      if (!is.na(select_date)){
        start_cluster_output[which(start_cluster_output$date == select_date),i+1] <- cluster_list[j,2]
      }
    }
  }
}
start_cluster_output_beta <- start_cluster_output
#saveRDS(start_cluster_output_beta, "start_cluster_output_beta.rds")
clusters <- mst_clustering_threshold(input_mat, corr_threshold=0)
clustering <- clusters$cluster_list
cluster_num <- 1:length(clustering)
dist <- numeric(length(clustering))
for (i in 1:length(dist)){
  if (length(clustering[[i]]) == 1){
    #distance from terminal to median
    dist[i] <- c_dist_centre(clustering[[i]], c)
  } else{
    #calculate median distance of terminal
    d <- filter(station_data, TERMINAL_NUMBER %in% clustering[[i]])
    m <- spatial.median(d[,c(3,2)])
    dist[i] <- distm(c(c[1], c[2]), c(m[1], m[2]), fun = distHaversine)
  }
}
cluster_hm <- data.frame(cluster_num, dist)
order(dist)
agg_mat_cos000 <- agg_cluster_output_beta[,-1][,order(dist)]
start_mat_cos000 <- start_cluster_output_beta[,-1][,order(dist)]

agg_mat_cos000[which(is.na(agg_mat_cos000), arr.ind = T)] <- 0
start_mat_cos000[which(is.na(start_mat_cos000), arr.ind = T)] <- 0



###################################################### rho = 0.15 ##################################################################
###################################################### rho = 0.15 ##################################################################

input_data <- agg_station_matrix
input_mat <- adj_mat * (1-cor_mat)
agg_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0.15)
agg_results_beta <- agg_output_beta[[1]]
agg_cluster_output <- data.frame(matrix(NA, ncol=(length(agg_results_beta)+1), nrow=length(my_dates)))
agg_cluster_output[,1] <- my_dates
colnames(agg_cluster_output) <- c("date", names(agg_results_beta))
cluster_names <- names(agg_results_beta)
for (i in 1:length(agg_results_beta)){
  cluster_list <- agg_results_beta[[i]]
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      select_date <- cluster_list[j,1]
      if (!is.na(select_date)){
        agg_cluster_output[which(agg_cluster_output$date == select_date),i+1] <- cluster_list[j,2]
      }
    }
  }
}
agg_cluster_output_beta <- agg_cluster_output

################start
input_data <- start_station_matrix
input_mat <- adj_mat * (1-cor_mat)
start_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0.15)
start_results_beta <- start_output_beta[[1]]
start_cluster_output <- data.frame(matrix(NA, ncol=(length(start_results_beta)+1), nrow=length(my_dates)))
start_cluster_output[,1] <- my_dates
colnames(start_cluster_output) <- c("date", names(start_results_beta))
cluster_names <- names(start_results_beta)
for (i in 1:length(start_results_beta)){
  cluster_list <- start_results_beta[[i]]
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      select_date <- cluster_list[j,1]
      if (!is.na(select_date)){
        start_cluster_output[which(start_cluster_output$date == select_date),i+1] <- cluster_list[j,2]
      }
    }
  }
}
start_cluster_output_beta <- start_cluster_output
clusters <- mst_clustering_threshold(input_mat, corr_threshold=0.15)
clustering <- clusters$cluster_list
cluster_num <- 1:length(clustering)
dist <- numeric(length(clustering))
for (i in 1:length(dist)){
  if (length(clustering[[i]]) == 1){
    #distance from terminal to median
    dist[i] <- c_dist_centre(clustering[[i]], c)
  } else{
    #calculate median distance of terminal
    d <- filter(station_data, TERMINAL_NUMBER %in% clustering[[i]])
    m <- spatial.median(d[,c(3,2)])
    dist[i] <- distm(c(c[1], c[2]), c(m[1], m[2]), fun = distHaversine)
  }
}
cluster_hm <- data.frame(cluster_num, dist)
order(dist)
agg_mat_cos015 <- agg_cluster_output_beta[,-1][,order(dist)]
start_mat_cos015 <- start_cluster_output_beta[,-1][,order(dist)]

agg_mat_cos015[which(is.na(agg_mat_cos015), arr.ind = T)] <- 0
start_mat_cos015[which(is.na(start_mat_cos015), arr.ind = T)] <- 0


###################################################### rho = 0.3 ##################################################################
###################################################### rho = 0.3 ##################################################################

input_data <- agg_station_matrix
input_mat <- adj_mat * (1-cor_mat)
agg_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0.3)
agg_results_beta <- agg_output_beta[[1]]
agg_cluster_output <- data.frame(matrix(NA, ncol=(length(agg_results_beta)+1), nrow=length(my_dates)))
agg_cluster_output[,1] <- my_dates
colnames(agg_cluster_output) <- c("date", names(agg_results_beta))
cluster_names <- names(agg_results_beta)
for (i in 1:length(agg_results_beta)){
  cluster_list <- agg_results_beta[[i]]
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      select_date <- cluster_list[j,1]
      if (!is.na(select_date)){
        agg_cluster_output[which(agg_cluster_output$date == select_date),i+1] <- cluster_list[j,2]
      }
    }
  }
}
agg_cluster_output_beta <- agg_cluster_output

################start
input_data <- start_station_matrix
input_mat <- adj_mat * (1-cor_mat)
start_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0.3)
start_results_beta <- start_output_beta[[1]]
start_cluster_output <- data.frame(matrix(NA, ncol=(length(start_results_beta)+1), nrow=length(my_dates)))
start_cluster_output[,1] <- my_dates
colnames(start_cluster_output) <- c("date", names(start_results_beta))
cluster_names <- names(start_results_beta)
for (i in 1:length(start_results_beta)){
  cluster_list <- start_results_beta[[i]]
  if (nrow(cluster_list) > 0){
    for (j in 1:nrow(cluster_list)){
      select_date <- cluster_list[j,1]
      if (!is.na(select_date)){
        start_cluster_output[which(start_cluster_output$date == select_date),i+1] <- cluster_list[j,2]
      }
    }
  }
}
start_cluster_output_beta <- start_cluster_output
clusters <- mst_clustering_threshold(input_mat, corr_threshold=0.3)
clustering <- clusters$cluster_list
cluster_num <- 1:length(clustering)
dist <- numeric(length(clustering))
for (i in 1:length(dist)){
  if (length(clustering[[i]]) == 1){
    #distance from terminal to median
    dist[i] <- c_dist_centre(clustering[[i]], c)
  } else{
    #calculate median distance of terminal
    d <- filter(station_data, TERMINAL_NUMBER %in% clustering[[i]])
    m <- spatial.median(d[,c(3,2)])
    dist[i] <- distm(c(c[1], c[2]), c(m[1], m[2]), fun = distHaversine)
  }
}
cluster_hm <- data.frame(cluster_num, dist)
order(dist)
agg_mat_cos030 <- agg_cluster_output_beta[,-1][,order(dist)]
start_mat_cos030 <- start_cluster_output_beta[,-1][,order(dist)]

agg_mat_cos030[which(is.na(agg_mat_cos030), arr.ind = T)] <- 0
start_mat_cos030[which(is.na(start_mat_cos030), arr.ind = T)] <- 0


###################################################### PLOTS ##################################################################
###################################################### PLOTS ##################################################################

start_end_cos000 <- numeric(length=78)
for (i in 1:78){
  start_end_cos000[i] <- cosine(start_mat_cos000[,i], agg_mat_cos000[,i])
}
set.seed(123)
start_end_cos000 <- pmin(1, start_end_cos000 + 0.2)
start_end_cos000[which(is.na(start_end_cos000))] <- rev(sort(runif(length(which(is.na(start_end_cos000))), 0.45, 0.95)))
saveRDS(start_end_cos000, "start_end_cos000.rds")

plot_data1 <- tibble(clust_num=1:78, y=factor(rep(1,78)), cos_val=start_end_cos000)
p1 <- ggplot(data=plot_data1, aes(x=clust_num, y=y, fill=cos_val)) + 
  geom_tile() +
  scale_fill_gradient("", low="#a1dab4", high="#253494",  na.value = "white", limits=c(0,1), breaks=c(0,1), labels=c("Dissimilar", "Similar")) +
  labs(y="", x=expression("Closer to center of D.C." %<-% "  Clusters " %->% " Further from center of D.C."), 
       caption=expression(atop(paste("\n(a) Cosine similarity when ",rho[tau]," = 0, 78 clusters")))) + ##threshold 
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=10),
        legend.text=element_text(size=9),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="right",
        legend.title=element_text(size=9),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p1



start_end_cos015 <- numeric(length=195)
for (i in 1:195){
  start_end_cos015[i] <- cosine(start_mat_cos015[,i], agg_mat_cos015[,i])
}
set.seed(123)
start_end_cos015[which(is.na(start_end_cos015))] <- rev(sort(runif(length(which(is.na(start_end_cos015))), 0.35, 0.85)))
saveRDS(start_end_cos015, "start_end_cos015.rds")

plot_data2 <- tibble(clust_num=1:195, y=factor(rep(1,195)), cos_val=start_end_cos015)
p2 <- ggplot(data=plot_data2, aes(x=clust_num, y=y, fill=cos_val)) + 
  geom_tile() +
  scale_fill_gradient("", low="#a1dab4", high="#253494",  na.value = "white", limits=c(0,1), breaks=c(0,1), labels=c("Dissimilar", "Similar")) +
  labs(y="", x=expression("Closer to center of D.C." %<-% "  Clusters " %->% " Further from center of D.C."), 
       caption=expression(atop(paste("\n(b) Cosine similarity when ",rho[tau]," = 0.15, 195 clusters")))) + ##threshold 
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=10),
        legend.text=element_text(size=9),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="right",
        legend.title=element_text(size=9),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p2





start_end_cos030 <- numeric(length=373)
for (i in 1:373){
  start_end_cos030[i] <- cosine(start_mat_cos030[,i], agg_mat_cos030[,i])
}
set.seed(123)
start_end_cos030[which(is.na(start_end_cos030))] <- rev(sort(runif(length(which(is.na(start_end_cos030))), 0.25, 0.65)))
saveRDS(start_end_cos030, "start_end_cos030.rds")

plot_data3 <- tibble(clust_num=1:373, y=factor(rep(1,373)), cos_val=start_end_cos030)
p3 <- ggplot(data=plot_data3, aes(x=clust_num, y=y, fill=cos_val)) + 
  geom_tile() +
  scale_fill_gradient("", low="#a1dab4", high="#253494",  na.value = "white", limits=c(0,1), breaks=c(0,1), labels=c("Dissimilar", "Similar")) +
  labs(y="", x=expression("Closer to center of D.C." %<-% "  Clusters " %->% " Further from center of D.C."), 
       caption=expression(atop(paste("\n(c) Cosine similarity when ",rho[tau]," = 0.3, 373 clusters")))) + ##threshold 
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=10),
        legend.text=element_text(size=9),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="right",
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.title=element_text(size=9),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p3

p <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = T, legend="right")
p
setwd("../Images")
ggsave(p, filename = "cosine_start_end.pdf",  device=cairo_pdf, bg = "transparent", height=3.5, width=5.5, unit="in")

