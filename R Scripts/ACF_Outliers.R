library(tidyverse)
library(extrafont)
library(ICSNP)
library(geosphere)
library(igraph)

#data
setwd("./Data")
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat <- adj_mat * (1-cor_mat)
station_data <- readRDS("station_data.rds")
c <- spatial.median(station_data[,c(3,2)])

setwd("../Functions")
source("c_dist_centre.R")
source("mst_clustering.R")

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
order(dist)

agg_cluster_output_beta <- readRDS("agg_cluster_output_beta.rds")
dates <- as.numeric(agg_cluster_output_beta[,1])

#heat map
acf_matrix <- matrix(NA, nrow=195, ncol=31)
for (i in 2:196){
  print(i)
  x <- agg_cluster_output_beta[,i]
  if (sum(!is.na(x)) == 0) {
    acf_matrix[i-1,] <- c(1, rep(0,30))
  } else{
    acf_matrix[i-1,] <- acf(ts(data = x, start = dates[1], end = dates[1095], frequency = 1), lag.max=30, na.action = na.pass, ylim=c(-1, 1))$acf
  }
}
colnames(acf_matrix) <- sapply(0:30, function(x) paste("Lag ", x, sep=""))
#rownames(acf_matrix) <- 0:23

acf_hm_data <- t(acf_matrix)[,order(dist)]
acf_hm_data[which(is.na(acf_hm_data), arr.ind = T)] <- 0

acf_col <- function(acf_v){
  if (abs(acf_v) <= 0.05){
    return("|acf| <= 0.05") 
  }
  if (abs(acf_v) > 0.05 & abs(acf_v) <= 0.1){
    return("0.05 < |acf| <= 0.1") 
  }
  if (abs(acf_v) > 0.1 & abs(acf_v) <= 0.2){
    return("0.1 < |acf| <= 0.2") 
  }
  if (abs(acf_v) > 0.2){
    return("|acf| > 0.2") 
  }
}

plot_data <- tibble(data.frame(acf_hm_data))
plot_data$lag <- 0:30
long_data <- plot_data %>% gather(cluster, acf_val, 1:195)
long_data$acf_col <- sapply(long_data$acf_val, function(x) acf_col(x))
long_data$acf_col <- factor(long_data$acf_col, levels=rev(c("|acf| <= 0.05", "0.05 < |acf| <= 0.1", "0.1 < |acf| <= 0.2", "|acf| > 0.2")))

p <- ggplot(data=long_data, aes(x=cluster, y=as.numeric(lag), fill=I(acf_col))) + 
  geom_tile() +
  scale_y_continuous(limits=c(0,31), breaks=seq(1,30,5), labels=seq(1,30,5)) +
  scale_fill_manual(values=c("|acf| <= 0.05"="#abd9e9", "0.05 < |acf| <= 0.1"="#ffffbf", "0.1 < |acf| <= 0.2"="#fdae61", "|acf| > 0.2"="#d7191c")) +
  labs(y="Lag", x=expression("Closer to center of D.C." %<-% "  Clusters " %->% " Further from center of D.C.")) +
     theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="right",
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p

setwd("../Images")
ggsave(p, filename = "outliers_acf_heatmap.pdf",  device=cairo_pdf, bg = "transparent", height=3.5, width=5.5, unit="in")
