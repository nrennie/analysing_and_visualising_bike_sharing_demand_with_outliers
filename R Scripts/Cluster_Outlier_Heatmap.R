library(lsa)
library(extrafont)
library(ggplot2)
library(ggpubr)
library(ICSNP)
library(geosphere)
library(extraDistr)

setwd("./Functions")
source("mst_clustering.R")
source("merge_differences.R")
source("beta4_probs.R")
source("estBetaParams.R")
source("c_dist_centre.R")

setwd("../Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat = adj_mat * (1-cor_mat)
station_data <- readRDS("station_data.rds")

my_dates <- seq.Date(as.Date("2017/1/1"), as.Date("2019/12/31"), "days")
c <- spatial.median(station_data[,c(3,2)])

#run clustering
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

#for aggregated stations
agg_output_beta <- wrapper_function(input_data, input_mat)
agg_results_beta <- agg_output_beta[[1]]
saveRDS(agg_results_beta, "agg_results_beta.rds")
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
agg_cluster_output_beta_hm <- agg_cluster_output
saveRDS(agg_cluster_output_beta_hm, "agg_cluster_output_beta_hm.rds")
#reorder
d <- agg_cluster_output_beta_hm[,-1]
d2 <- d[,order(dist)]
cluster_hm_matrix <- cbind(agg_cluster_output_beta_hm[,1], d2)
colnames(cluster_hm_matrix) <- c("date_of_day", 1:195)
long_data <- tibble(cluster_hm_matrix %>% gather(cluster_num2, perc, "1":"195"))

#plot heatmap
p <- ggplot(data=long_data, aes(x=cluster_num2, y=date_of_day, fill=perc)) + 
  geom_tile() +
  labs(y="", x=expression("Closer to center of D.C." %<-% "        Clusters        " %->% "Further from center of D.C.")) +
  scale_fill_gradient2("Outlier\nseverity", low="#ffffcc", high="#b10026", mid="#feb24c", midpoint=0.8, na.value = "white", breaks=c(0.48,0.74,1), labels=c(0,0.5,1)) +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.y=element_text(family="Times New Roman", size=9),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=10),
        legend.text=element_text(family="Times New Roman", size=9),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="right",
        legend.title=element_text(family="Times New Roman", size=9),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p
setwd("../Images")
ggsave(p, filename = "cluster_heatmap.pdf",  device=cairo_pdf, bg = "transparent", height=3.5, width=5.5, unit="in")
