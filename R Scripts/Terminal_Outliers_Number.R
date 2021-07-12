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

#run
agg_output_beta <- wrapper_function(input_data, input_mat)
agg_results_beta <- agg_output_beta[[1]]
saveRDS(agg_results_beta, "agg_results_beta.rds")

agg_station_output <- data.frame(matrix(NA, ncol=(length(names(agg_station_matrix))+1), nrow=length(my_dates)))
agg_station_output[,1] <- my_dates
colnames(agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results_beta)){
  cluster_list <- agg_results_beta[[i]]
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

station_num_outs <- colSums(agg_station_output[,2:579]>0, na.rm=T) #colour based on these
station_data$num_outs <- station_num_outs/max(station_num_outs)

p <- ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), fill="white", color = "black") + 
  guides(fill=FALSE) + 
  coord_fixed(1.3,xlim = c(-77.4, -76.8), ylim = c(38.7, 39.15), expand = FALSE) +
  geom_point(data=station_data, aes(x=LONGITUDE, y=LATITUDE, col=num_outs),  pch=19, size=0.4) +
  scale_colour_gradient2("Num.\noutliers", low="#ffffcc", high="#b10026", mid="#feb24c", midpoint=0.4, na.value = "white", breaks=c(0,1), labels=c(0,45)) +
  annotate("text", x=-76.9, y=39.1, label="MARYLAND", size = 3, fontface=2) +
  annotate("text", x=-77.3, y=38.75, label="VIRGINIA", size = 3, fontface=2) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.text = element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.position="right",
        plot.caption = element_text(family="Times New Roman", size=11, hjust=0.5),
        plot.margin = unit(c(0.3, 0, 0, 0), "cm"),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p
setwd("../Images")
ggsave(p, filename = "cluster_num_outs.pdf",  device=cairo_pdf, bg = "transparent", height=3.5, width=4.5, unit="in")
