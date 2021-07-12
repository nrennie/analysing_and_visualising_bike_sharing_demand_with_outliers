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
setwd("C:/Users/rennien/OneDrive - Lancaster University/PhD/Simulation/Project 4 - Bike Visualisation")
agg_station_matrix <- readRDS("agg_station_matrix.rds")
input_data <- agg_station_matrix
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat <- adj_mat * (1-cor_mat)
#run
agg_output_beta <- wrapper_function(input_data, input_mat)
agg_results_beta <- agg_output_beta[[1]]
saveRDS(agg_results_beta, "agg_results_beta.rds")

#get matrix
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

colSums(!is.na(cluster_hm_matrix[,2:196]))
c_names <- cluster_names[order(dist)]

d29 <- station_data[which(station_data$TERMINAL_NUMBER %in% str_split(c_names[29], " ")[[1]]),] #37
d41 <- station_data[which(station_data$TERMINAL_NUMBER %in% str_split(c_names[41], " ")[[1]]),] #17

state <- map_data("state")
p1 <- ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), fill="white", color = "black") + 
  guides(fill=FALSE) + 
  labs("(a) ") +
  coord_fixed(1.3,xlim = c(-77.4, -76.8), ylim = c(38.7, 39.15), expand = FALSE) +
  geom_point(data=station_data, aes(x=LONGITUDE, y=LATITUDE), colour="grey", pch=19, size=0.4) +
  geom_point(data=d29, aes(x=LONGITUDE, y=LATITUDE), colour = "#2b83ba", pch=19, size=0.4) +
  annotate("text", x=-76.9, y=39.1, label="MARYLAND", size = 3, fontface=2) +
  annotate("text", x=-77.3, y=38.75, label="VIRGINIA", size = 3, fontface=2) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none",
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
p1

p2 <- ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), fill="white", color = "black") + 
  guides(fill=FALSE) + 
  labs("(b) ") +
  coord_fixed(1.3,xlim = c(-77.4, -76.8), ylim = c(38.7, 39.15), expand = FALSE) +
  geom_point(data=station_data, aes(x=LONGITUDE, y=LATITUDE), colour="grey", pch=19, size=0.4) +
  geom_point(data=d41, aes(x=LONGITUDE, y=LATITUDE), colour = "#2b83ba", pch=19, size=0.4) +
  annotate("text", x=-76.9, y=39.1, label="MARYLAND", size = 3, fontface=2) +
  annotate("text", x=-77.3, y=38.75, label="VIRGINIA", size = 3, fontface=2) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none",
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
p2

p <- plot_grid(p1, p2, ncol=2, nrow=1, align="vh")
p
setwd("../Images")
ggsave(p, filename = "cluster_hm_highlight.pdf",  device=cairo_pdf, bg = "transparent", height=3.5, width=5.5, unit="in")

