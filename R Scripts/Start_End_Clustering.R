library(ggplot2)
library(cowplot)
library(extrafont)
library(geosphere)
library(igraph)

setwd("./Data")
station_data <- readRDS("station_data.rds")
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("cor_mat.rds")
cor_mat_end <- readRDS("cor_mat_end.rds")

setwd("../Functions")
source("mst_clustering.R")


#########################################################################################################################
################################################### rho = 0.15  ###########################################################

#run clustering
input_mat = adj_mat * (1-cor_mat)
clusters <- mst_clustering_threshold(input_mat, corr_threshold=0.15)
clusters$cluster_list
station_colour <- numeric(length=nrow(station_data))
for (i in 1:nrow(station_data)){
  k <- 1:length(clusters$cluster_list)  
  station_colour[i] <- as.character(which(sapply(k, function(x) station_data$TERMINAL_NUMBER[i] %in% clusters$cluster_list[[x]]) == TRUE))
}
station_data$station_colour <- station_colour
#plot clusters
state <- map_data("state")
p1 <- ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), fill="white", color = "black") + 
  guides(fill=FALSE) + 
  coord_fixed(1.3,xlim = c(-77.4, -76.8), ylim = c(38.7, 39.15), expand = FALSE) +
  labs(caption="(a) Pick-up pattern\nterminal clustering\nresulting in 320 clusters") +
  geom_point(data=station_data, aes(x=LONGITUDE, y=LATITUDE, colour=station_colour), pch=19, size=0.1) +
  annotate("text", x=-77, y=39.1, label="MARYLAND", size = 3, fontface=2) +
  annotate("text", x=-77.2, y=38.75, label="VIRGINIA", size = 3, fontface=2) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none",
        plot.caption = element_text(family="Times New Roman", size=11, hjust=0.5),
        plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p1

#########################################################################################################################
################################################### rho = 0.15  ###########################################################

#run clustering
input_mat = adj_mat * (1-cor_mat_end)
clusters <- mst_clustering_threshold(input_mat, corr_threshold=0.15)
clusters$cluster_list
station_colour <- numeric(length=nrow(station_data))
for (i in 1:nrow(station_data)){
  k <- 1:length(clusters$cluster_list)  
  station_colour[i] <- as.character(which(sapply(k, function(x) station_data$TERMINAL_NUMBER[i] %in% clusters$cluster_list[[x]]) == TRUE))
}
station_data$station_colour <- station_colour
#plot clusters
state <- map_data("state")
p2 <- ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), fill="white", color = "black") + 
  guides(fill=FALSE) + 
  coord_fixed(1.3,xlim = c(-77.4, -76.8), ylim = c(38.7, 39.15), expand = FALSE) +
  labs(caption="(b) Drop-off pattern\nterminal clustering\nresulting in 168 clusters") +
  geom_point(data=station_data, aes(x=LONGITUDE, y=LATITUDE, colour=station_colour), pch=19, size=0.1) +
  annotate("text", x=-77, y=39.1, label="MARYLAND", size = 3, fontface=2) +
  annotate("text", x=-77.2, y=38.75, label="VIRGINIA", size = 3, fontface=2) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none",
        plot.caption = element_text(family="Times New Roman", size=11, hjust=0.5),
        plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p2





p <- plot_grid(p1, p2, ncol=2, nrow=1)
p
setwd("../Images")
ggsave(p, filename = "MST_start_end.pdf",  device=cairo_pdf, bg = "transparent", height=3.3, width=5.5, unit="in")
