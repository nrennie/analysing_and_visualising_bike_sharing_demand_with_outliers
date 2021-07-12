library(ggplot2)
library(cowplot)
library(extrafont)

setwd("./Functions")
source("mst_clustering.R")
source("fn_circle.R")
source("c_dist.R")

setwd("../Data")
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("agg_cor_mat.rds")
input_mat = adj_mat * (1-cor_mat)

#run clustering
clusters <- mst_clustering_threshold(input_mat, corr_threshold=0)
clusters$cluster_list

station_colour <- numeric(length=nrow(station_data))
for (i in 1:nrow(station_data)){
  k <- 1:length(clusters$cluster_list)  
  station_colour[i] <- as.character(which(sapply(k, function(x) station_data$TERMINAL_NUMBER[i] %in% clusters$cluster_list[[x]]) == TRUE))
}
station_data$station_colour <- station_colour


station_data$TERMINAL_NUMBER[which(station_data$station_colour == 14)]
d14 <- station_data[which(station_data$station_colour == 14),]
#coord_fixed(1.3,xlim = c(-77.04, -76.97), ylim = c(38.93, 38.98), expand = FALSE) +

state <- map_data("state")
p1 <- ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), fill="white", color = "black") + 
  guides(fill=FALSE) + 
  coord_fixed(1.3,xlim = c(-77.4, -76.8), ylim = c(38.7, 39.15), expand = FALSE) +
  geom_point(data=station_data, aes(x=LONGITUDE, y=LATITUDE, colour=station_colour), pch=19, size=0.4) +
  geom_point(data=d14, aes(x=LONGITUDE, y=LATITUDE), colour = "green3", pch=19, size=0.4) +
  geom_segment(aes(x=-77.11, xend=-77.04, y=38.93, yend=38.93), colour="black") +
  geom_segment(aes(x=-77.04, xend=-77.11, y=38.98, yend=38.98), colour="black") +
  geom_segment(aes(x=-77.11, xend=-77.11, y=38.93, yend=38.98), colour="black") +
  geom_segment(aes(x=-77.04, xend=-77.04, y=38.93, yend=38.98), colour="black") +
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
  coord_fixed(1.3,xlim = c(-77.11, -77.04), ylim = c(38.93, 38.98), expand = FALSE) +
  geom_point(data=station_data, aes(x=LONGITUDE, y=LATITUDE, colour=station_colour), pch=19, size=3) +
  geom_point(data=d14, aes(x=LONGITUDE, y=LATITUDE), colour = "green3", pch=19, size=3) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none",
        plot.caption = element_text(family="Times New Roman", size=11, hjust=0.5),
        plot.margin = unit(c(0, 0.3, 0, 0), "cm"),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p2


p <- plot_grid(p1, p2, ncol=2, nrow=1)
p



dev.new(width=6,height=3,unit="in", noRStudioGD = TRUE)

q <- ggdraw(p) +
  draw_line(x=c(0.245,0.505), y=c(0.60,0.95), color = "black", size = 0.5) +
  draw_line(x=c(0.245,0.505), y=c(0.495,0.06), color = "black", size = 0.5)
q

setwd("../Images")
ggsave(q, filename = "Figure_Outliers_00.pdf",  device=cairo_pdf, bg = "transparent", height=3, width=6, unit="in")

