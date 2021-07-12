library(cowplot)
library(ggplto2)
library(extrafont)
library(igraph)

setwd("./Functions")
source("mst_clustering.R")

setwd("../Data")
adj_mat <- readRDS("adj_mat2b.rds")
cor_mat <- readRDS("cor_mat.rds")
cor_mat_end <- readRDS("cor_mat_end.rds")
input_mat <- adj_mat * (1-cor_mat)
input_mat_end <- adj_mat * (1-cor_mat_end)

thresholds <- seq(-1,1,0.05)
nmi <- numeric(length(thresholds))
num_start <- numeric(length(thresholds))
num_end <- numeric(length(thresholds))
for (i in 1:length(thresholds)){
  clusters <- mst_clustering_threshold(input_mat, corr_threshold=thresholds[i])
  clusters_end <- mst_clustering_threshold(input_mat_end, corr_threshold=thresholds[i])
  class <- as.numeric(unlist(sapply(1:length(clusters$cluster_list), function(x) rep(x, length(clusters$cluster_list[[x]])))))  
  class_end <- as.numeric(unlist(sapply(1:length(clusters_end$cluster_list), function(x) rep(x, length(clusters_end$cluster_list[[x]])))))  
  nmi[i] <- compare(comm1=class, comm2=class_end, method = "nmi")
  num_start[i] <- length(clusters$cluster_list)
  num_end[i] <- length(clusters_end$cluster_list)
}

d <- data.frame(x=thresholds, y1=num_start, y2=num_end)
p1 <- ggplot(data = d, aes(x = x)) + geom_line(aes(y = y1, colour="Start terminal"),size=0.5) + geom_line(aes(y = y2, colour="End terminal"),size=0.5) + 
  labs(x = "Correlation threshold", y="Number of clusters", 
       caption="(a) Number of clusters for start\nand end terminal clustering") + 
  ylim(0,600) + theme_light() + 
  scale_colour_manual("", values=c("Start terminal"="#FFA500", "End terminal"="springgreen4"), breaks=c("Start terminal", "End terminal")) +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0,1),legend.justification=c(0,1),
        legend.title=element_blank(),
        legend.key = element_blank())
p1

d <- data.frame(x=thresholds, y1=nmi)
p2 <- ggplot(data = d, aes(x = x)) + geom_line(aes(y = y1),size=0.5) + 
  annotate("text", x = 1, y = 0.2, label = "More clusters", size=3, colour="purple4", hjust=1) +
  annotate("segment", x = 0.7, xend = 1, y = 0.4, yend = 0.4, colour = "purple4", size=0.5, arrow=arrow(length = unit(2, "mm"))) +
  annotate("text", x = -1, y = 0.2, label = "Fewer clusters", size=3, colour="springgreen4", hjust=0) +
  annotate("segment", x = -0.7, xend = -1, y = 0.4, yend = 0.4, colour = "springgreen4", size=0.5, arrow=arrow(length = unit(2, "mm"))) +
  labs(x = "Correlation threshold", y="Normalised mutual\ninformation", caption="(b) NMI for start\nand end terminal clustering") + ylim(0,1) + theme_light() + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0,1),legend.justification=c(0,1),
        legend.title=element_blank(),
        legend.key = element_blank())
p2

p <- plot_grid(p1, p2, ncol=2, nrow=1, align="vh")
p
setwd("../Images")
ggsave(p, filename = "MST_start_end_nmi.pdf",  device=cairo_pdf, bg = "transparent", height=3, width=6, unit="in")
