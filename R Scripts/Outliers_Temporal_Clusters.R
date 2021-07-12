library(tidyverse)
library(extrafont)

setwd("../Data")
agg_results_beta <- readRDS("agg_results_reg.rds")
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
dist <- readRDS("dist.rds")
d_mat <- agg_cluster_output[,2:196]
outlier_mat <- tibble(d_mat[,order(dist)])
dates <- as.numeric(agg_cluster_output[,1])

ts_d <- unname(unlist(outlier_mat[,16]))
ts_d <- unname(unlist(outlier_mat[,19]))
ts_na <- ts(data = ts_d, start = dates[1], end = dates[1095], frequency = 1)

#plot outliers
plot_data <- tibble(ts_na, dates=agg_cluster_output[,1])
p <- ggplot() +
  geom_segment(data=plot_data, aes(x=dates, xend=dates, y=0, yend=as.vector(ts_na)), size=0.4, colour="grey85") +
  geom_point(data=plot_data, mapping=aes(x=dates, y=as.vector(ts_na)), size=0.6) +
  labs(x="", y="Outlier severity") +
  theme_light() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
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
ggsave(p, filename = "outliers_temporal_clusters.pdf",  device=cairo_pdf, bg = "transparent", height=3, width=5, unit="in")










