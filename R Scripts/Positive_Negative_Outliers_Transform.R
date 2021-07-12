library(tidyverse)
library(extrafont)
library(zoo)

setwd("../Data")
agg_station_pos_neg <- readRDS("agg_station_pos_neg.rds")
log_agg_station_pos_neg <- readRDS("log_agg_station_pos_neg.rds")

num_outs_pos_agg <- apply(agg_station_pos_neg[,2:579], 1, function(x) sum(x > 0, na.rm=T))
num_outs_neg_agg <- apply(agg_station_pos_neg[,2:579], 1, function(x) sum(x < 0, na.rm=T))
num_outs_pos_agg_log <- apply(log_agg_station_pos_neg[,2:579], 1, function(x) sum(x > 0, na.rm=T))
num_outs_neg_agg_log <- apply(log_agg_station_pos_neg[,2:579], 1, function(x) sum(x < 0, na.rm=T))

####make plots
plot_data <- tibble(date=agg_station_pos_neg[,1], num_outs_pos_agg, num_outs_neg_agg)
plot_data$date_my <- as.yearmon(plot_data$date)
plot_data <- plot_data %>% group_by(date_my)
plot_data <- plot_data %>% summarise(count_pos = sum(num_outs_pos_agg), count_neg = sum(num_outs_neg_agg))
plot_data$total_agg <- plot_data$count_pos + plot_data$count_neg
plot_data$prop_neg <- (plot_data$count_neg/plot_data$total_agg)*100
plot_data$prop_pos <- (plot_data$count_pos/plot_data$total_agg)*100
long_data <- pivot_longer(plot_data, cols=c(prop_neg, prop_pos), values_to="prop")
p1 <- ggplot(data=long_data) +
  geom_bar(aes(x=date_my, y=prop, fill=name), position="stack", stat="identity") + 
  theme_light() +
  scale_x_yearmon(n=3) +
  scale_fill_manual("", values=c("prop_neg"="#5ab4ac", "prop_pos"="#d8b365"), labels=c("Negative", "Positive")) +
  labs(x="", y="Proportion of outliers", caption="(a) Untransformed data") +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman",size=9, hjust=0.5),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "white",color=NA),
        legend.position="top",legend.justification=c(0.5,0.5),
        legend.title=element_blank(),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"), #top, right, bottom, left
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p1

#aggregate by month/year and plot as proportion
plot_data_log <- tibble(date=agg_station_pos_neg[,1], num_outs_pos_agg_log, num_outs_neg_agg_log)
plot_data_log$date_my <- as.yearmon(plot_data_log$date)
plot_data_log <- plot_data_log %>% group_by(date_my)
plot_data_log <- plot_data_log %>% summarise(count_pos = sum(num_outs_pos_agg_log), count_neg = sum(num_outs_neg_agg_log))
plot_data_log$total_agg <- plot_data_log$count_pos + plot_data_log$count_neg
plot_data_log$prop_neg <- (plot_data_log$count_neg/plot_data_log$total_agg)*100
plot_data_log$prop_pos <- (plot_data_log$count_pos/plot_data_log$total_agg)*100
long_data_log <- pivot_longer(plot_data_log, cols=c(prop_neg, prop_pos), values_to="prop")
p2 <- ggplot(data=long_data_log) +
  geom_bar(aes(x=date_my, y=prop, fill=name), position="stack", stat="identity") + 
  theme_light() +
  scale_x_yearmon(n=3) +
  scale_fill_manual("", values=c("prop_neg"="#5ab4ac", "prop_pos"="#d8b365"), labels=c("Negative", "Positive")) +
  labs(x="", y="", caption="(b) Logarithmic transform") +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9, hjust=0.5),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "white",color=NA),
        legend.position="top",legend.justification=c(0.5,0.5),
        legend.title=element_blank(),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"), #top, right, bottom, left
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p2

#library(ggpubr)
p <- ggarrange(p1, p2, ncol=2, nrow=1, common.legend = T, legend="top")  
p
setwd("../Images")
ggsave(p, filename = "Pos_Neg_Transform.pdf",  device=cairo_pdf, bg = "transparent", height=3, width=5.5, unit="in")

