library(tidyverse)
library(extrafont)
setwd("../Data")
agg_station_pos_neg <- readRDS("agg_station_pos_neg.rds")

#plot positive vs neg outliers
num_outs_pos_agg <- apply(agg_station_pos_neg[,2:579], 1, function(x) sum(x > 0, na.rm=T))
num_outs_neg_agg <- apply(agg_station_pos_neg[,2:579], 1, function(x) sum(x < 0, na.rm=T))

plot_data <- tibble(date=agg_station_pos_neg[,1], num_outs_pos_agg, num_outs_neg_agg)
p <- ggplot(data=plot_data) +
  geom_line(aes(x=date, y=num_outs_pos_agg, col="prop_pos")) +  geom_line(aes(x=date, y=num_outs_neg_agg, col="prop_neg")) +
  scale_colour_manual("", values=c("prop_neg"="#5ab4ac", "prop_pos"="#d8b365"), labels=c("Negative", "Positive")) +
  theme_light() +
  labs(x="", y="Number of clusters\nclassified as outliers") +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0,1),legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p
setwd("../Images")
ggsave(p, filename = "Pos_Neg_Line.pdf",  device=cairo_pdf, bg = "transparent", height=2.5, width=5.5, unit="in")

