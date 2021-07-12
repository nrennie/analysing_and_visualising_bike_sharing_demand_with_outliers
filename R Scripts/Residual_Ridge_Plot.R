library(ggplot2)
library(cowplot)
library(extrafont)

setwd("./Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")
d5 <- agg_station_matrix[[6]]

setwd("../Functions")
source("residuals_function.R")

res5 <- residuals_function(d5, c(1,1,1))
plot_data5 <- res5 %>% gather(hour_of_day, count, "0":"23")
plot_data5$hour_of_day <- as.numeric(plot_data5$hour_of_day)

##ridge plot of residuals at each time point
p <- ggplot(data=plot_data5, mapping = aes(x = count, y = factor(hour_of_day), fill=0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 5) +xlim(-18,18) +
  scale_fill_viridis_c(direction = -1) +
  labs(x="Residual usage", y="Hour of day") +
  theme_light() +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p
setwd("../Images")
ggsave(p, filename = "residuals_ridges.pdf",  device=cairo_pdf, bg = "transparent", height=4, width=4.5, unit="in")

