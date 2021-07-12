library(cowplot)
library(extrafont)
library(tidyverse)

setwd("./Data")
OD_data <- readRDS("OD_data.rds")
data_by_OD <- split(OD_data, OD_data$OD)
agg_station_matrix <- readRDS("agg_station_matrix.rds")

d1 <- data_by_OD[[17230]]
d2 <- data_by_OD[[28793]]
d3 <- data_by_OD[[28834]]
d1 <- mutate(d1, count = 1)
d2 <- mutate(d2, count = 1)
d3 <- mutate(d3, count = 1)
p1 <- ggplot() + 
  scale_y_continuous(name="Origin-Destination", limits=c(0.5,3.5), breaks = c(1,2,3), labels=c("31094-\n31924", "31203-\n31654", "31204-\n31002")) + 
  theme_light() + 
  geom_segment(aes(x=as.POSIXct(strptime("2017-01-01 00:00:00", "%Y-%m-%d %H:%M")),
                   xend=as.POSIXct(strptime("2019-12-31 23:59:59", "%Y-%m-%d %H:%M")), 
                   y=1, 
                   yend=1), 
               colour="black", size=0.2) +
  geom_segment(aes(x=as.POSIXct(strptime("2018-11-14 00:00:00", "%Y-%m-%d %H:%M")),
                   xend=as.POSIXct(strptime("2019-12-31 23:59:59", "%Y-%m-%d %H:%M")), 
                   y=2, 
                   yend=2), 
               colour="black", size=0.2) +
  geom_segment(aes(x=as.POSIXct(strptime("2017-01-01 00:00:00", "%Y-%m-%d %H:%M")),
                   xend=as.POSIXct(strptime("2019-12-31 23:59:59", "%Y-%m-%d %H:%M")), 
                   y=3, 
                   yend=3), 
               colour="black", size=0.2) +
  geom_point(data=d1, aes(x=as.POSIXct(strptime(Start.date, "%Y-%m-%d %H:%M")), y=count), colour="purple4", pch=19, size=1.5) +
  geom_point(data=d2, aes(x=as.POSIXct(strptime(Start.date, "%Y-%m-%d %H:%M")), y=count+1), colour="dodgerblue2", pch=19, size=1.5) +
  geom_point(data=d3, aes(x=as.POSIXct(strptime(Start.date, "%Y-%m-%d %H:%M")), y=count+2), colour="#FFAA50", pch=19, size=1.5) +
  labs(x="", title="", caption="(a) Time-stamp data for three\norigin-destinations") +
  coord_cartesian(expand=F) +
  scale_x_continuous(breaks=c(as.POSIXct(strptime("2017-01-01 02:47:53", "%Y-%m-%d %H:%M")), as.POSIXct(strptime("2018-01-01 02:47:53", "%Y-%m-%d %H:%M")), as.POSIXct(strptime("2019-01-01 02:47:53", "%Y-%m-%d %H:%M"))), labels=c("2017", "2018", "2019"), limits=c(as.POSIXct(strptime("2017-01-01 00:00:00", "%Y-%m-%d %H:%M")), as.POSIXct(strptime("2020-01-01 23:59:59", "%Y-%m-%d %H:%M")))) +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p1

#functional plot
d1_wide <- agg_station_matrix[[134]]
#plot demand over time (non-cumulative)
plot_data <- d1_wide %>% gather(hour_of_day, count, "0":"23")
plot_data$hour_of_day <- as.numeric(plot_data$hour_of_day)
hour_labels <- c("00:00", "04:00", 
                 "08:00", "12:00", 
                 "16:00", "20:00")
p2 <- ggplot() +
  geom_line(data = plot_data, mapping = aes(x = hour_of_day, y = count, group = date_of_day),size=0.1, col="grey") + theme_light() + 
  geom_line(aes(x=0:23, y=colMeans(d1_wide[,2:25])), colour="#A4343A", size=0.5) + theme_light() + 
  coord_cartesian(expand=F)+
  labs(caption="(b) Daily usage patterns for\nterminal 31203") +
  scale_y_continuous(name="Number of bikes hired per hour", limits=c(0,55)) + 
  scale_x_continuous(name="", breaks=seq(0,20,4), labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 3, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p2


p <- plot_grid(p1, p2, ncol=2, nrow=1, align="vh", rel_widths = c(1,1))
p
setwd("../Images")
ggsave(p, filename = "bookings_od_terminal.pdf",  device=cairo_pdf, bg = "transparent", height=3, width=5.5, unit="in")
