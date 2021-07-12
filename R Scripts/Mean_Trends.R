library(tidyverse)
library(cowplot)
library(extrafont)
library(lubridate)


setwd("./Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")
hour_labels <- c("00:00", "", "", "", "", "", "", "",
                 "08:00", "", "", "", "", "", "", "",
                 "16:00", "", "", "", "", "", "", "")

###############################################################################################################################################
###############################################################################################################################################

############################################################### MEAN ######################################################################

d1_wide <- agg_station_matrix[[134]]
d1_wide$day <- weekdays(d1_wide$date_of_day, abbreviate = TRUE)
plot_data <- d1_wide %>% group_by(day)
plot_data <- plot_data %>% summarise_at(c(colnames(plot_data)[2:25]), mean, na.rm = TRUE)
plot_data <- plot_data[1:7,]
long_data <- plot_data %>% gather(hour_of_day, count, "0":"23")
long_data$hour_of_day <- as.numeric(long_data$hour_of_day)
long_data$day <- factor(long_data$day, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), labels=c("M", "Tu", "W", "Th", "F", "Sa", "Su"))
p1 <- ggplot() +
  geom_line(data = long_data, mapping = aes(x = hour_of_day, y = count, group = day, col=day),size=0.1) + theme_light() + 
  coord_cartesian(expand=F)+
  guides(colour=guide_legend(ncol=2)) +
  labs(caption="(a) Mean usage\npatterns by weekday") +
  scale_y_continuous(name="Usage per hour", limits=c(0,35)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(size=8),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0.0,1.05),legend.justification=c(0,1),
        legend.box.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p1


###############################################################################################################################################


d1_wide <- agg_station_matrix[[134]]
d1_wide$mon <- months(d1_wide$date_of_day, abbreviate = TRUE)
plot_data <- d1_wide %>% group_by(mon)
plot_data <- plot_data %>% summarise_at(c(colnames(plot_data)[2:25]), mean, na.rm = TRUE)
plot_data <- plot_data[1:12,]
long_data <- plot_data %>% gather(hour_of_day, count, "0":"23")
long_data$hour_of_day <- as.numeric(long_data$hour_of_day)
long_data$mon <- factor(long_data$mon, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                        labels=c("Ja", "Fe", "Mr", "Ap", "My", "Jn",
                                 "Jl", "Au", "Se", "Oc", "Nv", "De"))
p2 <- ggplot() +
  geom_line(data = long_data, mapping = aes(x = hour_of_day, y = count, group = mon, col=mon),size=0.1) + theme_light() + 
  coord_cartesian(expand=F)+
  guides(colour=guide_legend(ncol=2)) +
  labs(caption="(b) Mean usage\npatterns by month") +
  scale_y_continuous(name="Usage per hour", limits=c(0,35)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(size=8),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0.0,1.05),legend.justification=c(0,1),
        legend.box.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p2


###############################################################################################################################################


d1_wide <- agg_station_matrix[[134]]
d1_wide$year <- year(d1_wide$date_of_day)
plot_data <- d1_wide %>% group_by(year)
plot_data <- plot_data %>% summarise_at(c(colnames(plot_data)[2:25]), mean, na.rm = TRUE)
plot_data <- plot_data[1:3,]
long_data <- plot_data %>% gather(hour_of_day, count, "0":"23")
long_data$hour_of_day <- as.numeric(long_data$hour_of_day)
long_data$year <- factor(long_data$year)
p3 <- ggplot() +
  geom_line(data = long_data, mapping = aes(x = hour_of_day, y = count, group = year, col=year),size=0.1) + theme_light() + 
  coord_cartesian(expand=F)+
  labs(caption="(c) Mean usage\npatterns by year") +
  scale_y_continuous(name="Usage per hour", limits=c(0,35)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(size=8),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0.0,1.05),legend.justification=c(0,1),
        legend.box.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p3


###############################################################################################################################################
###############################################################################################################################################

############################################################### VARIANCE ######################################################################
d1_wide <- agg_station_matrix[[134]]
d1_wide$day <- weekdays(d1_wide$date_of_day, abbreviate = TRUE)
plot_data <- d1_wide %>% group_by(day)
plot_data <- plot_data %>% summarise_at(c(colnames(plot_data)[2:25]), var, na.rm = TRUE)
plot_data <- plot_data[1:7,]
long_data <- plot_data %>% gather(hour_of_day, count, "0":"23")
long_data$hour_of_day <- as.numeric(long_data$hour_of_day)
long_data$day <- factor(long_data$day, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), labels=c("M", "Tu", "W", "Th", "F", "Sa", "Su"))
p4 <- ggplot() +
  geom_line(data = long_data, mapping = aes(x = hour_of_day, y = count, group = day, col=day),size=0.1) + theme_light() + 
  coord_cartesian(expand=F)+
  guides(colour=guide_legend(ncol=2)) +
  labs(caption="(d) Inter-daily variance\nby weekday") +
  scale_y_continuous(name="Variance", limits=c(0,120)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(size=8),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0.0,1.05),legend.justification=c(0,1),
        legend.box.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p4


###############################################################################################################################################


d1_wide <- agg_station_matrix[[134]]
d1_wide$mon <- months(d1_wide$date_of_day, abbreviate = TRUE)
plot_data <- d1_wide %>% group_by(mon)
plot_data <- plot_data %>% summarise_at(c(colnames(plot_data)[2:25]), var, na.rm = TRUE)
plot_data <- plot_data[1:12,]
long_data <- plot_data %>% gather(hour_of_day, count, "0":"23")
long_data$hour_of_day <- as.numeric(long_data$hour_of_day)
long_data$mon <- factor(long_data$mon, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                        labels=c("Ja", "Fe", "Mr", "Ap", "My", "Jn",
                                 "Jl", "Au", "Se", "Oc", "Nv", "De"))
p5 <- ggplot() +
  geom_line(data = long_data, mapping = aes(x = hour_of_day, y = count, group = mon, col=mon),size=0.1) + theme_light() + 
  coord_cartesian(expand=F)+
  guides(colour=guide_legend(ncol=2)) +
  labs(caption="(e)Inter-daily variance\nby month") +
  scale_y_continuous(name="Variance", limits=c(0,120)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(size=8),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0.0,1.05),legend.justification=c(0,1),
        legend.box.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p5


###############################################################################################################################################


d1_wide <- agg_station_matrix[[134]]
d1_wide$year <- year(d1_wide$date_of_day)
plot_data <- d1_wide %>% group_by(year)
plot_data <- plot_data %>% summarise_at(c(colnames(plot_data)[2:25]), var, na.rm = TRUE)
plot_data <- plot_data[1:3,]
long_data <- plot_data %>% gather(hour_of_day, count, "0":"23")
long_data$hour_of_day <- as.numeric(long_data$hour_of_day)
long_data$year <- factor(long_data$year)
p6 <- ggplot() +
  geom_line(data = long_data, mapping = aes(x = hour_of_day, y = count, group = year, col=year),size=0.1) + theme_light() + 
  coord_cartesian(expand=F)+
  labs(caption="(f)  Inter-daily variance\nby year") +
  scale_y_continuous(name="Variance", limits=c(0,120)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(size=8),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0.0,1.05),legend.justification=c(0,1),
        legend.box.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p6


p <- plot_grid(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2, align="h", rel_widths = c(1,1))
p
setwd("../Images")
ggsave(p, filename = "mean_var_trends.pdf",  device=cairo_pdf, bg = "transparent", height=5.5, width=5.5, unit="in")


