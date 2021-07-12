library(ggplot2)
library(cowplot)
library(extrafont)
library(ggridges)
library(viridis)

#data for clustering
setwd("../Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")
t_cols <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9')
hour_labels <- c("00:00", "", "", "", "", "", "", "",
                 "08:00", "", "", "", "", "", "", "",
                 "16:00", "", "", "", "", "", "", "")

#usage patterns
d5 <- agg_station_matrix[[6]]
plot_data5 <- d5 %>% gather(hour_of_day, count, "0":"23")
plot_data5$hour_of_day <- as.numeric(plot_data5$hour_of_day)
p1 <- ggplot() +
  geom_line(data = plot_data5, mapping = aes(x = hour_of_day, y = count, group = date_of_day),size=0.3, col=t_cols[5]) + theme_light() + 
  coord_cartesian(expand=F)+
  labs(caption="(a) Usage patterns") +
  scale_y_continuous(name="Usage", limits=c(0,22)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p1


#residual usage patterns
res5 <- residuals_function(d5, c(1,1,1))
plot_data5 <- res5 %>% gather(hour_of_day, count, "0":"23")
plot_data5$hour_of_day <- as.numeric(plot_data5$hour_of_day)
p2 <- ggplot() +
  geom_line(data = plot_data5, mapping = aes(x = hour_of_day, y = count, group = date_of_day),size=0.3, col=t_cols[5]) + theme_light() + 
  coord_cartesian(expand=F)+
  labs(caption="(b) Residual usage patterns") +
  scale_y_continuous(name="Residual usage", limits=c(-6,18)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
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




#partition residuals
summer <- c("April", "May", "June", "July", "August", "September", "October")
winter <- c("November", "December", "January", "February", "March")
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")

res5 <- filter(residuals_function(d5, c(1,1,1)), months(date_of_day) %in% summer & weekdays(date_of_day) %in% weekday)
plot_data5 <- res5 %>% gather(hour_of_day, count, "0":"23")
plot_data5$hour_of_day <- as.numeric(plot_data5$hour_of_day)
p3 <- ggplot() +
  geom_line(data = plot_data5, mapping = aes(x = hour_of_day, y = count, group = date_of_day),size=0.3, col=t_cols[5]) + theme_light() + 
  coord_cartesian(expand=F)+
  labs(caption="(c) Summer\nweekdays") +
  scale_y_continuous(name="Residual usage", limits=c(-6,18)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p3

res5 <- filter(residuals_function(d5, c(1,1,1)), months(date_of_day) %in% summer & weekdays(date_of_day) %in% weekend)
plot_data5 <- res5 %>% gather(hour_of_day, count, "0":"23")
plot_data5$hour_of_day <- as.numeric(plot_data5$hour_of_day)
p4 <- ggplot() +
  geom_line(data = plot_data5, mapping = aes(x = hour_of_day, y = count, group = date_of_day),size=0.3, col=t_cols[5]) + theme_light() + 
  coord_cartesian(expand=F)+
  labs(caption="(d) Summer\nweekends") +
  scale_y_continuous(name="Residual usage", limits=c(-6,18)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p4

res5 <- filter(residuals_function(d5, c(1,1,1)), months(date_of_day) %in% winter & weekdays(date_of_day) %in% weekday)
plot_data5 <- res5 %>% gather(hour_of_day, count, "0":"23")
plot_data5$hour_of_day <- as.numeric(plot_data5$hour_of_day)
p5 <- ggplot() +
  geom_line(data = plot_data5, mapping = aes(x = hour_of_day, y = count, group = date_of_day),size=0.3, col=t_cols[5]) + theme_light() + 
  coord_cartesian(expand=F)+
  labs(caption="(e) Winter\nweekdays") +
  scale_y_continuous(name="Residual usage", limits=c(-6,18)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p5

res5 <- filter(residuals_function(d5, c(1,1,1)), months(date_of_day) %in% winter & weekdays(date_of_day) %in% weekend)
plot_data5 <- res5 %>% gather(hour_of_day, count, "0":"23")
plot_data5$hour_of_day <- as.numeric(plot_data5$hour_of_day)
p6 <- ggplot() +
  geom_line(data = plot_data5, mapping = aes(x = hour_of_day, y = count, group = date_of_day),size=0.3, col=t_cols[5]) + theme_light() + 
  coord_cartesian(expand=F)+
  labs(caption="(f) Winter\nweekends") +
  scale_y_continuous(name="Residual usage", limits=c(-6,18)) + 
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p6


p_a <- plot_grid(p1, p2, ncol=2, nrow=1, align="vh")
p_b <- plot_grid(p3, p4, p5, p6, ncol=4, nrow=1, align="vh")
p <- plot_grid(p_a, p_b, nrow=2, ncol=1, align="vh")
p
setwd("../Images")
ggsave(p, filename = "residuals.pdf",  device=cairo_pdf, bg = "transparent", height=5.5, width=5.5, unit="in")

