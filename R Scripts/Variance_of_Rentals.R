library(zoo)
library(ggplot2)
library(extrafont)
library(changepoint)

setwd("./Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")

d1_wide <- agg_station_matrix[[134]][1:1095,]
long_data <- d1_wide %>% gather(hour_of_day, count, "0":"23")
long_data$hour_of_day <- as.numeric(long_data$hour_of_day)
long_data <- long_data[order(long_data$date_of_day, long_data$hour_of_day),]
long_data$obs <- 1:26280
long_data$var_24 <- rollapply(long_data$count, width = 24, FUN = var, na.pad=TRUE)

p <- ggplot() +
  geom_line(data=long_data, mapping=aes(x=obs, y=var_24), size=0.2) +
  scale_x_continuous("", breaks=c(1, 8761, 17521), labels=c("2017", "2018", "2019")) +
  geom_rect(mapping=aes(xmin=which(long_data$date_of_day == as.Date("2017-04-01"))[1], xmax=which(long_data$date_of_day == as.Date("2017-10-31"))[1], ymin=0, ymax=175), fill=alpha("green4", 0.3)) +
  geom_rect(mapping=aes(xmin=which(long_data$date_of_day == as.Date("2018-04-01"))[1], xmax=which(long_data$date_of_day == as.Date("2018-10-31"))[1], ymin=0, ymax=175), fill=alpha("green4", 0.3)) +
  geom_rect(mapping=aes(xmin=which(long_data$date_of_day == as.Date("2019-04-01"))[1], xmax=which(long_data$date_of_day == as.Date("2019-10-31"))[1], ymin=0, ymax=175), fill=alpha("green4", 0.3)) +
  ylim(0,175) +
  labs(y="Variance") +
  theme_light() +
  coord_cartesian(expand=F) +
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

p

setwd("../Images")
ggsave(p, filename = "variance_rentals.pdf",  device=cairo_pdf, bg = "transparent", height=2, width=5.5, unit="in")


fit_changepoint = cpt.var(long_data$count, know.mean = F, method="BinSeg", Q=100)
cpts(fit_changepoint)
plot(fit_changepoint)
p <- ggplot() +
  geom_line(data=long_data, mapping=aes(x=obs, y=var_24), size=0.2) +
  scale_x_continuous("", breaks=c(1, 8761, 17521), labels=c("2017", "2018", "2019")) +
  geom_rect(mapping=aes(xmin=1, xmax=1985, ymin=0, ymax=175), fill=alpha("green4", 0.3)) +
  geom_rect(mapping=aes(xmin=1985, xmax=7384, ymin=0, ymax=175), fill=alpha("dodgerblue2", 0.3)) +
  geom_rect(mapping=aes(xmin=7384, xmax=8157, ymin=0, ymax=175), fill=alpha("deeppink3", 0.3)) +
  geom_rect(mapping=aes(xmin=8157, xmax=10907, ymin=0, ymax=175), fill=alpha("purple4", 0.3)) +
  geom_rect(mapping=aes(xmin=10907, xmax=12209, ymin=0, ymax=175), fill=alpha("#FFAA00", 0.3)) +
  geom_rect(mapping=aes(xmin=12209, xmax=16146, ymin=0, ymax=175), fill=alpha("lightgrey", 0.3)) +
  geom_rect(mapping=aes(xmin=16146, xmax=19409, ymin=0, ymax=175), fill=alpha("gold2", 0.3)) +
  geom_rect(mapping=aes(xmin=19409, xmax=24907, ymin=0, ymax=175), fill=alpha("cyan3", 0.3)) +
  geom_rect(mapping=aes(xmin=24907, xmax=26280, ymin=0, ymax=175), fill=alpha("maroon3", 0.3)) +
  geom_vline(xintercept=2161, col="red") +
  geom_vline(xintercept=10921, col="red") +
  geom_vline(xintercept=19681, col="red") +
  geom_vline(xintercept=7273, col="red") +
  geom_vline(xintercept=16033, col="red") +
  geom_vline(xintercept=24793, col="red") +
  ylim(0,175) +
  labs(y="Variance") +
  theme_light() +
  coord_cartesian(expand=F) +
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

p
ggsave(p, filename = "cpt.pdf",  device=cairo_pdf, bg = "transparent", height=2, width=5.5, unit="in")
