library(tidyverse)
library(ggpubr)
library(extrafont)

setwd("./Data/Weather/Rain")
rain_matrix <- readRDS("rain_matrix.rds")
setwd(".../Data/Weather/Temp")
temp_matrix <- readRDS("temp_matrix.rds")

my_dates <- seq.Date(as.Date("2017/1/1"), as.Date("2019/12/31"), "days")
temp_day <- rowMeans(temp_matrix[,2:25], na.rm=T)
temp_prob_data <- data.frame(dates=my_dates, temp_day)
p1 <- ggplot() +
  geom_line(data = temp_prob_data, mapping = aes(x = dates, y = temp_day, col=temp_day),size=0.3) + theme_light() + 
  coord_cartesian(expand=F)+
  scale_colour_gradient("", low="gold2", high="red3", na.value = "lightgray") +
  labs(caption="(a) Average daily temperature (F)", x="") +
  scale_y_continuous(name="Temperature (F)", limits=c(15,90)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        plot.margin = unit(c(0.7, 0.2, 0.2, 0.2), "cm"), #top, right, bottom, left
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p1

r2 <- rowSums(rain_matrix[,2:25], na.rm=T)
plot_data <- data.frame(x=rain_matrix$date_of_day, y=r2)
p2 <- ggplot() +
  geom_line(data = plot_data, mapping = aes(x = x, y = r2, col=r2),size=0.3) + theme_light() + 
  coord_cartesian(expand=F)+
  scale_color_gradient("", low="lightblue", high="navy") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(caption="(b) Total daily precipitation (in)", x="") +
  scale_y_continuous(name="Precipitation (in)", limits=c(0,6)) + 
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        plot.margin = unit(c(0.7, 0.2, 0.2, 0.2), "cm"), #top, right, bottom, left
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())
p2

p <- ggarrange(p1, p2, ncol=2, nrow=1)
p
setwd("../Images")
ggsave(p, filename = "app_weather.pdf",  device=cairo_pdf, bg = "transparent", height=2.5, width=5, unit="in")


