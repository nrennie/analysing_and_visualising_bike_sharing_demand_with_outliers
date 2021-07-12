library(ggplot2)
library(cowplot)
library(extrafont)
library(mrfDepth)
library(MASS)
library(lubridate)
library(tidyverse)

setwd("./Functions")
source("residuals_function.R")
source("depth.R")
source("depth_threshold.R")

setwd("../Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")
t_cols <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9')

#plot bookings
d1 <- agg_station_matrix[[245]]
d2 <- agg_station_matrix[[238]]
d3 <- agg_station_matrix[[239]]
d4 <- agg_station_matrix[[244]]
d5 <- agg_station_matrix[[233]] 
d6 <- agg_station_matrix[[246]]
d7 <- agg_station_matrix[[248]]
d8 <- agg_station_matrix[[442]]
d9 <- agg_station_matrix[[467]]

summer <- c("April", "May", "June", "July", "August", "September", "October")
winter <- c("November", "December", "January", "February", "March")


d5_s <- d5[which(months(d5$date_of_day) %in% summer),]
d5_w <- d5[which(months(d5$date_of_day) %in% winter),]
res5_s <- residuals_function(d5_s, c(1,1,1))
res5_w <- residuals_function(d5_w, c(1,1,1))

depths5_s <- mfd(array(t(res5_s[,2:25]), dim=c(ncol(res5_s[,2:25]),nrow(res5_s[,2:25]),1)), time=0:23, type="projdepth")$MFDdepthZ
c5_s <- depth_threshold(res5_s[,2:25],perc=0.01)
dates5_s <- as.Date(res5_s$date_of_day, origin="1970-01-01")
depths5_w <- mfd(array(t(res5_w[,2:25]), dim=c(ncol(res5_w[,2:25]),nrow(res5_w[,2:25]),1)), time=0:23, type="projdepth")$MFDdepthZ
c5_w <- depth_threshold(res5_w[,2:25],perc=0.01)
dates5_w <- as.Date(res5_w$date_of_day, origin="1970-01-01")
df5 <- data.frame(depths5=c(depths5_s, depths5_w), dates5=c(dates5_s, dates5_w))
df5 <- df5[order(df5$dates5),]
df5$obs5 <- 1:nrow(df5)

#plot 5
p5 <- ggplot(df5, aes(x=obs5, y=depths5, group = 1)) + 
  geom_segment(aes(x=obs5, xend=obs5, y=0.3, yend=depths5), colour=alpha(t_cols[5],0.5), size=0.2) + 
  geom_point(size=0.3) + 
  #add segment
  #geom_segment(aes(x=obs5[which(dates5 == "2017-01-01")], xend=obs5[which(dates5 == "2017-03-31")], y=c5_w, yend=c5_w), col="red") +
  #geom_segment(aes(x=obs5[which(dates5 == "2017-04-01")], xend=obs5[which(dates5 == "2017-10-31")], y=c5_s, yend=c5_s), col="red") +
  #geom_segment(aes(x=obs5[which(dates5 == "2017-11-01")], xend=obs5[which(dates5 == "2018-03-31")], y=c5_w, yend=c5_w), col="red") +
  #geom_segment(aes(x=obs5[which(dates5 == "2018-04-01")], xend=obs5[which(dates5 == "2018-10-31")], y=c5_s, yend=c5_s), col="red") +
  #geom_segment(aes(x=obs5[which(dates5 == "2018-11-01")], xend=obs5[which(dates5 == "2019-03-31")], y=c5_w, yend=c5_w), col="red") +
  #geom_segment(aes(x=obs5[which(dates5 == "2019-04-01")], xend=obs5[which(dates5 == "2019-10-31")], y=c5_s, yend=c5_s), col="red") +
  #geom_segment(aes(x=obs5[which(dates5 == "2019-11-01")], xend=obs5[which(dates5 == "2019-12-31")], y=c5_w, yend=c5_w), col="red") +
  ylab("Functional depths") + ylim(0.3,0.8) +
  labs(caption="(a) Functional depths") +
  scale_x_continuous(name="", breaks=c(df5$obs5[which(df5$dates5 == "2017-01-01")], 
                                       df5$obs5[which(df5$dates5 == "2018-01-03")],df5$obs5[which(df5$dates5 == "2019-01-03")]), labels=c("2017", "2018", "2019")) +
  theme_light() +
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

diffs5_s <- (c5_s - depths5_s)/c5_s
diffs5_w <- (c5_w - depths5_w)/c5_w
df5 <- data.frame(diffs5=c(diffs5_s, diffs5_w), dates5=c(dates5_s, dates5_w))
df5 <- df5[order(df5$dates5),]
df5$obs5 <- 1:nrow(df5)
#plot 5
p5b <- ggplot(df5, aes(x=obs5, y=diffs5, group = 1)) + 
  geom_segment(aes(x=obs5, xend=obs5, y=0, yend=diffs5), colour=alpha(t_cols[5],0.5), size=0.2) + 
  geom_point(size=0.3) + 
  geom_hline(yintercept=0, col="red") + 
  ylab("Standardised threshold distances") + ylim(-0.6,0.2) +
  coord_cartesian(expand = F) +
  labs(caption="(b) Normalised threshold exceedances") +
  scale_x_continuous(name="", breaks=c(df5$obs5[which(df5$dates5 == "2017-01-01")], 
                                       df5$obs5[which(df5$dates5 == "2018-01-03")],df5$obs5[which(df5$dates5 == "2019-01-03")]), labels=c("2017", "2018", "2019")) +
  theme_light() +
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
p5b

p <- plot_grid(p5, p5b, ncol=2, nrow=1, align="vh")
p
setwd("../Images")
ggsave(p, filename = "depths_diffs.pdf",  device=cairo_pdf, bg = "transparent", height=3, width=6, unit="in")

