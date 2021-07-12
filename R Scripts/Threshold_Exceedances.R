library(ggplot2)
library(cowplot)
library(extrafont)
library(mrfDepth)
library(MASS)
library(dplyr)

setwd("./Functions")
source("residuals_function.R")
source("depth.R")
source("depth_threshold.R")
source("merge_differences.R")

setwd("../Data")
start_station_matrix <- readRDS("start_station_matrix.rds")

#plot bookings
d1 <- agg_station_matrix[[233]]
d2 <- agg_station_matrix[[238]]
d3 <- agg_station_matrix[[239]]
d4 <- agg_station_matrix[[244]]
d5 <- agg_station_matrix[[245]]
d6 <- agg_station_matrix[[246]]
d7 <- agg_station_matrix[[248]]
d8 <- agg_station_matrix[[442]]
d9 <- agg_station_matrix[[467]]

summer <- c("April", "May", "June", "July", "August", "September", "October")
winter <- c("November", "December", "January", "February", "March")

d1_s <- d1[which(months(d1$date_of_day) %in% summer),]
d2_s <- d2[which(months(d2$date_of_day) %in% summer),]
d3_s <- d3[which(months(d3$date_of_day) %in% summer),]
d4_s <- d4[which(months(d4$date_of_day) %in% summer),]
d5_s <- d5[which(months(d5$date_of_day) %in% summer),]
d6_s <- d6[which(months(d6$date_of_day) %in% summer),]
d7_s <- d7[which(months(d7$date_of_day) %in% summer),]
d8_s <- d8[which(months(d8$date_of_day) %in% summer),]
d9_s <- d9[which(months(d9$date_of_day) %in% summer),]

d1_w <- d1[which(months(d1$date_of_day) %in% winter),]
d2_w <- d2[which(months(d2$date_of_day) %in% winter),]
d3_w <- d3[which(months(d3$date_of_day) %in% winter),]
d4_w <- d4[which(months(d4$date_of_day) %in% winter),]
d5_w <- d5[which(months(d5$date_of_day) %in% winter),]
d6_w <- d6[which(months(d6$date_of_day) %in% winter),]
d7_w <- d7[which(months(d7$date_of_day) %in% winter),]
d8_w <- d8[which(months(d8$date_of_day) %in% winter),]
d9_w <- d9[which(months(d9$date_of_day) %in% winter),]

res1_s <- residuals_function(d1_s, c(1,1,1))
res2_s <- residuals_function(d2_s, c(1,1,1))
res3_s <- residuals_function(d3_s, c(1,1,1))
res4_s <- residuals_function(d4_s, c(1,1,1))
res5_s <- residuals_function(d5_s, c(1,1,1))
res6_s <- residuals_function(d6_s, c(1,1,1))
res7_s <- residuals_function(d7_s, c(1,1,1))
res8_s <- residuals_function(d8_s, c(1,1,1))
res9_s <- residuals_function(d9_s, c(1,1,1))

res1_w <- residuals_function(d1_w, c(1,1,1))
res2_w <- residuals_function(d2_w, c(1,1,1))
res3_w <- residuals_function(d3_w, c(1,1,1))
res4_w <- residuals_function(d4_w, c(1,1,1))
res5_w <- residuals_function(d5_w, c(1,1,1))
res6_w <- residuals_function(d6_w, c(1,1,1))
res7_w <- residuals_function(d7_w, c(1,1,1))
res8_w <- residuals_function(d8_w, c(1,1,1))
res9_w <- residuals_function(d9_w, c(1,1,1))

#plot 1
diffs1 <- c(diffs1_s, diffs1_w)
names(diffs1) <- c(dates1_s, dates1_w)
diffs2 <- c(diffs2_s, diffs2_w)
names(diffs2) <- c(dates2_s, dates2_w)
diffs3 <- c(diffs3_s, diffs3_w)
names(diffs3) <- c(dates3_s, dates3_w)
diffs4 <- c(diffs4_s, diffs4_w)
names(diffs4) <- c(dates4_s, dates4_w)
diffs5 <- c(diffs5_s, diffs5_w)
names(diffs5) <- c(dates5_s, dates5_w)
diffs6 <- c(diffs6_s, diffs6_w)
names(diffs6) <- c(dates6_s, dates6_w)
diffs7 <- c(diffs7_s, diffs7_w)
names(diffs7) <- c(dates7_s, dates7_w)
diffs8 <- c(diffs8_s, diffs8_w)
names(diffs8) <- c(dates8_s, dates8_w)
diffs9 <- c(diffs9_s, diffs9_w)
names(diffs9) <- c(dates9_s, dates9_w)

x <- merge_differences(list(t31303=diffs1, t31308=diffs2, t31309=diffs3,
                            t31315=diffs4, t31316=diffs5, t31317=diffs6,
                            t31319=diffs7, t32014=diffs8, t32040=diffs9))
c <- apply(x, 1 ,function(y) sum(y[y>0]))
#plot zn
exceedances <- na.omit(c[c > 0])
dates <- as.Date(names(exceedances), origin="1970-01-01")
df <- data.frame(exceedances, dates)
p <- ggplot(df, aes(x=dates, y=exceedances, group = 1)) + geom_point(size=2) + 
  geom_segment(aes(x=dates,xend=dates,y=0,yend=exceedances)) + 
  geom_hline(yintercept=0, col="red") + 
  xlab("Date") + ylab("Sum of threshold exceedances") + ylim(0,0.5) +
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

p
setwd("../Images")
ggsave(p, filename = "sum_exceedances.pdf",  device=cairo_pdf, bg = "transparent", height=2.5, width=4.5, unit="in")

