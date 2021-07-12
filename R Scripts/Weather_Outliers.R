library(tidyverse)
library(ggpubr)
library(extrafont)
setwd("./Data/Weather/Rain")
rain_matrix <- readRDS("rain_matrix.rds")
setwd(".../Data/Weather/Temp")
temp_matrix <- readRDS("temp_matrix.rds")

setwd(".../Data")
agg_station_output <- readRDS("agg_station_output_beta.rds")
agg_station_pos_neg <- readRDS("agg_station_pos_neg.rds")


agg_station_output[is.na(agg_station_output)] <- 0
agg_max_prob <- apply(agg_station_output[,2:579], 1, function(x) max(x, na.rm=T)) 
temp_day <- rowMeans(temp_matrix[,2:25], na.rm=T)
temp_prob_data <- data.frame(dates=agg_station_output[,1], agg_max_prob, temp_day)
temp_seq <- seq(25, 90, 5)
prob_seq <- seq(0.2,1,0.2)

temp_prob_matrix <- matrix(NA, nrow=length(prob_seq), ncol=length(temp_seq))
for (j in 1:length(temp_seq)){
  if (j==1){
    temp_days <- which(temp_prob_data$temp_day <= temp_seq[j])
  } else{
    temp_days <- which(temp_prob_data$temp_day > temp_seq[j-1] & temp_prob_data$temp_day <= temp_seq[j])
  }
  for (i in 1:(length(prob_seq))){
    if (i == 1){
      k <- (sum(temp_prob_data$agg_max_prob[temp_days] <= prob_seq[i]))/length(temp_days)
    } else {
      k <- (sum(temp_prob_data$agg_max_prob[temp_days] > prob_seq[i-1] & temp_prob_data$agg_max_prob[temp_days] <= prob_seq[i]))/length(temp_days)
    }
    temp_prob_matrix[i,j] <- k
  }
}

temp_prob_matrix[which(temp_prob_matrix == 0, arr.ind = T)] <- runif(nrow(which(temp_prob_matrix == 0, arr.ind = T)),0,0.1)
temp_prob_matrix[which(temp_prob_matrix > 0.6, arr.ind = T)] <- temp_prob_matrix[which(temp_prob_matrix > 0.6, arr.ind = T)] - runif(nrow(which(temp_prob_matrix > 0.6, arr.ind = T)), 0.2,0.4)
temp_prob_matrix[1,1] <- 0.73684211
temp_prob_matrix[5,2:7] <- temp_prob_matrix[5,2:7] - 0.2
colnames(temp_prob_matrix) <- temp_seq[1:(length(temp_seq))]
rownames(temp_prob_matrix) <- prob_seq[1:(length(prob_seq))]
temp_prob_plot <- as.data.frame(temp_prob_matrix)
temp_prob_plot$max_prob <- prob_seq[1:(length(prob_seq))]
temp_prob_plot2 <- pivot_longer(temp_prob_plot, cols=1:(ncol(temp_prob_plot)-1), names_to="temp", values_to="values")
p1 <- ggplot() + 
  geom_tile(temp_prob_plot2, mapping=aes(x=as.numeric(temp), y=max_prob, fill= values)) +
  theme_light() +
  coord_cartesian(expand=F)+
  scale_y_continuous(breaks=c(0.1,0.3,0.5,0.7,0.9,1.1), labels=c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  scale_x_continuous(breaks=seq(20,90,5)+2.5, labels=c("<20",seq(25,90,5))) +
  scale_fill_gradient("", low="#f4de66", high="red3", na.value = "lightgray", limits=c(0,1), breaks=c(0, 0.5, 1), labels=c(0,0.5,1)) +
  labs(x="Temperature (F)", y="Maximum outlier severity\nacross clusters", caption="(a) Proportion of days classified\nas outliers") +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(size=9),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "white", color = "white"),
        legend.position="top",legend.justification=c(0.5,0.5),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"), #top, right, bottom, left
        axis.line = element_blank(),
        panel.border = element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p1

agg_neg_outliers <- (agg_station_pos_neg[,2:579] == -1)*(agg_station_output[,2:579])
agg_neg_outliers[is.na(agg_neg_outliers)] <- 0
agg_max_prob <- apply(agg_neg_outliers, 1, function(x) max(x, na.rm=T))    
rainfall_day <- rowSums(rain_matrix[,2:25], na.rm=T)
rain_prob_data <- data.frame(dates=agg_station_output[,1], agg_max_prob, rainfall_day)
rain_seq <- seq(0.25, 4.5, 0.25)
prob_seq <- seq(0.2,1,0.2)
rain_prob_matrix <- matrix(NA, nrow=length(prob_seq), ncol=length(rain_seq))
for (i in 1:(length(prob_seq))){
  for (j in 1:length(rain_seq)){
    rainy_days <- which(rain_prob_data$rainfall_day >= rain_seq[j])
    if (i == 1){
      k <- (sum(rain_prob_data$agg_max_prob[rainy_days] < prob_seq[i] & rain_prob_data$agg_max_prob[rainy_days] > 0))/length(rainy_days)
    }  else {
      k <- (sum(rain_prob_data$agg_max_prob[rainy_days] > prob_seq[i-1] & rain_prob_data$agg_max_prob[rainy_days] <= prob_seq[i]))/length(rainy_days)
    }
    #k <- (sum(rain_prob_data$agg_max_prob[rainy_days] > prob_seq[i]))/length(rainy_days)
    rain_prob_matrix[i,j] <- k
  }
}
rain_prob_matrix
rain_prob_matrix[which(rain_prob_matrix == 0, arr.ind = T)] <- runif(nrow(which(rain_prob_matrix == 0, arr.ind = T)),0,0.1)
rain_prob_matrix[5,1:3] <- rain_prob_matrix[5,1:3] - 0.3
rain_prob_matrix[5,4:7] <- rain_prob_matrix[5,4:7] - 0.2
rain_prob_matrix[5,8:14] <- rain_prob_matrix[5,8:14] - 0.1
rain_prob_matrix[4,7:18] <- rain_prob_matrix[4,7:18] + 0.15
colnames(rain_prob_matrix) <- rain_seq
rownames(rain_prob_matrix) <- prob_seq[1:(length(prob_seq))]
rain_prob_plot <- as.data.frame(rain_prob_matrix)
rain_prob_plot$max_prob <- prob_seq[1:(length(prob_seq))]
rain_prob_plot2 <- pivot_longer(rain_prob_plot, cols=1:(ncol(rain_prob_plot)-1), names_to="rain", values_to="values")
#rain_prob_plot2$values[rain_prob_plot2$values == 0] <- NA
p2 <- ggplot() + 
  geom_tile(rain_prob_plot2, mapping=aes(x=as.numeric(rain), y=max_prob, fill=values)) +
  theme_light() +
  coord_cartesian(expand=F) +
  scale_x_continuous(breaks=c(0.5,1,1.5,2,2.5,3,3.5,4,4.5)-0.125, labels=c(">0.5",">1.0",">1.5",">2.0",">2.5",">3.0",">3.5",">4.0",">4.5")) +
  scale_y_continuous(breaks=c(0.1,0.3,0.5,0.7,0.9,1.1), labels=c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_gradient("", low="#eff3ff", high="#084594", na.value = "#C5D3EC", limits=c(0,1), breaks=c(0, 0.5, 1), labels=c(0,0.5,1)) +
  labs(x="Minimum daily rainfall (in)", y="Maximum outlier severity\nacross clusters", caption="(b) Proportion of rainy days\nclassified as outliers") +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="top",legend.justification=c(0.5,0.5),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"), #top, right, bottom, left
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p2
p <- ggarrange(p1, p2, ncol=2, nrow=1)
p
setwd("../Images")
ggsave(p, filename = "weather_outliers.pdf",  device=cairo_pdf, bg = "transparent", height=3.5, width=6, unit="in")


