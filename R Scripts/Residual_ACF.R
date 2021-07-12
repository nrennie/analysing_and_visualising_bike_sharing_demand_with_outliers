library(ggplot2)
library(cowplot)
library(extrafont)

setwd("../Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")
d5 <- agg_station_matrix[[6]]
res5 <- residuals_function(d5, c(1,1,1))
output <- apply(d, 2, function(x)  acf(x, lag.max = 30))

#heat map
acf_matrix <- matrix(NA, nrow=24, ncol=31)
for (i in 1:24){
  acf_matrix[i,] <- output[[i]]$acf
}
colnames(acf_matrix) <- sapply(0:30, function(x) paste("Lag ", x, sep=""))
rownames(acf_matrix) <- 0:23

hour_labels <- c("00:00", "", "", "", "04:00", "", "", "",
                 "08:00", "", "", "", "12:00", "", "", "",
                 "16:00", "", "", "", "20:00", "", "", "")

acf_col <- function(acf_v){
  if (abs(acf_v) <= 0.05){
    return("|acf| <= 0.05") 
  }
  if (abs(acf_v) > 0.05 & abs(acf_v) <= 0.1){
    return("0.05 < |acf| <= 0.1") 
  }
  if (abs(acf_v) > 0.1 & abs(acf_v) <= 0.2){
    return("0.1 < |acf| <= 0.2") 
  }
  if (abs(acf_v) > 0.2){
    return("|acf| > 0.2") 
  }
}

plot_data <- tibble(data.frame(t(acf_matrix)))
plot_data$lag <- 0:30
colnames(plot_data) <- 0:23
long_data <- plot_data %>% gather(hour_of_day, acf_val, "0":"23")
colnames(long_data)[1] <- "lag"
long_data$acf_col <- sapply(long_data$acf_val, function(x) acf_col(x))
long_data$acf_col <- factor(long_data$acf_col, levels=rev(c("|acf| <= 0.05", "0.05 < |acf| <= 0.1", "0.1 < |acf| <= 0.2", "|acf| > 0.2")))

p <- ggplot(data=long_data, aes(x=as.numeric(hour_of_day), y=as.numeric(lag), fill=I(acf_col))) + 
  geom_tile() +
  scale_x_continuous(name="", breaks=0:23, labels=hour_labels) +
  scale_y_continuous(breaks=seq(0,30,5), labels=seq(0,30,5)) +
  scale_fill_manual(values=c("|acf| <= 0.05"="#abd9e9", "0.05 < |acf| <= 0.1"="#ffffbf", "0.1 < |acf| <= 0.2"="#fdae61", "|acf| > 0.2"="#d7191c")) +
  labs(y="Lag") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="right",
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p

setwd("../Images")
ggsave(p, filename = "acf_heatmap.pdf",  device=cairo_pdf, bg = "transparent", height=3.5, width=5.5, unit="in")





