library(moments)
library(ggplot2)
library(extrafont)

setwd("./Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")

skew_vec <- tibble(skew=unlist(lapply(agg_station_matrix, function(x) skewness(rowSums(x[,2:25])))))

#plot distribution of skewness 
p <- ggplot(skew_vec, aes(x=skew)) + 
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, color="#2c7fb8", fill="#2c7fb8")+
  geom_density(alpha=0.3, fill="#2c7fb8", colour="#2c7fb8")+
  xlim(-1,18) + ylim(0,0.75) +
  labs(x="Skewness", y="Density") +
  coord_cartesian(expand=F) +
  theme_light() +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), "cm"), #top, right, bottom, left
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p
setwd("../Images")
ggsave(p, filename = "skew_dist.pdf",  device=cairo_pdf, bg = "transparent", height=3, width=5, unit="in")



###############################################################################################################################################
###############################################################################################################################################

d1_wide <- agg_station_matrix[[165]]
total_days <- rowSums(d1_wide[,2:25])
standard_days <- (total_days - mean(total_days))/max(total_days)
df <- data.frame(standard_days)
p1 <- ggplot(df, aes(x=standard_days)) + 
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, color="#2c7fb8", fill="#2c7fb8")+
  geom_density(alpha=0.3, fill="#2c7fb8", colour="#2c7fb8")+
  xlim(-1,1) + ylim(0,4) +
  labs(x="Normalised total daily usage", y="Density",
       caption="(a) Normalised total daily usage\nfor terminal 31235") +
  coord_cartesian(expand=F) +
  theme_light() +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), "cm"), #top, right, bottom, left
        legend.title=element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p1

total_days <- log(rowSums(d1_wide[,2:25]))
standard_days <- (total_days - mean(total_days[1:1086]))/max(total_days)
df <- data.frame(standard_days)
p2 <- ggplot(df, aes(standard_days)) + 
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, color="#2c7fb8", fill="#2c7fb8")+
  geom_density(alpha=0.3, fill="#2c7fb8", colour="#2c7fb8")+
  xlim(-1,1) + ylim(0,4) +
  coord_cartesian(expand=F) +
  labs(x="Normalised total daily usage", y="Density",
       caption="(b) Normalised log-transformed total\ndaily usage for terminal 31235") +
  theme_light() +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="none",legend.justification=c(0,1),
        legend.title=element_blank(),
        plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), "cm"), #top, right, bottom, left
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p2

p <- plot_grid(p1, p2, ncol=2, nrow=1, align="vh", rel_widths = c(1,1))
p
ggsave(p, filename = "usage_histogram.pdf",  device=cairo_pdf, bg = "transparent", height=3, width=5.5, unit="in")
