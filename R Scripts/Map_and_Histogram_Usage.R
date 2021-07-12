library(ggplot2)
library(extrafont)
library(cowplot)

setwd("./Data")
station_data <- readRDS("station_data.rds")
agg_station_matrix <- readRDS("agg_station_matrix.rds")

total_terminals <- lapply(agg_station_matrix, function(x) sum(x[,2:25]))
total_terminals <- unlist(total_terminals)/3
station_data$total_terminals <- total_terminals
#make plot
station_data_sorted <- station_data[order(station_data$total_terminals),]
state <- map_data("state")
p1 <- ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), fill="white", color = "black") + 
  guides(fill=FALSE) + 
  coord_fixed(1.3,xlim = c(-77.4, -76.8), ylim = c(38.7, 39.15), expand = FALSE) +
  geom_point(data=station_data_sorted, aes(x=LONGITUDE, y=LATITUDE, colour=total_terminals), pch=19, size=0.4) +
  #geom_segment(data=df1, aes(x = start_lon, y = start_lat, xend = end_lon, yend = end_lat, colour = "500-1,000")) +
  #geom_segment(data=df2, aes(x = start_lon, y = start_lat, xend = end_lon, yend = end_lat, colour = "1,000-5,000")) +
  #geom_segment(data=df3, aes(x = start_lon, y = start_lat, xend = end_lon, yend = end_lat, colour = ">5,000")) +
  #scale_colour_manual("", values=c("500-1,000"="#66c2a5", "1,000-5,000"="#fc8d62", ">5,000"="#8da0cb")) +
  scale_colour_gradient("", low="#e5f5e0", high="#00441b", limits=c(0,135000), breaks=c(0, 135000), labels=c("0","135,000")) +
  labs(caption="\n(a) Mean annual usage by\nterminal", x="", y="") +
  #annotate("text", x=-77, y=39.1, label="MARYLAND", size = 3, fontface=2) +
  #annotate("text", x=-77.2, y=38.75, label="VIRGINIA", size = 3, fontface=2) +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=9, hjust=0),
        legend.position=c(0.2,0.2),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.key.height = unit(0.4, "cm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"), #top, right, bottom, left
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p1


#Bar chart of terminal usage
agg_station_matrix <- readRDS("agg_station_matrix.rds")
total_terminals <- lapply(agg_station_matrix, function(x) sum(x[,2:25]))
total_terminals <- unlist(total_terminals)/3
df <- data.frame(x=factor(names(total_terminals), levels=names(total_terminals)), y=total_terminals)
p2 <- ggplot() + 
  geom_histogram(data=df, mapping=aes(x=y), position="identity", alpha=0.5, color="#74c476", fill="#74c476", binwidth=5000) +
  coord_cartesian(expand=F) +
  ylim(0,225) +
  scale_x_continuous(breaks=c(0,50000,100000), labels=c("0","50,000","100,000")) +
  labs(x="Mean annual usage", y="Number of terminals",
       caption="(b) Histogram of mean annual\nusage of terminals") +
  theme_light() +
  theme(axis.text.y=element_text(family="Times New Roman", size=9),
        axis.text.x = element_text(family="Times New Roman", size=9),
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
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"), #top, right, bottom, left
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.key = element_blank())

p2

p <- plot_grid(p1, p2, ncol=2, nrow=1, align="v", axis = "b")
p
setwd("../Images")
ggsave(p, filename = "usage_terminal.pdf",  device=cairo_pdf, bg = "transparent", height=3, width=5.5, unit="in")

