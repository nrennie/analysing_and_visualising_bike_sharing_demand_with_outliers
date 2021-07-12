library(geosphere)
library(igraph)
library(cowplot)
library(ggplot2)
library(extrafont)

setwd("./Functions")
source("sdcs_function.R")
source("c_dist.R")

setwd("../Data")
station_data <- readRDS("station_data.rds")
agg_cor_mat <- readRDS("agg_cor_mat.rds")

#clustering function
clustering <- function(cor_mat, station_data, D_inner, D_outer, R, corr_threshold){
  #construct graph
  adj_mat <- matrix(0,nrow=nrow(station_data), ncol=nrow(station_data))
  for (i in 1:nrow(station_data)){
    for (j in 1:nrow(station_data)){
      if (i > j){
        # dist i and j < x from capitol
        if (station_data$centre_distances[i] < R & station_data$centre_distances[i] < R){
          if (c_dist(station_data$TERMINAL_NUMBER[i],station_data$TERMINAL_NUMBER[j]) < D_inner){
            adj_mat[i,j] = 1
            adj_mat[j,i] = 1
          }
        }
        #else
        else{
          if (c_dist(station_data$TERMINAL_NUMBER[i],station_data$TERMINAL_NUMBER[j]) < D_outer){
            adj_mat[i,j] = 1
            adj_mat[j,i] = 1
          }
        }
      }
    }
  }
  
  #input matrix
  input_mat <- adj_mat * (1-cor_mat)
  #mst clustering
  g_t <- graph.adjacency(adjmatrix=input_mat, mode="undirected", weighted=TRUE)
  mst_g_t <- mst(g_t)
  mst_weights <- edge_attr(mst_g_t)$weight
  edges_remove <- which(mst_weights >= (1 - corr_threshold))
  new_graph <- delete_edges(mst_g_t, edges_remove)
  components <- decompose(new_graph, min.vertices=1)
  cluster_list <- list()
  for (c in 1:length(components)){
    cluster_list[[c]] <- get.vertex.attribute(components[[c]])$name 
  }
  return(cluster_list)
  #return(list(cluster_list=cluster_list, adj_mat_corr=as_adjacency_matrix(new_graph, type="both", sparse=F)))
}

clustering_c <- clustering(cor_mat=agg_cor_mat, station_data=station_data, D_inner=500, D_outer=1000, R=5000, corr_threshold=0)

####################################################################################################################################
####################################################################################################################################

#Test clustering threshold
c <- seq(-1, 1, 0.05)
num_clusts_c <- numeric(length(c))
sdcs_c <- numeric(length(c))
for (i in 1:length(c)){
  clustering_c <- clustering(cor_mat=agg_cor_mat, station_data=station_data, D_inner=500, D_outer=1000, R=5000, corr_threshold=c[i])
  num_clusts_c[i] <- length(clustering_c)
  size_clusts <- sapply(clustering_c, function(x) length(x))
  sdcs_c[i] <- sdcs_function(size_clusts)
}

#Test radius
R <- seq(0, 30000, 1000)
num_clusts_R <- numeric(length(R))
sdcs_R <- numeric(length(R))
for (i in 1:length(R)){
  clustering_R <- clustering(cor_mat=agg_cor_mat, station_data=station_data, D_inner=500, D_outer=1000, R=R[i], corr_threshold=0.15)
  num_clusts_R[i] <- length(clustering_R)
  size_clusts <- sapply(clustering_R, function(x) length(x))
  sdcs_R[i] <- sdcs_function(size_clusts)
}

#Test inner distances
D_i <- seq(0, 5000, 250)
num_clusts_D_i <- numeric(length(D_i))
sdcs_D_i <- numeric(length(D_i))
for (i in 1:length(D_i)){
  clustering_D_i <- clustering(cor_mat=agg_cor_mat, station_data=station_data, D_inner=D_i[i], D_outer=1000, R=5000, corr_threshold=0.15)
  num_clusts_D_i[i] <- length(clustering_D_i)
  size_clusts <- sapply(clustering_D_i, function(x) length(x))
  sdcs_D_i[i] <- sdcs_function(size_clusts)
}

#Test outer distances
D_o <- seq(0, 5000, 250)
num_clusts_D_o <- numeric(length(D_o))
sdcs_D_o <- numeric(length(D_o))
for (i in 1:length(D_o)){
  clustering_D_o <- clustering(cor_mat=agg_cor_mat, station_data=station_data, D_inner=500, D_outer=D_o[i], R=5000, corr_threshold=0.15)
  num_clusts_D_o[i] <- length(clustering_D_o)
  size_clusts <- sapply(clustering_D_o, function(x) length(x))
  sdcs_D_o[i] <- sdcs_function(size_clusts)
}

#save results
saveRDS(num_clusts_c, "num_clusts_c.rds")
saveRDS(sdcs_c, "sdcs_c.rds")
saveRDS(num_clusts_R, "num_clusts_R.rds")
saveRDS(sdcs_R, "sdcs_R.rds")
saveRDS(num_clusts_D_i, "num_clusts_D_i.rds")
saveRDS(sdcs_D_i, "sdcs_D_i.rds")
saveRDS(num_clusts_D_o, "num_clusts_D_o.rds")
saveRDS(sdcs_D_o, "sdcs_D_o.rds")
####################################################################################################################################
####################################################################################################################################


d <- data.frame(x=c, y1=num_clusts_c, y2=sdcs_c)
p1 <- ggplot(data = d, aes(x = x)) + geom_line(aes(y = y1),size=0.5) + geom_line(aes(y = y2/0.1),size=0.5, col="dodgerblue2") + 
  labs(x = "", caption=expression(paste("(a) Correlation threshold, ", rho[tau]))) +  theme_light() + 
  scale_y_continuous(name = "Number of clusters", limits=c(0,600), sec.axis = sec_axis(~.*0.1, name="Standard deviation of\ncluster size (SDCS)")) +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.y.right = element_text(family="Times New Roman", color = "dodgerblue2", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0,1),legend.justification=c(0,1),
        legend.title=element_blank(),
        legend.key = element_blank())
p1

d <- data.frame(x=R, y1=num_clusts_R, y2=sdcs_R)
p2 <- ggplot(data = d, aes(x = x)) + geom_line(aes(y = y1),size=0.5) + geom_line(aes(y = y2/0.1),size=0.5, col="dodgerblue2") + 
  labs(x = "", caption=expression(paste("(b) Radius, ", R))) +  theme_light() + 
  scale_y_continuous(name = "Number of clusters", limits=c(0,600), sec.axis = sec_axis(~.*0.1, name="Standard deviation of\ncluster size (SDCS)")) +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.y.right = element_text(family="Times New Roman", color = "dodgerblue2", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0,1),legend.justification=c(0,1),
        legend.title=element_blank(),
        legend.key = element_blank())
p2

d <- data.frame(x=D_i, y1=num_clusts_D_i, y2=sdcs_D_i)
p3 <- ggplot(data = d, aes(x = x)) + geom_line(aes(y = y1),size=0.5) + geom_line(aes(y = y2/0.1),size=0.5, col="dodgerblue2") + 
  labs(x = "", caption=expression(paste("(c) Inner distance threshold, ", D[inner]))) +  theme_light() + xlim(0,2000) +
  scale_y_continuous(name = "Number of clusters", limits=c(0,600), sec.axis = sec_axis(~.*0.1, name="Standard deviation of\ncluster size (SDCS)")) +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.y.right = element_text(family="Times New Roman", color = "dodgerblue2", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0,1),legend.justification=c(0,1),
        legend.title=element_blank(),
        legend.key = element_blank())
p3


d <- data.frame(x=D_o, y1=num_clusts_D_o, y2=sdcs_D_o)
p4 <- ggplot(data = d, aes(x = x)) + geom_line(aes(y = y1),size=0.5) + geom_line(aes(y = y2/0.1),size=0.5, col="dodgerblue2") + 
  labs(x = "", caption=expression(paste("(d) Outer distance threshold, ", D[outer]))) +  theme_light() + 
  scale_y_continuous(name = "Number of clusters", limits=c(0,600), sec.axis = sec_axis(~.*0.1, name="Standard deviation of\ncluster size (SDCS)")) +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.y.right = element_text(family="Times New Roman", color = "dodgerblue2", size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title=element_text(family="Times New Roman", size=9),
        plot.caption=element_text(family="Times New Roman", size=11, hjust=0.5),
        legend.text=element_text(family="Times New Roman", size=9),
        plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0,1),legend.justification=c(0,1),
        legend.title=element_blank(),
        legend.key = element_blank())
p4

p <- plot_grid(p1, p2, p3, p4, ncol=2, nrow=2, align="vh")
p
setwd("../Images")
ggsave(p, filename = "MST_params.pdf",  device=cairo_pdf, bg = "transparent", height=5.5, width=5.5, unit="in")

