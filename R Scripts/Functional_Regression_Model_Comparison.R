setwd("./Functions")
source("sse_function.R")
source("residuals_function_model_comp.R")

setwd("../Data")
agg_station_matrix <- readRDS("agg_station_matrix.rds")

d1 <- agg_station_matrix[[233]]
d2 <- agg_station_matrix[[238]]
d3 <- agg_station_matrix[[239]]
d4 <- agg_station_matrix[[244]]
d5 <- agg_station_matrix[[245]]
d6 <- agg_station_matrix[[246]]
d7 <- agg_station_matrix[[248]]
d8 <- agg_station_matrix[[442]]
d9 <- agg_station_matrix[[467]]

model_matrix <- matrix(c(0,0,0,
                         1,0,0,
                         0,1,0,
                         0,0,1,
                         1,1,0,
                         0,1,1,
                         1,0,1,
                         1,1,1), ncol=3, byrow=T)

#get sum of squared errors
terminal1_sse <- apply(model_matrix, 1, function(x) sse_function(d1, x))
terminal2_sse <- apply(model_matrix, 1, function(x) sse_function(d2, x))
terminal3_sse <- apply(model_matrix, 1, function(x) sse_function(d3, x))
terminal4_sse <- apply(model_matrix, 1, function(x) sse_function(d4, x))
terminal5_sse <- apply(model_matrix, 1, function(x) sse_function(d5, x))
terminal6_sse <- apply(model_matrix, 1, function(x) sse_function(d6, x))
terminal7_sse <- apply(model_matrix, 1, function(x) sse_function(d7, x))
terminal8_sse <- apply(model_matrix, 1, function(x) sse_function(d8, x))
terminal9_sse <- apply(model_matrix, 1, function(x) sse_function(d9, x))

round(terminal1_sse/nrow(d1), 2)
round(terminal2_sse/nrow(d2), 2)
round(terminal3_sse/nrow(d3), 2)
round(terminal4_sse/nrow(d4), 2)
round(terminal5_sse/nrow(d5), 2)
round(terminal6_sse/nrow(d6), 2)
round(terminal7_sse/nrow(d7), 2)
round(terminal8_sse/nrow(d8), 2)
round(terminal9_sse/nrow(d9), 2)
