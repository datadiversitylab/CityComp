library(here)
library(plotly)
library(factoextra)

clim_30s <- read.csv(here("data", "clim_only_30s_cluster_data", "cities_5.csv"))
comb_30s <- read.csv(here("data", "combined_30s_cluster_data", "cities_6.csv"))
hum <- read.csv(here("data", "human_only_cluster_data", "cities_6_alldata_30sclim.csv"))
humd <- hum[,c(7:16)]
pc <- prcomp(humd, scale. = TRUE)
summary(pc)
PCAdata <- data.frame(pc$x, "cluster"=factor(km.res$cluster))

p <- plot_ly(PCAdata,x=~PC1,y=~PC2,text=~hum$City,
             mode="markers",color = ~cluster)
p

km.res <- kmeans(humd, 6)



p <- plot_ly(clim_30s,x=~PC1,y=~PC2,text=~City,
             mode="markers",color = ~Cluster)

p <- plot_ly(comb_30s,x=~PC1,y=~PC2,text=~City,
             mode="markers",color = ~Cluster)

