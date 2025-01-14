library(here)
library(plotly)
library(factoextra)
library(reshape2)
library(data.table)

#Read in matrices
clim_30s <- read.csv(here("data", "clim_only_30s_cluster_data", "cities_5.csv"))
comb_30s <- read.csv(here("data", "combined_30s_cluster_data", "cities_6.csv"))

#Generate distance matrices
ds_clim_30s <- clim_30s[,c("PC1", "PC2")]
row.names(ds_clim_30s) <- clim_30s$City
distm_clim_30s <- as.matrix(dist(ds_clim_30s, method = "euclidean", 
                            diag = FALSE, upper = FALSE))

ds_comb_30s <- comb_30s[,c("PC1", "PC2")]
row.names(ds_comb_30s) <- comb_30s$City
distm_comb_30s <- as.matrix(dist(ds_comb_30s, method = "euclidean", 
                                 diag = FALSE, upper = FALSE))

#Export distances matrices
data.table::fwrite(round(distm_clim_30s, 3), file = here("data", "clim_only_30s_cluster_data", "dists.csv.gz"))
data.table::fwrite(round(distm_comb_30s, 3), file = here("data", "combined_30s_cluster_data", "dists.csv.gz"))


p <- plot_ly(clim_30s,x=~PC1,y=~PC2,text=~City,
             mode="markers",color = ~Cluster)

p <- plot_ly(comb_30s,x=~PC1,y=~PC2,text=~City,
             mode="markers",color = ~Cluster)


##Human-only dataset

dataScaled <- read.csv("data/cities_scaled.csv", row.names = 1)[,c(3:12)]
pc <- princomp(dataScaled)
km.res <- kmeans(dataScaled, 6)
PCAdata <- data.frame(pc$scores, "cluster"=factor(km.res$cluster))

plot_ly(PCAdata,x=~Comp.1,y=~Comp.2,text=~row.names(dataScaled),
        mode="markers",color = ~cluster)%>%
  layout(
    xaxis = list(
      range=c(-15,15)
    )
  )



#Final
# 
# # Climate only
# m <- data.table::fread(file = here("data", "clim_only_30s_cluster_data", "dists.csv.gz"))
# clim_30s <- read.csv(here("data", "clim_only_30s_cluster_data", "cities_5.csv"))
# City_chars <- clim_30s[,c(1:5,26)]
# City_pcs <- clim_30s[,c(27, 28)]
# 
# # Combined
# m <- data.table::fread(file = here("data", "combined_30s_cluster_data", "dists.csv.gz"))
# comb_30s <- read.csv(here("data", "combined_30s_cluster_data", "cities_6.csv"))
# City_chars <- comb_30s[,c(1:5,36)]
# City_pcs <- comb_30s[,c(37, 38)]

#Human only
dataScaled <- read.csv(here("data", "cities_scaled.csv"), row.names = 1)
datacoords <- read.csv(here("data", "cities_coords.csv"), row.names = 1)
pc <- princomp(dataScaled[,c(3:12)])
km.res <- kmeans(dataScaled[,c(3:12)], 6)
PCAdata <- data.frame(pc$scores, "cluster" = factor(km.res$cluster))
City_chars <- cbind.data.frame(City = row.names(dataScaled), 
                               Region =  dataScaled$Region, 
                               Cluster = km.res$cluster, 
                               Longitude =  datacoords$x,
                               Latitude = datacoords$y)
City_pcs <- PCAdata[,c(1,2)]
colnames(City_pcs) <- c("PC1", "PC2")
