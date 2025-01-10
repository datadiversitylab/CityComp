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


