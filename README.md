# Dimensionality_Reduction
Dimensionality reduction techniques (PCA and UMAP) for plotting breast cancer dataset.
``` {r }
require(ggplot2)
require(umap)
load("bc_data.RData")
dim_reduction(data = data, n.neighbors = 3)

````
