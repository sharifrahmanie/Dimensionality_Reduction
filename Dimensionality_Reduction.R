load("bc_data.RData")
require(ggplot2)
require(umap)
dim_reduction <- function(data, 
                          n.neighbors) {
  # By biomedical_informatics Edris Sharif Rahmani April 19,2023
  subtype <- data$subtype
  pca <- princomp(x = data[,-ncol(data)],  cor = TRUE, scores = TRUE, covmat = NULL)
  varpca <- pca$sdev^2 / sum(pca$sdev^2)
  pca_df <- predict(pca, data[-ncol(data)])
  pca_df <- data.frame(pca_df[, 1:2])
  pca_df$subtype <- subtype
  pca_df$subtype <- factor(pca_df$subtype)
  umap <- umap(data[, -ncol(data)],
               n_neighbors = n.neighbors,
               labels = subtype)
  umap_df <- data.frame(umap$layout, Subtype = subtype)
  my_pal <- c("#1B9E77", "#6E2C00","#D95F02", "#7570B3", "#E7298A",
              "#66A61E", "#E6AB02", "#A6761D", "#666666", "#9A7D0A")
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                   axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                   axis.text.y = element_text(family = "Times",colour = "black"),
                   plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                   axis.title.y = element_text(family = "Times", size = rel(1.4)),
                   axis.title.x = element_text(family = "Times", size = rel(1.4)))
  g1 <- ggplot(aes(x = Comp.1, y = Comp.2, color = subtype, fill = subtype), data =  pca_df) +
    geom_point(size = 2, shape = 21) +
    xlab(sprintf("PC1 (%2.2f%%)", varpca[1]*100)) +
    ylab(sprintf("PC2 (%2.2f%%)", varpca[2]*100)) +
    scale_color_manual(values=c(my_pal)) + stat_ellipse() +
    scale_fill_manual(values=c(my_pal)) +
    theme_classic() + mytheme +
    labs(subtitle = 'PCA plot')
  g2 <- ggplot(umap_df, aes(x = X1, y = X2, color = Subtype)) +
    geom_point(size = 2) +
    xlab('UMAP1') + ylab('UMAP2') +
    scale_color_manual(values=c(my_pal)) + 
    scale_fill_manual(values=c(my_pal)) +
    theme_classic() + mytheme +
    labs(subtitle = 'UMAP plot')
  pdf("DimReduction.pdf", width = 10, height = 8)
  print(g1)
  print(g2)
  dev.off()
}
dim_reduction(data = data, n.neighbors = 3)
