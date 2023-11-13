data<-read.table(file=file.choose(),header=T,sep="\t")
View(data)
summary(data)
boxplot(data$calories)
data <- data[, -1]
View(data)
data_scaled <- scale(data)
pr.out<-prcomp(data_scaled,scale=FALSE,center=TRUE)
eigenvalues <- pr.out$sdev^2
variance_proportion <- eigenvalues / sum(eigenvalues)
plot(1:length(variance_proportion), variance_proportion, type = "b", 
     xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     main = "Scree Plot")
num_components <- 3
selected_components <- pr.out$x[, 1:num_components]
View(selected_components)
dist_matrix <- dist(selected_components)
hc_complete <- hclust(dist_matrix, method = "complete")
hc_single <- hclust(dist_matrix, method = "single")
hc_average <- hclust(dist_matrix, method = "average")
par(mfrow = c(1,3))
plot(hc_complete, main = 'Complete Linkage')
plot(hc_single, main = 'Single Linkage')
plot(hc_average, main = 'Average Linkage')
cut_height <- 3.3
cluster_assignments <- cutree(hc_complete, h = cut_height)
install.packages("dplyr")
library(dplyr)
selected_components_df <- as.data.frame(selected_components)
lineup_k2_complete <- mutate(selected_components_df, cluster = cluster_assignments)
count(lineup_k2_complete, cluster)
install.packages("ggplot2")
library(ggplot2)
install.packages("crayon")
library(crayon)
str(lineup_k2_complete)
install.packages("plotly")
library(plotly)
plot_ly(data = lineup_k2_complete, x = ~PC1, y = ~PC2, z = ~PC3, color = ~factor(cluster), type = "scatter3d") %>%
  add_markers() %>%
  layout(scene = list(aspectmode = "cube"))
model_km2 <- kmeans(selected_components, centers = 2)
install.packages("purrr")
library("purrr")
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = selected_components, centers = k)
  model$tot.withinss
})


elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)
library(ggplot2)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()
model_km3 <- kmeans(selected_components, centers = 4)
clust_km3 <- model_km3$cluster
lineup_km3 <- mutate(selected_components_df, cluster = clust_km3)
plot_ly(data = lineup_km3, x = ~PC1, y = ~PC2, z = ~PC3, color = ~factor(cluster), type = "scatter3d", aspectmode = "cube") %>%
  add_markers()
#linkage question,interpretaion with pca