#ASSIGNMENT - A4(2)
#Cluster Analysis Survey.csv dataset
#AUTHOR : Rakshitha Vignesh Sargurunathan
#VID : V01109007
#CREATED ON : 07/01/2024

# Install necessary packages
install.packages("psych")
install.packages("factoextra")
install.packages("tidyverse")

# Load required libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)

# Load the dataset
setwd('E:\\VCU\\SCMA\\A4')
data <- read.csv('Survey.csv')

# Inspect the first few rows of the dataset
head(data)

# Get information about the dataset
str(data)

# Set the background variable
background_var <- 'Income'

# Extract the background variable
X <- data[[background_var]]

# Standardize the data
X_scaled <- scale(X)

# Determine the optimal number of clusters using the elbow method
wss <- function(k) {
  kmeans(X_scaled, k, nstart = 10)$tot.withinss
}

k.values <- 2:10
wss_values <- map_dbl(k.values, wss)

# Determine the silhouette score
silhouette_scores <- function(k) {
  km <- kmeans(X_scaled, centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(X_scaled))
  mean(ss[, 3])
}

sil_scores <- map_dbl(k.values, silhouette_scores)

# Plot the elbow curve and silhouette scores
elbow_plot <- ggplot() +
  geom_line(aes(x = k.values, y = wss_values), color = "blue") +
  geom_point(aes(x = k.values, y = wss_values), color = "blue") +
  scale_y_continuous(name = "Within Sum of Squares", sec.axis = sec_axis(~ ., name = "Silhouette Score")) +
  geom_line(aes(x = k.values, y = sil_scores), color = "red") +
  geom_point(aes(x = k.values, y = sil_scores), color = "red") +
  labs(x = "Number of Clusters", title = "Elbow Method and Silhouette Score For Optimal Number of Clusters") +
  theme_minimal()

print(elbow_plot)

# Fit the KMeans model with the optimal number of clusters
optimal_clusters <- 5
kmeans_result <- kmeans(X_scaled, centers = optimal_clusters, nstart = 10)
data$Cluster <- as.factor(kmeans_result$cluster)

# Analyze the clusters
cluster_centers <- scale(kmeans_result$centers, center = FALSE, scale = 1 / attr(X_scaled, "scaled:scale"))
cluster_centers <- sweep(cluster_centers, 2, attr(X_scaled, "scaled:center"), FUN = "+")
cluster_centers_df <- as.data.frame(cluster_centers)
colnames(cluster_centers_df) <- background_var

print(cluster_centers_df)

# Visualize the clusters
ggplot(data, aes_string(x = background_var, y = 0, color = "Cluster")) +
  geom_jitter(width = 0.1, height = 0) +
  labs(title = "Clusters Visualization based on Income", x = "Income", y = NULL) +
  theme_minimal()

# Detailed cluster profiling
for (cluster in 1:optimal_clusters) {
  cat(paste0("Cluster ", cluster, "\n"))
  cluster_data <- data %>% filter(Cluster == cluster)
  print(summary(cluster_data[[background_var]]))
  cat("\n")
}

# Heatmap of the cluster centers
heatmap(as.matrix(cluster_centers_df), Rowv = NA, Colv = NA, scale = "none", col = heat.colors(256), margins = c(5,10))

# Boxplot to compare the distribution of Income across clusters
ggplot(data, aes(x = Cluster, y = as.numeric(Income), fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Income Distribution Across Clusters", x = "Cluster", y = "Income") +
  theme_minimal()

# Violin plot to show the distribution density of Income across clusters
ggplot(data, aes(x = Cluster, y = as.numeric(Income), fill = Cluster)) +
  geom_violin() +
  labs(title = "Income Distribution Density Across Clusters", x = "Cluster", y = "Income") +
  theme_minimal()

