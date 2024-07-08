#ASSIGNMENT - A4(3)
#Multidimensional Scaling on icecream.csv 
#AUTHOR : Rakshitha Vignesh Sargurunathan
#VID : V01109007
#CREATED ON : 07/01/2024


# Install and load necessary packages
install.packages("reshape2")
install.packages("ggdendro")
install.packages("GGally")
library(ggplot2)
library(reshape2)
library(ggdendro)
library(dplyr)
library(GGally)

# Load the dataset
setwd('E:\\VCU\\SCMA\\A4')
data <- read.csv('icecream.csv')
data

# Select numerical features
features <- data %>% select(-Brand)

# Standardize the data
features_scaled <- as.data.frame(scale(features))

# Compute the dissimilarity matrix
dissimilarity_matrix <- dist(features_scaled)

# Apply MDS
mds_fit <- cmdscale(dissimilarity_matrix, k = 2)
mds_df <- as.data.frame(mds_fit)
colnames(mds_df) <- c("Dimension 1", "Dimension 2")
mds_df$Brand <- data$Brand

# Apply K-Means clustering
set.seed(42)
n_clusters <- 3
kmeans_fit <- kmeans(mds_fit, centers = n_clusters)
mds_df$Cluster <- as.factor(kmeans_fit$cluster)

# 2D Scatter Plot with Clusters
ggplot(mds_df, aes(x = `Dimension 1`, y = `Dimension 2`, color = Cluster, label = Brand)) +
  geom_point(size = 5) +
  geom_text(vjust = -1, size = 4) +
  labs(title = "MDS of Brands with Clustering", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal() +
  scale_color_manual(values = c("red", "green", "blue"))

# Heatmap of the dissimilarity matrix
dissimilarity_matrix_matrix <- as.matrix(dissimilarity_matrix)
rownames(dissimilarity_matrix_matrix) <- data$Brand
colnames(dissimilarity_matrix_matrix) <- data$Brand

heatmap(dissimilarity_matrix_matrix, main = "Heatmap of the Dissimilarity Matrix", col = heat.colors(256))

# Combine MDS results with the original data for pairplot
combined_df <- cbind(data, mds_df[, c("Dimension 1", "Dimension 2", "Cluster")])

# Pairplot
ggpairs(combined_df, columns = 2:7, aes(color = Cluster, alpha = 0.5)) +
  theme_minimal()
