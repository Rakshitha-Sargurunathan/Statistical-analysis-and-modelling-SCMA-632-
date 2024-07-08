#ASSIGNMENT - A4(1)
#PCA and FA on Survey.csv dataset
#AUTHOR : Rakshitha Vignesh Sargurunathan
#VID : V01109007
#CREATED ON : 07/01/2024

# Load required libraries
library(tidyverse)
library(corrplot)
library(FactoMineR)
library(factoextra)

# Load the dataset
setwd('E:\\VCU\\SCMA\\A4')
data <- read.csv('Survey.csv')
data

# Display the first few rows of the data
head(data)

# Display column names
colnames(data)

# Display the shape of the data
dim(data)

# Display data summary
summary(data)

# Check for missing values
colSums(is.na(data))

# Select numeric columns
numeric_data <- data %>% select_if(is.numeric)
print("\nTHE NUMERICAL COLUMNS ARE\n\n:")
print(colnames(numeric_data))

# Standardize the data
scaled_data <- scale(numeric_data)

# Compute the correlation matrix
R <- cor(scaled_data)
round(R, 2)

# Visualize the correlation matrix
corrplot(R, method = "circle", type = "upper", tl.cex = 0.8, number.cex = 0.7)

# Compute the covariance matrix
cov_matrix <- cov(scaled_data)

# Perform eigen decomposition
eigen_decomp <- eigen(cov_matrix)
lambdas <- eigen_decomp$values
eig_vector <- eigen_decomp$vectors

print(lambdas)
print(eig_vector)

# Compute explained variance
explained_variance <- lambdas / sum(lambdas)
cumulative_explained_variance <- cumsum(explained_variance)
n_components_90_variance <- which(cumulative_explained_variance >= 0.90)[1]

cat("\nNumber of components to reach 90% explained variance:", n_components_90_variance, "\n\n")

# Plot cumulative explained variance
plot(cumulative_explained_variance, type = "b", pch = 19, xlab = "Number of Principal Components", ylab = "Cumulative Explained Variance", main = "Cumulative Explained Variance Plot")
abline(h = 0.90, col = "red", lty = 2)

# Perform PCA
pca <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Create a DataFrame for the principal components
pca_df <- as.data.frame(pca$x)

# Plot the first two principal components
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.5) +
  labs(x = paste0("Principal Component 1 (", round(pca$sdev[1]^2 / sum(pca$sdev^2) * 100, 2), "%)"),
       y = paste0("Principal Component 2 (", round(pca$sdev[2]^2 / sum(pca$sdev^2) * 100, 2), "%)"),
       title = "PCA: First Two Principal Components") +
  theme_minimal()

# Save the transformed data to a new CSV file
write.csv(pca_df, "/path/to/save/pca_transformed_data.csv", row.names = FALSE)

# Install and load the 'psych' package for factor analysis
if (!require(psych)) install.packages("psych", dependencies = TRUE)
library(psych)

# Perform factor analysis
fa <- fa(numeric_data, nfactors = 5, rotate = "varimax")

# Check Eigenvalues to determine number of factors
ev <- fa$values
print("Eigenvalues:", ev)

# Get factor loadings
loadings <- fa$loadings
print("Factor Loadings:\n", loadings)

# Transform data
transformed_data <- fa$scores
print("Transformed Data:\n", transformed_data)

