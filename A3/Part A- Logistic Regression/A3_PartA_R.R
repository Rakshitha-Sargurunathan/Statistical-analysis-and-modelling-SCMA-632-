#ASSIGNMENT - A3(Part A)
#Performing Logistic regression analysis on Campaign_responses dataset
#AUTHOR : Rakshitha Vignesh Sargurunathan
#VID : V01109007
#CREATED ON : 07/01/2024


setwd('E:\\VCU\\SCMA\\DATA')
install.packages("dplyr")
install.packages("caret")
install.packages("pROC")
library(dplyr)
library(caret)
library(pROC)
library(ggplot2)

# Load the dataset
data <- read.csv('campaign_responses.csv')
data

# Convert the target variable to binary
data$responded <- as.factor(ifelse(data$responded == 'Yes', 1, 0))

# Split the dataset into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(data$responded, p = 0.7, list = FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Fit a logistic regression model
logreg <- glm(responded ~ ., data = trainData, family = binomial)

# Make predictions on the testing set
pred_prob <- predict(logreg, testData, type = "response")
pred <- ifelse(pred_prob > 0.5, 1, 0)

# Evaluate the model with a confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred), as.factor(testData$responded))
print('Confusion Matrix:')
print(conf_matrix)

# Calculate the ROC curve and AUC
roc <- roc(testData$responded, pred_prob)
roc_auc <- auc(roc)

# Plot the ROC curve
ggroc(roc) + ggtitle(paste("ROC Curve (AUC =", round(roc_auc, 2), ")"))

# Print the classification report
print('Classification Report:')
print(conf_matrix$byClass)


# Load necessary libraries
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Fit a decision tree model
tree <- rpart(responded ~ ., data = trainData, method = "class")

# Make predictions on the testing set
pred_tree <- predict(tree, testData, type = "class")
pred_prob_tree <- predict(tree, testData, type = "prob")[, 2]

# Evaluate the model with a confusion matrix
conf_matrix_tree <- confusionMatrix(as.factor(pred_tree), as.factor(testData$responded))
print('Confusion Matrix (Decision Tree):')
print(conf_matrix_tree)

# Calculate the ROC curve and AUC
roc_tree <- roc(testData$responded, pred_prob_tree)
roc_auc_tree <- auc(roc_tree)

# Plot the ROC curve
ggroc(roc_tree) + ggtitle(paste("ROC Curve (Decision Tree) (AUC =", round(roc_auc_tree, 2), ")"))

# Plot the decision tree
rpart.plot(tree, type = 4, extra = 104)

# Print the classification report
print('Classification Report (Decision Tree):')
print(conf_matrix_tree$byClass)


