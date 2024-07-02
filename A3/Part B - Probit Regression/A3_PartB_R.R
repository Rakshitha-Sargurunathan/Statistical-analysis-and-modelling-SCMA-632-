

# Load necessary libraries
library(dplyr)

# Load the dataset
setwd('E:\\VCU\\SCMA\\DATA')
data <- read.csv("NSSO68.csv")

# Display the first few rows of the dataset
head(data)

# Define the target variable (1 if non-vegetarian, 0 if vegetarian)
data$is_non_vegetarian <- ifelse(data$nonvegtotal_q > 0, 1, 0)

# Select features - you should choose the ones you think are relevant
# For demonstration, we select a subset of features (adjust based on your dataset)
features <- c('hhdsz', 'Religion', 'Social_Group', 'Type_of_land_owned', 'Land_Owned', 
              'MPCE_URP', 'Age', 'Sex', 'Education', 'Regular_salary_earner')

# Subset the data to include only the selected features and the target variable
data_subset <- data %>% select(one_of(features), is_non_vegetarian)

# Handle missing values
# Option 1: Remove rows with missing values
data_subset <- na.omit(data_subset)

# Option 2: Impute missing values (e.g., using the mean for numeric variables)
# library(mice)
# imputed_data <- mice(data_subset, m = 1, method = 'mean', maxit = 50, seed = 500)
# data_subset <- complete(imputed_data)

# Fit the Probit regression model
probit_model <- glm(is_non_vegetarian ~ ., data = data_subset, family = binomial(link = "probit"))

# Print the summary of the model
summary(probit_model)

# Discussing the results and characteristics of the Probit model:
# - The Probit model is a type of regression used for binary dependent variables.
# - It assumes that the probability of the binary outcome is related to a normally distributed latent variable.
# - Advantages of Probit model: It can handle cases where the dependent variable is binary, provides more realistic assumptions about the error terms, and is useful when the relationship between the independent variables and the probability of the outcome is nonlinear.