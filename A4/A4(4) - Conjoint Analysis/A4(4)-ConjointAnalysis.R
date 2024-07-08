#ASSIGNMENT - A4(4)
#Conjoint Analysis on pizza_data.csv
#AUTHOR : Rakshitha Vignesh Sargurunathan
#VID : V01109007
#CREATED ON : 07/01/2024

# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)

# Read the dataset
setwd('E:\\VCU\\SCMA\\A4')
df <- read_csv("pizza_data.csv")

# Print dataset information
print(str(df))

# Convert categorical variables to factors
df <- df %>%
  mutate(across(c(brand, price, weight, crust, cheese, size, toppings, spicy), as.factor))

# Fit the conjoint analysis model
model <- lm(ranking ~ brand + price + weight + crust + cheese + size + toppings + spicy, data = df)
summary(model)

# Extract part-worth utilities
part_worths <- coef(model)
print("Part-worth Utilities:")
print(part_worths)

# Calculate importance of each attribute
attribute_importance <- data.frame(Attribute = names(part_worths)[-1],
                                   Importance = abs(part_worths[-1]))

# Normalize the importance
attribute_importance <- attribute_importance %>%
  group_by(Attribute) %>%
  summarise(Importance = sum(Importance)) %>%
  mutate(Relative_Importance = 100 * Importance / sum(Importance))

print("Relative Importance of Attributes:")
print(attribute_importance)

# Decode the part-worth utilities back to their original categories
part_worths_decoded <- coef(model)[-1]

# Plot relative importance of attributes
ggplot(attribute_importance, aes(x = Attribute, y = Relative_Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Relative Importance of Attributes", x = "Attributes", y = "Importance")

# Calculate utility scores for each profile
utility_scores <- df %>%
  mutate(utility = model$fitted.values)

# Print the profile with the highest utility score
best_profile <- utility_scores %>%
  arrange(desc(utility)) %>%
  slice(1)

print("The profile that has the highest utility score:")
print(best_profile)

# Identify the preferred level for each attribute
preferred_levels <- df %>%
  summarise(across(c(brand, price, weight, crust, cheese, size, toppings, spicy), 
                   ~ names(which.max(table(.)))))

print("Preferred Levels for Each Attribute:")
print(preferred_levels)

