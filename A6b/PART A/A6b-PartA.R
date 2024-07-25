#A6B Part A - ARCH and GARCH model
#AUTHOR : Rakshitha Vignesh Sargurunathan
#VID    : V01109007
#DATE   : 07/24/2024

setwd('E:\\VCU\\SCMA\\A6b')

# Load necessary libraries
library(quantmod)
library(tseries)
library(dplyr)
install.packages("rugarch")
library(rugarch)

# Load the data
data <- read.csv("AAPL.csv")

# Check for missing values and handle them
data <- data %>% 
  mutate(across(everything(), ~ ifelse(. == "", NA, .))) %>% 
  mutate(across(everything(), ~ na.approx(., na.rm = FALSE))) %>% 
  na.omit()

# Convert 'Vol.' and 'Change %' to numeric values
data$Vol. <- as.numeric(gsub(",", "", data$Volume.))
data$Change_pct <- as.numeric(gsub("%", "", data$Change.))

# Ensure 'Price', 'Open', 'High', and 'Low' are numeric
data$Price <- as.numeric(data$Price)
data$Open <- as.numeric(data$Open)
data$High <- as.numeric(data$High)
data$Low <- as.numeric(data$Low)

# Check for ARCH effects
returns <- diff(log(data$Price))
returns <- na.omit(returns)
arch_test <- ArchTest(returns)
print(arch_test)

# Fit an ARCH/GARCH model
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                   distribution.model = "norm")
fit <- ugarchfit(spec = spec, data = returns)
print(fit)

# Forecast the three-month volatility
forecast <- ugarchforecast(fit, n.ahead = 63) # 63 trading days ~ 3 months
vol_forecast <- sigma(forecast)
print(vol_forecast)