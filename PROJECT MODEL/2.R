# Load necessary libraries
library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(tidyverse)

# Load the data
data <- read_excel("C:/Users/luffy/OneDrive/Desktop/PROJECT/Career Mode player datasets - FIFA 15-22.xlsx", sheet = "FIFA 22")

# Filter the data for players aged 20 to 22
filtered_data <- data %>% 
  filter(age >= 20 & age <= 22)

# Handling missing values in the market value
filtered_data <- filtered_data %>% 
  drop_na(value_eur)

# Selecting features and the target variable
analysis_data <- filtered_data %>% 
  select(overall, potential, value_eur)

# Splitting data into training and testing sets
set.seed(123)  # For reproducibility
training_rows <- createDataPartition(analysis_data$value_eur, p = 0.8, list = FALSE)
train_data <- analysis_data[training_rows, ]
test_data <- analysis_data[-training_rows, ]

# Train the Random Forest model
rf_model <- randomForest(value_eur ~ overall + potential, data = train_data, ntree = 100, importance = TRUE)

# Predict on the test set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model
rmse <- sqrt(mean((predictions - test_data$value_eur)^2))
rsquared <- summary(lm(predictions ~ test_data$value_eur))$r.squared

# Print model performance
print(list(RMSE = rmse, R_squared = rsquared))

# Optional: Plot feature importance
importance <- importance(rf_model)
varImpPlot(rf_model)
summary(rf_model)
coef(rf_model)

importance <- importance(rf_model)
print(importance)
