library(readxl)
library(dplyr)
library(randomForest)

# Load data
data <- read_excel("C:/Users/luffy/OneDrive/Desktop/PROJECT/Career Mode player datasets - FIFA 22.xlsx")

data$player_positions <- as.factor(data$player_positions)

data <- data %>% select(overall, potential, age, value_eur, wage_eur ) %>% 
  na.omit()  # Removing rows with NA values
print(colnames(data))
# Convert factors if necessary


set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

rf_model <- randomForest(value_eur ~ ., data = train_data, ntree = 500, mtry = 3)

# Print model summary
print(rf_model)

# Make predictions on the test data
predictions <- predict(rf_model, newdata = test_data)

# Calculate RMSE or any other relevant metric
rmse <- sqrt(mean((predictions - test_data$value_eur)^2))
print(paste("RMSE:", rmse))

new_player_data <- data.frame(overall = 85, potential = 90, age = 22, wage_eur = 100000)
predicted_value <- predict(rf_model, newdata = new_player_data)
print(predicted_value)