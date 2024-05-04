library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
# Assuming the data has been loaded into a variable called fifa_22
fifa_22 <- read_excel("C:/Users/luffy/OneDrive/Desktop/PROJECT/Career Mode player datasets - FIFA 15-22.xlsx", sheet = "FIFA 22")

fifa_22 <- fifa_22 %>%
  select(sofifa_id, age, overall, potential, value_eur) %>%
  filter(!is.na(age) & !is.na(overall) & !is.na(potential) & !is.na(value_eur))
model <- lm(potential ~ overall + age + value_eur, data = fifa_22)
summary(model)

fifa_22$predicted_future_overall <- predict(model, newdata = fifa_22)
print(summary(model))

p <- ggplot(fifa_22, aes(x = overall, y = predicted_future_overall)) +
  geom_point() +
  geom_line(aes(x = overall, y = potential), color = "red") +
  labs(title = "Predicted vs. Actual Future Overall Ratings",
       x = "Current Overall Rating",
       y = "Predicted Future Overall Rating")

interactive_plot <- ggplotly(p)

# Display the interactive plot
interactive_plot