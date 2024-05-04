summary(data)
data <- data %>% drop_na(overall, potential, age, value_eur)

# Calculate the correlation between market value and other attributes
correlations <- cor(data[, c("overall", "potential", "age")], data$value_eur, use = "complete.obs")
print(correlations)

# Plotting
library(ggplot2)
ggplot(data, aes(x = overall, y = value_eur)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# Linear regression to see the influence of multiple attributes
fit <- lm(value_eur ~ overall + age + potential , data = data)
summary(fit)

# Plotting residuals to check for fit issues
plot(resid(fit))

anova_model <- anova(fit)
print(anova_model)


