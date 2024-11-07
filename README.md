# Version-1
Simple Linear Regression
# Load necessary libraries
library(readr)      # For reading CSV files
library(ggplot2)    # For creating plots
library(broom)      # For tidying model output

# Step 1: Read the data
# Read the CSV file named "walking_data.csv"
# The file should have two columns: "time" and "speed"
walking_data <- read_csv("walking_data.csv")

# Print the first few rows of the data to verify it's loaded correctly
print(head(walking_data))

# Step 2: Explore the data
# Calculate summary statistics for both variables
summary(walking_data)

# Create a scatter plot to visualize the relationship between time and speed
ggplot(walking_data, aes(x = time, y = speed)) +
  geom_point() +
  labs(title = "Scatter Plot of Walking Speed vs. Time",
       x = "Time",
       y = "Walking Speed") +
  theme_minimal()

# Step 3: Perform simple linear regression
# Fit a linear model where speed is the dependent variable and time is the independent variable
model <- lm(speed ~ time, data = walking_data)

# Step 4: Examine the model results
# Print a summary of the model, including coefficients, R-squared, and p-values
summary(model)

# Use broom to get a tidy version of the model coefficients
tidy_model <- tidy(model)
print(tidy_model)

# Step 5: Visualize the regression line
ggplot(walking_data, aes(x = time, y = speed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression: Walking Speed vs. Time",
       x = "Time",
       y = "Walking Speed") +
  theme_minimal()

# Step 6: Model diagnostics
# Create diagnostic plots to check assumptions
par(mfrow = c(2, 2))  # Set up a 2x2 plot layout
plot(model)

# Step 7: Predictions
# Create a data frame with new time values for prediction
new_times <- data.frame(time = seq(min(walking_data$time), max(walking_data$time), length.out = 100))

# Make predictions using the model
predictions <- predict(model, newdata = new_times, interval = "confidence")

# Combine predictions with new_times
prediction_data <- cbind(new_times, predictions)

# Plot the original data, regression line, and confidence interval
ggplot() +
  geom_point(data = walking_data, aes(x = time, y = speed)) +
  geom_line(data = prediction_data, aes(x = time, y = fit), color = "blue") +
  geom_ribbon(data = prediction_data, aes(x = time, ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(title = "Linear Regression with Confidence Interval",
       x = "Time",
       y = "Walking Speed") +
  theme_minimal()

# Print the R-squared value
cat("R-squared:", summary(model)$r.squared, "\n")

# Print the equation of the regression line
cat("Regression equation: speed =", 
    round(coef(model)[1], 3), "+", 
    round(coef(model)[2], 3), "* time\n")
