
# vehicle_weight_mpg.R
# Clarice Landman-Montano
# 11/08/2026
# Analyzes vehicle weight and fuel efficiency to predict MPG.

# Load the built-in vehicle data
data(mtcars)
vehicle_data <- mtcars

# Plot MPG against vehicle weight
plot(vehicle_data$wt, vehicle_data$mpg,
     main = "Fuel Efficiency vs. Vehicle Weight",
     xlab = "Vehicle Weight (Thousands of Pounds)",
     ylab = "Fuel Efficiency (MPG)",
     pch = 19, col = "steelblue")

# Fit the regression model and add its line
fuel_efficiency_model <- lm(mpg ~ wt, data = vehicle_data)
abline(fuel_efficiency_model, col = "red", lwd = 2)

# Calculate slope statistics
regression_summary <- summary(fuel_efficiency_model)
weight_slope_estimate <- coef(fuel_efficiency_model)["wt"]
weight_slope_p_value <- regression_summary$coefficients["wt", "Pr(>|t|)"]
weight_slope_confidence_interval <- confint(fuel_efficiency_model,
                                            "wt", level = 0.95)

cat("Slope Estimate:", weight_slope_estimate, "\n")
cat("p-value:", weight_slope_p_value, "\n")
cat("95% Confidence Interval for Slope:",
    weight_slope_confidence_interval[1, 1],
    weight_slope_confidence_interval[1, 2], "\n")

# Interpret the slope and statistical significance
if (weight_slope_p_value < 0.05) {
  cat("The slope is statistically significant at the 0.05 level,",
      "indicating\n")
  cat("a significant relationship between weight and fuel efficiency.\n")
} else {
  cat("The slope is not statistically significant at the 0.05 level.\n")
}

# Predict MPG for a 6,000-pound vehicle
vehicle_6000lbs <- data.frame(wt = 6000 / 1000)

mpg_prediction_interval <- predict(fuel_efficiency_model,
                                   newdata = vehicle_6000lbs,
                                   interval = "prediction",
                                   level = 0.95)

# Print the prediction
cat("\nPredicted MPG for a 6,000-pound vehicle:\n")
print(mpg_prediction_interval)

# Evaluate the prediction
observed_weight_range <- range(vehicle_data$wt)

if (vehicle_6000lbs$wt < observed_weight_range[1] ||
    vehicle_6000lbs$wt > observed_weight_range[2]) {
  cat("\nNote: The weight value is outside the range of the dataset,",
      "which\n")
  cat("limits the accuracy of the prediction. Use caution when",
      "interpreting\n")
  cat("this result.\n")
}
