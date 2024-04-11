
# Loading libraries and data.
library(tidyverse)
time_series_CO2 <- read_csv("hawai.csv")
str(time_series_CO2)
# Convert the "time" column to a Date format ( in YYYY.MM format)
time_series_CO2$time <- as.Date(paste0(time_series_CO2$time, "-01"), format = "%Y.%m-%d")

# Create a time series 
my_time_series <- ts(time_series_CO2$CO2, start = c(1958, 1), frequency = 12)
# Determine the split point
split_point <- round(0.7 * length(my_time_series))

# Splitting into training and testing sets
train <- head(my_time_series, split_point)
test <- tail(my_time_series, length(my_time_series) - split_point)

# Test that they are identical.# Should be TRUE
identical(train, head(my_time_series, split_point))  
identical(test, tail(my_time_series, length(my_time_series) - split_point)) 

##### 
library(fpp3)

# Convert data to tsibble format
train <- as_tsibble(train)
test <- as_tsibble(test)

# Fit an ARIMA model to the training data
arima_model <- train %>%
  model(
    ARIMA = ARIMA(value)
  )

# Forecast using the ARIMA model
forecast_values <- arima_model %>%
  forecast(h = nrow(test))

# Plot actual vs. forecasted values
autoplot(forecast_values) +
  labs(title = "ARIMA Forecast",
       x = "Date",
       y = "CO2 Concentration")

# Calculate residuals
residuals <- test$value - forecast_values$.mean

library(ggplot2)  

# Assuming residuals is a numeric vector or time series
# Create a data frame with time and residuals
residual_df <- data.frame(Time = time(residuals), Residuals = residuals)

# Plot residuals
ggplot(residual_df, aes(x = Time, y = Residuals)) +
  geom_line() +
  labs(title = "Residuals Plot",
       x = "Time",
       y = "Residuals")


# Autocorrelation Function (ACF) Plot of residuals
Acf(residuals, main = "ACF Plot of Residuals")

# Ljung-Box Test for residuals
Box.test(residuals, lag = 20, type = "Ljung-Box")


# Histogram of residuals
histogram_plot <- ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Q-Q plot of residuals
qq_plot <- ggplot(data.frame(Residuals = residuals), aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot of Residuals") +
  theme_minimal()

# Display plots
histogram_plot
qq_plot

shapiro_test <- residuals %>%
  stats::shapiro.test()

shapiro_test
