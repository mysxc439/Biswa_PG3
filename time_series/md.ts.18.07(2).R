##  Ratio to Trend Modelling – AirPassengers (Linear and Quadratic Trend)

# Load data
data("AirPassengers")

# Step 1: Compute Yearly Averages
yearly_avg = aggregate(AirPassengers, nfrequency = 1, FUN = mean)
yearly_avg

# Time variable
t = 1:12

## --- LINEAR TREND MODEL ---
linear_trend = lm(yearly_avg ~ t)
summary(linear_trend)

# Estimate monthly linear trend
t_full = 6:149
monthly_linear_trend = coef(linear_trend)[1] + coef(linear_trend)[2] * ((t_full + 0.5) / 12)
plot(monthly_linear_trend, type = "l", main = "Monthly Linear Trend", ylab = "Trend", xlab = "Month")

# Step 2a: Detrending using Linear Trend
detrended_linear = AirPassengers / monthly_linear_trend
plot(detrended_linear, main = "Detrended Data (Linear)", ylab = "Ratio", xlab = "Month")

# Step 3a: Seasonal Indices from Linear Detrending
seasonal_indices_linear = tapply(detrended_linear, cycle(AirPassengers), mean, na.rm = TRUE)
seasonal_indices_linear=seasonal_component_linear/mean(seasonal_indices_linear)
plot(seasonal_indices_linear, type = "l", main = "Seasonal Indices (Linear)", ylab = "Index", xlab = "Month")

# Step 4a: Deseasonalise Data
seasonal_component_linear = rep(seasonal_indices_linear, length.out = length(AirPassengers))
deseasonalised_linear = AirPassengers / seasonal_component_linear
plot(deseasonalised_linear, main = "Deseasonalised Data (Linear)", ylab = "Value", xlab = "Month")

# Step 5a: Irregular Component (Linear)
irregular_component_linear = AirPassengers / (monthly_linear_trend * seasonal_component_linear)
plot(irregular_component_linear, main = "Irregular Component (Linear)", ylab = "Component", xlab = "Month")


## --- QUADRATIC (POLYNOMIAL) TREND MODEL ---
poly_trend = lm(yearly_avg ~ t + I(t^2))
summary(poly_trend)

# Estimate monthly trend from polynomial
monthly_poly_trend = 102.7064 + (19.1039/12)*(t_full + 0.5) + (0.9862/144)*((t_full + 0.5)^2)
plot(monthly_poly_trend, type = "l", main = "Monthly Quadratic Trend", ylab = "Trend", xlab = "Month")
trend_estimate = monthly_poly_trend

# Step 2b: Detrending using Polynomial Trend
detrended_poly = AirPassengers / trend_estimate
plot(detrended_poly, main = "Detrended Data (Quadratic)", ylab = "Ratio", xlab = "Month")

# Step 3b: Seasonal Indices from Polynomial Detrending
seasonal_indices_poly = tapply(detrended_poly, cycle(AirPassengers), mean, na.rm = TRUE)
seasonal_indices_poly=seasonal_indices_poly/mean(seasonal_indices_poly)
plot(seasonal_indices_poly, type = "l", main = "Seasonal Indices (Quadratic)", ylab = "Index", xlab = "Month")

# Step 4b: Deseasonalise Data
seasonal_component_poly = rep(seasonal_indices_poly, length.out = length(AirPassengers))
deseasonalised_poly = AirPassengers / seasonal_component_poly
plot(deseasonalised_poly, main = "Deseasonalised Data (Quadratic)", ylab = "Value", xlab = "Month")

# Step 5b: Irregular Component (Quadratic)
irregular_component_poly = AirPassengers / (trend_estimate * seasonal_component_poly)
plot(irregular_component_poly, main = "Irregular Component (Quadratic)", ylab = "Component", xlab = "Month")