library(astsa)
library(dplyr)
library(gt)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tseries)

#Data and the explantory variable table
data<-read.csv("/Users/antonyang/Downloads/Past Courses Note/Time Series/Time Series Project/Location1.csv")

training_weekly_data <- data %>%
  filter(year(Time) == c(2017, 2018, 2019, 2020)) %>%
  mutate(Date = floor_date(as.Date(Time), "week")) %>%  
  group_by(Date) %>%
  summarize(
    across(-c(Time), mean, na.rm = TRUE)          
  )

test_weekly_data<-data %>%
  filter(year(Time) == 2021) %>%
  mutate(Date = floor_date(as.Date(Time), "week")) %>%  
  group_by(Date) %>%
  summarize(
    across(-c(Time), mean, na.rm = TRUE)          
  ) %>%
  slice(-1)

explanatory_variables<-data.frame(
  Variables = c("Date", "temperature_2m", "relativehumidity_2m", "dewpoint_2m", "windspeed_10m", "windspeed_100m", "winddirection_10m", "winddirection_100m", "windgusts_10m", "Power"),
  Descriptions = c("Weekly date when readings occured.", "Temperature in degrees Farenheit at 2 meters above the surface.", "Relative humidity (as a percentage) at 2 meters above the surface.", "Dew point in degrees Farenheit at 2 meters above the surface.", "Wind speed in meters per second at 10 meters above the surface.", "Wind speed in meters per second at 100 meters above the surface", "Wind direction in degrees (0-360) at 10 meters above the surface", "Wind direction in degrees (0-360) at 100 meters above the surface.", "Wind gusts in meters per second at 100 meters above the surface.", "Turbine output, normalized to be between 0 and 1.")
)

explanatory_variables %>%
  gt() 

#Time series plot
par(mfrow = c(3,1))
tsdata<-ts(training_weekly_data$Power, start = c(2017, 2), frequency = 52)
tsplot(tsdata, col = 4, main = "Weekly Power Generation", ylab = "Power")
tsplot(diff(tsdata), col = 4, main = "First Difference Weekly Power Generation", ylab = "Power")
k = kernel("modified.daniell", 10)
tsplot(kernapply(tsdata, k), col=4, main="Seasonal Moving Average", ylab = "Power")

#Scatterplots
data_long <- training_weekly_data %>%
  pivot_longer(cols = -c(Date, Power), names_to = "Variable", values_to = "Value")

ggplot(data_long, aes(x = Value, y = Power)) +
  geom_point(alpha = 0.6, color = 4) +  # Add scatter points
  facet_wrap(~ Variable, scales = "free_x") +  # Create a separate plot for each variable
  labs(
    x = "Value of Variable",
    y = "Power"
  ) +
  theme_minimal()

#ACF and PACF
diff_acf2<-acf2(diff(training_weekly_data$Power),main = "")

#Time series plot for the test set
par(mfrow = c(3,1))
tsdata_test<-ts(test_weekly_data$Power, start = c(2021, 3), frequency = 52)
tsplot(tsdata_test, col = 4, main = "Weekly Power Generation", ylab = "Power")
tsplot(diff(tsdata_test), col = 4, main = "First Difference Weekly Power Generation", ylab = "Power")
k = kernel("modified.daniell", 5)
tsplot(kernapply(tsdata_test, k), col=4, main="Seasonal Moving Average", ylab = "Power")

#Models
model_data<-data.frame(Model = c(1, 2,  3, 4),
                       "." = c("ARIMA(0,1,1)", "ARIMA(0,1,1) with external regressors", "SARIMA(0,1,1)x(0,1,1)_52", "SARIMA(0,1,1)x(0,1,1)_52 with external regressors"))

model_data %>%
  gt()

#Spectral Analysis
mvspec(tsdata, main = "", col = 4, lty = 5, type = "o", pch = 20)

periodogram<-mvspec(tsdata, main = "", col = 4, lty = 5, type = "o", pch = 20)

#Models summary
model1<-sarima(tsdata, p=0, d=1, q=1, P=0, D=0, Q=0, S=52, no.constant = TRUE, details = FALSE)
invisible(capture.output({
  model1<-sarima(tsdata, p=0, d=1, q=1, P=0, D=0, Q=0, S=52, no.constant = TRUE, gg=TRUE)
}))

model2<-sarima(tsdata, p=0, d=1, q=1, P=0, D=0, Q=0, S=52, xreg = cbind(training_weekly_data$windspeed_100m, training_weekly_data$dewpoint_2m), no.constant = TRUE, details = FALSE)
invisible(capture.output({
  model2<-sarima(tsdata, p=0, d=1, q=1, P=0, D=0, Q=0, S=52, xreg = cbind(training_weekly_data$windspeed_100m, training_weekly_data$dewpoint_2m), no.constant = TRUE, gg = TRUE)
}))

model3<-sarima(tsdata, p=0, d=1, q=1, P=0, D=1, Q=1, S=52, no.constant = TRUE, details = FALSE)
invisible(capture.output({
  model3<-sarima(tsdata, p=0, d=1, q=1, P=0, D=1, Q=1, S=52, no.constant = TRUE, gg = TRUE)
}))

model4<-sarima(tsdata, p=0, d=1, q=1, P=0, D=1, Q=1, S=52, xreg = cbind(training_weekly_data$windspeed_100m, training_weekly_data$dewpoint_2m), no.constant = TRUE, details = FALSE)

invisible(capture.output({
  model3<-sarima(tsdata, p=0, d=1, q=1, P=0, D=1, Q=1, S=52, no.constant = TRUE, gg = TRUE)
}))

#Models plot on the training set
prd1 <- training_weekly_data$Power - resid(model1$fit)  
prde1 <- sqrt(model1$fit$sigma2) 

prd_ts1 <- ts(prd1, start = start(ts(training_weekly_data$Power)), frequency = frequency(ts(training_weekly_data$Power)))

tsplot(prd_ts1, 
       lwd = 2, 
       col = rgb(0, 0, 0.9, 0.5), 
       ylim = c(0, 1), 
       ylab = "Power",
       main = "Model 1: Predicted vs Observed Power")

points(time(prd_ts1), 
       training_weekly_data$Power, 
       pch = 16, 
       col = rgb(0.8, 0.3, 0))

x <- time(prd_ts1)  
xx <- c(x, rev(x))
yy <- c(prd1 - 2 * prde1, rev(prd1 + 2 * prde1))

polygon(xx, yy, 
        border = NA, 
        col = rgb(0.4, 0.5, 0.6, 0.15))  

legend("topright", 
       legend = c("Predicted", "Observed"), 
       lty = c(1, NA), 
       lwd = c(2, NA), 
       pch = c(NA, 16), 
       col = c(rgb(0, 0, 0.9, 0.5), rgb(0.8, 0.3, 0)), 
       cex = 0.9, 
       bty = "n")

prd2 <- training_weekly_data$Power - resid(model2$fit)  
prde2 <- sqrt(model2$fit$sigma2) 

prd_ts2 <- ts(prd2, start = start(ts(training_weekly_data$Power)), frequency = frequency(ts(training_weekly_data$Power)))

tsplot(prd_ts2, 
       lwd = 2, 
       col = rgb(0, 0, 0.9, 0.5), 
       ylim = c(0, 1), 
       ylab = "Power",
       main = "Model 2: Predicted vs Observed Power")

points(time(prd_ts2), 
       training_weekly_data$Power, 
       pch = 16, 
       col = rgb(0.8, 0.3, 0))

x <- time(prd_ts2)  
xx <- c(x, rev(x))
yy <- c(prd2 - 2 * prde2, rev(prd2 + 2 * prde2))

polygon(xx, yy, 
        border = NA, 
        col = rgb(0.4, 0.5, 0.6, 0.15))  

legend("topright", 
       legend = c("Predicted", "Observed"), 
       lty = c(1, NA), 
       lwd = c(2, NA), 
       pch = c(NA, 16), 
       col = c(rgb(0, 0, 0.9, 0.5), rgb(0.8, 0.3, 0)), 
       cex = 0.9, 
       bty = "n")

prd3 <- training_weekly_data$Power - resid(model3$fit)
prde3 <- sqrt(model3$fit$sigma2) 

prd_ts3 <- ts(prd3, start = start(ts(training_weekly_data$Date)), frequency = frequency(ts(training_weekly_data$Power)))

tsplot(prd_ts3, 
       lwd = 2, 
       col = rgb(0, 0, 0.9, 0.5), 
       ylim = c(0, 1), 
       ylab = "Power",
       main = "Model 3: Predicted vs Observed Power")

points(time(prd_ts3), 
       training_weekly_data$Power, 
       pch = 16, 
       col = rgb(0.8, 0.3, 0))

x <- time(prd_ts3)  
xx <- c(x, rev(x))
yy <- c(prd3 - 2 * prde3, rev(prd3 + 2 * prde3))

polygon(xx, yy, 
        border = NA, 
        col = rgb(0.4, 0.5, 0.6, 0.15))  

legend("topright", 
       legend = c("Predicted", "Observed"), 
       lty = c(1, NA), 
       lwd = c(2, NA), 
       pch = c(NA, 16), 
       col = c(rgb(0, 0, 0.9, 0.5), rgb(0.8, 0.3, 0)), 
       cex = 0.9, 
       bty = "n")

prd4 <- training_weekly_data$Power - resid(model4$fit)
prde4 <- sqrt(model4$fit$sigma2) 

prd_ts4 <- ts(prd4, start = start(ts(training_weekly_data$Date)), frequency = frequency(ts(training_weekly_data$Power)))

tsplot(prd_ts4, 
       lwd = 2, 
       col = rgb(0, 0, 0.9, 0.5), 
       ylim = c(0, 1), 
       ylab = "Power",
       main = "Model 4: Predicted vs Observed Power")

points(time(prd_ts4), 
       training_weekly_data$Power, 
       pch = 16, 
       col = rgb(0.8, 0.3, 0))

x <- time(prd_ts4)  
xx <- c(x, rev(x))
yy <- c(prd4 - 2 * prde3, rev(prd4 + 2 * prde3))

polygon(xx, yy, 
        border = NA, 
        col = rgb(0.4, 0.5, 0.6, 0.15))  

legend("topright", 
       legend = c("Predicted", "Observed"), 
       lty = c(1, NA), 
       lwd = c(2, NA), 
       pch = c(NA, 16), 
       col = c(rgb(0, 0, 0.9, 0.5), rgb(0.8, 0.3, 0)), 
       cex = 0.9, 
       bty = "n")

#Predictions
prediction1<-sarima.for(tsdata, 52, p=0, d=1, q=1, P=0, D=0, Q=0, S=52, plot.all = TRUE, no.constant = TRUE)

prediction2<-sarima.for(tsdata, 52, p=0, d=1, q=1, P=0, D=0, Q=0, S=52, xreg = cbind(training_weekly_data$windspeed_100m, training_weekly_data$dewpoint_2m), newxreg = cbind(test_weekly_data$windspeed_100m, test_weekly_data$dewpoint_2m), plot.all = TRUE, no.constant = TRUE)

prediction3<-sarima.for(tsdata, 52, p=0, d=1, q=1, P=0, D=1, Q=1, S=52, plot.all = TRUE, no.constant = TRUE)

prediction4<-sarima.for(tsdata, 52, p=0, d=1, q=1, P=0, D=1, Q=1, S=52, plot.all = TRUE, xreg = cbind(training_weekly_data$windspeed_100m, training_weekly_data$dewpoint_2m), newxreg = cbind(test_weekly_data$windspeed_100m, test_weekly_data$dewpoint_2m), no.constant = TRUE)

#MSE
mse1 <- mean((test_weekly_data$Power -prediction1$pred)^2)

mse2 <- mean((test_weekly_data$Power - prediction2$pred)^2)

mse3 <- mean((test_weekly_data$Power - prediction3$pred)^2)

mse4 <- mean((test_weekly_data$Power - prediction4$pred)^2)

mse_table <- data.frame(
  Model = c("ARIMA(0, 1, 1)", "ARIMA(0, 1, 1) with External Regressors", "SARIMA(0, 1, 1)x(0, 1, 1)_52", "SARIMA(0, 1, 1)x(0, 1, 1)_52 with External Regressors"),
  MSE = c(mse1, mse2, mse3, mse4)
)

mse_table %>%
  gt() %>%
  fmt_number(columns = vars(MSE), decimals = 4)
