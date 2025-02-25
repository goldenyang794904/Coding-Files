library(ggplot2)
library(qcc)
library(forecast)
library(quantmod)

trials<-1000
money<-100
cost<-7
record<-numeric(trials+1)
record[1]<-money


for (i in 1:trials){
  print(i)
  dice<-sample(c(1:6), 2, replace = TRUE)
  result<-sum(dice)
  money<-money+result-cost
  record[i+1]<-money
}

money_data <- data.frame(
  Trial = 0:trials,  
  Money = record  
)

Changes<-data.frame(
  Trial = 0:trials,
  Changes = c(NA,diff(money_data$Money))
)

ggplot(money_data, aes(x = Trial, y = Money)) +  
  geom_line(color = "blue") +  
  labs(
    title = "Change in Money Over Time",  
    x = "Trial",  
    y = "Money"  
  ) + 
  theme_minimal()

#subgroup_size <- 2
#subgroup_changes <- matrix(Changes$Changes[-1], ncol = subgroup_size, byrow = TRUE)

#qcc(subgroup_changes, type = "R", title = "Moving Range Chart for Money Changes")

money_ts <- ts(money_data$Money, start = 0, frequency = 2)
# Apply Holt-Winters model
model <- HoltWinters(money_ts)

# Forecast and plot
forecasted <- forecast(model, h = 10)
plot(forecasted, main = "Holt-Winters Forecast for Money Over Time")








