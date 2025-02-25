library(dplyr)
library(pROC)
library(tidyverse)
library(neuralnet)
library(e1071)
library(naivebayes)
library(randomForest)
library(knitr)
library(gridExtra)
library(gt)

data<-read.csv('/Users/antonyang/Downloads/NFL Betting Machine Learning/QuarterBack_stats.csv')

set.seed(125)

data$completions_percentage <- data$completions/data$attempts
this_week <- 9
name <- 'A.Rodgers'
threshold_yard<-230

data$result<-ifelse(data$passing_yards > threshold_yard, 1, 0)
data$player_name <- as.factor(data$player_name)
data$season_type <- as.factor(data$season_type)
data$opponent_team <- as.factor(data$opponent_team)
data <- data %>%
  filter(completions_percentage >= 0.1 & !is.nan(completions_percentage))

training_set<-data%>%
  filter(season < 2024  & player_name == name)

test_set<-data%>%
  filter(season >= 2024 & player_name == name)

#Logistic Regression Model
model<-glm(result ~ week + attempts + interceptions + sacks + carries + passing_epa + passing_tds + passing_first_downs + completions_percentage + fantasy_points, data = training_set, family = "binomial")
final_model<-step(model, direction = "both", trace = 0)

test_set$predicted_probs_log<-predict(final_model, newdata = test_set, type = "response")

# Generate the ROC curve
roc_curve <- roc(test_set$result, test_set$predicted_probs_log, levels = c(0, 1), direction = "<")

# Plot the ROC curve
plot(roc_curve)

threshold<-0.6

test_set$predicted_result_log<-ifelse(test_set$predicted_probs_log >= threshold, 1, 0)

correct_predictions1 <- sum(test_set$predicted_result == test_set$result)
accuracy1 <- (correct_predictions1 / nrow(test_set))*100

#Simulated Result for this week
player_data<-data%>%
  filter(season == 2024 & player_name == name)%>%
  summarise(week = this_week, attempts = round(mean(attempts), 0), interceptions = round(mean(interceptions), 0), sacks = round(mean(sacks), 0), carries = round(mean(carries), 0), passing_epa = mean(passing_epa), passing_tds = round(mean(passing_tds), 0), passing_first_downs = round(mean(passing_first_downs), 0), completions_percentage = mean(completions_percentage), fantasy_points = mean(fantasy_points))

player_data$predicted_prob_log<-predict(final_model, newdata = player_data, type = "response")


#Naive Bayes
naive_model<-naiveBayes(result ~ week + attempts + interceptions + sacks + carries + passing_epa + passing_tds + passing_first_downs + completions_percentage + fantasy_points, data = training_set, usekernel = TRUE)
test_set$predicted_result_naive<-predict(naive_model, newdata = test_set)
test_set$predicted_probs_naive<-predict(naive_model, newdata = test_set, type = "raw")[,2]
correct_predictions3 <- sum(test_set$predicted_result_naive == test_set$result)
accuracy3 <- (correct_predictions3 / nrow(test_set)) * 100
pred3<-predict(naive_model, newdata = player_data, type = "raw")
player_data$predicted_prob_naive <- pred3[,2]

#Random Forest
forest_model<-randomForest(as.factor(result) ~ week + attempts + interceptions + sacks + carries + passing_epa + passing_tds + passing_first_downs + completions_percentage + fantasy_points, data = training_set, importance = TRUE)
test_set$predicted_result_forest<-predict(forest_model, newdata = test_set)
test_set$predicted_prob_forest<-predict(forest_model, newdata = test_set, type = "prob")[,2]
correct_predictions4 <- sum(test_set$predicted_result_forest == test_set$result)
accuracy4 <- (correct_predictions4 / nrow(test_set)) * 100
player_data$predicted_prob_forest<-predict(forest_model, newdata = player_data, type = "prob")[,2]

result_table <- data.frame(
  Model = c("Logistic Regression", "Naive Bayes", "Random Forest"),
  Accuracy = c(
    paste(round(accuracy1, 2), "%"),
    paste(round(accuracy3, 2), "%"),
    paste(round(accuracy4, 2), "%")
  ),
  Chances_of_Passing = c(
    paste(round(player_data$predicted_prob_log, 5) * 100, "%"),
    paste(round(player_data$predicted_prob_naive, 5) * 100, "%"),
    paste(round(player_data$predicted_prob_forest, 5) * 100, "%")
  )
)

result_table%>%
  gt()%>%
  tab_header(title = paste(name, "Passes Over", threshold_yard, "Yards in Week", this_week)) %>%
  cols_label(
    Model = "Model",
    Accuracy = "Accuracy",
    Chances_of_Passing = paste("Chances of Passing over", threshold_yard)
  )%>%
  fmt_missing(columns = everything(), rows = everything(), missing_text = "N/A") %>%
  tab_options(table.width = pct(100), table.align = "center")
  
  
  
  
  




