library(ggplot2)
library(astsa)

spots <- 1
trials <- 100000

prize_MO <- switch(spots,
                   `1` = c(0, 2),
                   `2` = c(0, 0, 10),
                   `3` = c(0, 0, 2, 25),
                   `4` = c(0, 0, 1, 5, 60),
                   `5` = c(0, 0, 0, 2, 20, 330),
                   `6` = c(0, 0, 0, 1, 6, 55, 1000),
                   `7` = c(0, 0, 0, 1, 2, 15, 100, 5000),
                   `8` = c(0, 0, 0, 0, 2, 6, 75, 550, 10000),
                   `9` = c(0, 0, 0, 0, 1, 5, 20, 125, 3000, 30000),
                   `10` = c(5, 0, 0, 0, 0, 2, 10, 45, 300, 5000, 100000)
)

prizeBW_MO <- switch(spots,
                     `1` = c(0, 50),
                     `2` = c(0, 15, 62),
                     `3` = c(0, 8, 17, 125),
                     `4` = c(0, 5, 11, 25, 300),
                     `5` = c(0, 5, 4, 12, 80, 930),
                     `6` = c(0, 5, 3, 6, 31, 155, 3500),
                     `7` = c(0, 5, 2, 4, 12, 75, 500, 12500),
                     `8` = c(0, 5, 2, 2, 7, 26, 200, 1800, 50000),
                     `9` = c(0, 5, 2, 2, 6, 15, 60, 525, 8000, 80000),
                     `10` = c(0, 5, 2, 2, 3, 7, 35, 145, 1300, 25000, 300000)
)

winning_amounts <- numeric(trials)
running_total <- 0  # Store running sum of winnings
results<-numeric(trials)

for (i in 1:trials) {
  print(i)
  numbers <- 1:80
  numbers_selected <- sample(numbers, size = spots, replace = FALSE)
  computer_selected <- sample(numbers, size = 20, replace = FALSE)
  bullseye_number <- sample(computer_selected, size = 1, replace = FALSE)
  
  matching_counts_base <- sum(numbers_selected %in% computer_selected)
  matching_counts_bullseye <- bullseye_number %in% numbers_selected
  
  # Calculate the current winning amount
  current_winning <- ifelse(matching_counts_bullseye, 
                            prizeBW_MO[matching_counts_base + 1], 
                            prize_MO[matching_counts_base + 1])
  
  winning_amounts[i] <- current_winning
  running_total <- running_total + current_winning  # Update running total
  results[i] <- running_total / i  # Compute average winnings
}

data <- data.frame("Trials" = 1:trials,
                   "Results" = results)

ggplot(data, aes(x = Trials, y = Results)) +
  geom_line() +
  labs(title = "Average Winnings Over Trials",
       x = "Number of Trials",
       y = "Average Winnings")
par(mfrow = c(2,1))
acf(data$Results, 10, plot=TRUE)
acf(data$Results, 10, plot=FALSE)
pacf(data$Results, 10, plot=TRUE)
pacf(data$Results, 10, plot=FALSE)
