#require package
library(dplyr)
library(ggplot2)

#reproducibility
#set.seed(12)

#number of trials
trials<-1000000

#storage for the matching counts for each trial
results<-numeric(trials)

#storage for the matching that wins for each trial
matching_result<-numeric(trials)

#storage for the matching bullseye (0 or 1) for each trial
bullseye_result<-numeric(trials)

#storage for the matching double bullseye (0 or 1) for each trial
doublebullseye_result<-numeric(trials)

#number of spots (numbers to be picked)
spots<-10

#the winning number of match for each spots
if (spots == 10){
  winning<-c("0","5","6","7","8","9","10")
} else if (spots == 9){
  winning<-c("4","5","6","7","8","9")
} else if (spots == 8){
  winning<-c("4","5","6","7","8")
} else if (spots == 7){
  winning<-c("3","4","5","6","7")
} else if (spots == 6){
  winning<-c("3","4","5","6")
} else if (spots == 5){
  winning<-c("3","4","5")
} else if (spots == 4){
  winning<-c("2","3","4")
} else if (spots == 3){
  winning<-c("2","3")
} else if (spots == 2){
  winning<-c("2")
} else if (spots == 1){
  winning<-c("1")
} 

#numbers on the KENO card
lottery_numbers<-c(1:80)

#for loop of trials
for (i in 1:trials){
  
  #track the progress
  print(i)
  
  #numbers that the player chose
  numbers_selected<-sample(lottery_numbers,size=spots,replace=FALSE)
  
  #numbers that drew randomly
  numbers<-sample(lottery_numbers,size=20,replace=FALSE)
  
  #sum of the matching numbers_selected and numbers
  matching_counts_base<-sum(numbers_selected %in% numbers)
  
  #store the results into the variable results
  results[i]<-matching_counts_base
  
  #detect if the spots win
  matching_count<-ifelse(matching_counts_base %in% winning, 1, 0)
  
  #store 1 if wins and 0 if loses
  matching_result[i]<-matching_count
  
  #Bullseye
  bullseye<-sample(numbers, size = 1)
  
  #detect if the player wins the Bullseye (0 or 1)
  matching_bullseye <- ifelse(bullseye %in% numbers_selected || matching_counts_base %in% winning, 1, 0)
  
  #store the bullseye result into the variable bullseye_result
  bullseye_result[i]<-matching_bullseye
  
  #Double Bullseye
  double_bullseye<-sample(numbers, size = 2, replace = FALSE)
  
  #detect if the player wins the double Bullseye (0 or 1)
  matching_doublebullseye <- ifelse(any(double_bullseye %in% numbers_selected) || matching_counts_base %in% winning, 1, 0)
  
  
  #store the result of double bullseye into the varaible double bullseye result
  doublebullseye_result[i]<-matching_doublebullseye
}

#the probability for each match counts
matching_probabilities <- sapply(0:spots, function(count) {
  as.numeric(sprintf("%.10f", sum(results == count) / trials))
})

#add the name to the chart
names(matching_probabilities) <- as.character(0:spots)

#the probability of winning the base KENO
base_winning_probability<-sum(matching_probabilities[winning])

#the odds of winning the base KENO
base_winning_odds<-sum(matching_result)/(trials-sum(matching_result))

#the denominator of the probability of winning the base KENO
base_winning_probability_fraction<-trials/sum(matching_result)

#the denominator of the odds of winning the base KENO
base_winning_odds_fraction<-(trials-sum(matching_result))/sum(matching_result)

#the probability of winning the Bullseye KENO
bullseye_winning_probability<-sum(bullseye_result)/trials

#the odds of winning the Bullseye KENO
bullseye_winning_odds<-sum(bullseye_result)/(trials-sum(bullseye_result))

#the denominator of the probability of winning the bullseye KENO
bullseye_winning_probability_fraction<-trials/sum(bullseye_result)

#the denominator of the odds of winning the bullseye KENO
bullseye_winning_odds_fraction<-(trials-sum(bullseye_result))/sum(bullseye_result)

#the probability of winning the Double Bullseye KENO
doublebullseye_winning_probability<-sum(doublebullseye_result)/trials

#the odds of winning the Double Bullseye KENO
doublebullseye_winning_odds<-sum(doublebullseye_result)/(trials-sum(doublebullseye_result))

#the denominator of the probability of winning the Double Bullseye KENO
doublebullseye_winning_probability_fraction<-trials/sum(doublebullseye_result)

#the denominator of the odds of winning the Double Bullseye KENO
doublebullseye_winning_odds_fraction<-(trials-sum(doublebullseye_result))/sum(doublebullseye_result)

#Format the matching probabilities with column names
formatted_matching_probabilities <- paste(names(matching_probabilities), ": ", matching_probabilities, "\n", sep = "")

ggplot(data = data.frame(results = results), aes(x = results)) +
  geom_histogram(aes(fill = ..x..), color = "black", bins = max(results)+1, position = "identity", width = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Distribution of Match Frequencies",
    x = "Number of Matches",
    y = "Frequency"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )+
  scale_x_continuous(breaks = seq(0, 10, by = 2))

ggplot(data = data.frame(results = results), aes(x = results, y = stat(count / sum(count)))) +
  geom_histogram(aes(fill = ..x..), color = "black", bins = max(results) + 1, position = "identity", width = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Probability Distribution of  Matches",
    x = "Number of Matches",
    y = "Probability"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = seq(0, 10, by = 2))

cat(
  " Probability of each match", "\n",
  "-------------------------------------------------------------------------------", "\n",
  formatted_matching_probabilities,
  "-------------------------------------------------------------------------------", "\n",
  "Base KENO Winning Probability:", round(base_winning_probability,6), "(1 in",round(base_winning_probability_fraction,2),")\n",
  "Base KENO Winning Odds:", round(base_winning_odds,6), "(1 in", round(base_winning_odds_fraction,2), ")\n",
  "Bullseye KENO Winning Probability:", round(bullseye_winning_probability,6), "(1 in", round(bullseye_winning_probability_fraction,2),")\n",
  "Bullseye KENO Winning Odds:", round(bullseye_winning_odds,6), "(1 in", round(bullseye_winning_odds_fraction,2) ,")\n",
  if (spots >= 2) {
    paste(
      "Double Bullseye KENO Winning Probability:", round(doublebullseye_winning_probability,6), "(1 in", round(doublebullseye_winning_probability_fraction,2), ")\n",
      "Double Bullseye KENO Winning Odds:", round(doublebullseye_winning_odds,6), "(1 in", round(doublebullseye_winning_odds_fraction,2), ")\n"
    )
  }
  else {
    paste(
      "Double Bullseye KENO Winning Proability: NA", "\n",
      "Double Bullseye KENO WInning Odds: NA", "\n"
    )
  }
)








