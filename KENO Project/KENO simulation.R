#required package
library(ggplot2)
library(dplyr)

#number of trials
trials<-10000

#amount of money (set it to 0 to know the gains and losses)
money<-0

#data of amount of money of each trial
money_history <- numeric(trials)

#all the numbers on the KENO card
lottery_numbers<-c(1:80)

for (trial in (1:trials)){
  
  #track the progress
  print(trial)
  
  #number of spots (numbers) to play
  spots<-sample(c(1:10), size = 1, replace = TRUE)
  
  #numbers that players picked
  numbers_picked<-sample(lottery_numbers, size=spots, replace = FALSE)
  
  #the amount of bet per draw
  bets<-sample(c(1,2,3,4,5,10,15,20), size = 1, replace = TRUE)
  
  #numbers of drawing
  drawings<-sample(c(1,2,3,4,5,10,20), size = 1, replace = TRUE)
  
  #subtract the amount of bettings times the number of drawings
  money<-money-(bets*drawings)
  
  #storage for the result of the 
  results<-NA
  
  #for loop of different numbers for each number of draw
  for (n in (1:drawings)){
    
    #number selected per draw
    numbers<-sample(lottery_numbers, size=20, replace = FALSE)
    
    #number of player's number match with the draws
    matching_counts<-sum(numbers_picked %in% numbers)
    
    #save the number of matching into the variable results
    results[n]<-matching_counts
  }
  
  #check the number of matching to see if the player wins
  for (i in (1:drawings)){
    if (spots==10){
      if (results[i]==10){
        money<-money+(bets*100000)
      } else if (results[i]==9){
        money<-money+(bets*5000)
      } else if (results[i]==8){
        money<-money+(bets*300)
      } else if (results[i]==7){
        money<-money+(bets*45)
      } else if (results[i]==6){
        money<-money+(bets*10)
      } else if (results[i]==5){
        money<-money+(bets*2)
      } else if (results[i]==0){
        money<-money+(bets*5)
      }
    } else if (spots == 9){
      if (results[i] == 9){
        money<-money+(bets*30000)
      } else if (results[i] == 8){
        money<-money+(bets*3000)
      } else if (results[i] == 7){
        money<-money+(bets*125)
      } else if (results[i] == 6){
        money<-money+(bets*20)
      } else if (results[i] == 5){
        money<-money+(bets*5)
      } else if (results[i] == 4){
        money<-money+(bets)
      }
    } else if (spots == 8){
      if (results[i] == 8){
        money<-money+(bets*10000)
      } else if (results[i] == 7){
        money<-money+(bets*550)
      } else if (results[i] == 6){
        money<-money+(bets*75)
      } else if (results[i] == 5){
        money<-money+(bets*6)
      } else if (results[i] == 4){
        money<-money+(bets*2)
      } 
    } else if (spots == 7){
      if (results[i] == 7){
        money<-money+(bets*5000)
      } else if (results[i] == 6){
        money<-money+(bets*100)
      } else if (results[i] == 5){
        money<-money+(bets*15)
      } else if (results[i] == 4){
        money<-money+(bets*2)
      } else if (results[i] == 3){
        money<-money+(bets)
      } 
    } else if (spots == 6){
      if (results[i] == 6){
        money<-money+(bets*1000)
      } else if (results[i] == 5){
        money<-money+(bets*55)
      } else if (results[i] == 4){
        money<-money+(bets*6)
      } else if (results[i] == 3){
        money<-money+(bets)
      } 
    } else if (spots == 5){
      if (results[i] == 5){
        money<-money+(bets*330)
      } else if (results[i] == 4){
        money<-money+(bets*20)
      } else if (results[i] == 3){
        money<-money+(bets*2)
      } 
    } else if (spots == 4){
      if (results[i] == 4){
        money<-money+(bets*60)
      } else if (results[i] == 3){
        money<-money+(bets*5)
      } else if (results[i] == 2){
        money<-money+(bets)
      } 
    } else if (spots == 3){
      if (results[i] == 3){
        money<-money+(bets*25)
      } else if (results[i] == 2){
        money<-money+(bets*2)
      } 
    } else if (spots == 2){
      if (results[i] == 2){
        money<-money+(bets*10)
      } 
    } else if (spots == 1){
      if (results[i] == 1){
        money<-money+(bets*2)
      }
    }
  }
  
  money_history[trial]<-money
}

# Create a data frame for plotting
plot_data <- data.frame(trial = 1:trials, money = money_history)

# Plot using ggplot2
ggplot(plot_data, aes(x = trial, y = money)) +
  geom_line(color = "gold") +
  labs(x = "Trials", y = "Money", title = "Money History Over Trials") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))


#print the amount of money
cat("Total money: ", ifelse(sign(money) == -1, "-$", "$"), abs(money),sep="")

  
  
  
  
  
  
  
  



