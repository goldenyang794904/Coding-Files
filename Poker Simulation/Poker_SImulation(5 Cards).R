library(tidyr)
library(dplyr)
library(ggplot2)

#Simulation of Texas Holdem (5 Cards) assuming that no one fold during the game

#Adjust the Number as you wish but the trials is best to be 10000 and Card_Best is best to be 3 or 5.

trials<-10000

Community_Table_Card<-3

#####################################################################################################################################################################

#Only run this part if you want to see the Probability of each hand (just delete the hash tags on line 13-18)

#Please Read this Before Running the Simulation!

#Distinct Hands is the number of different ways to draw the hand, not counting different suits
#Frequency is the number of ways to draw the hand, including the same card values in different suit
#Probability is the chance of getting this hand

#Probability_Hand<-data.frame("Hand"=c('Royal Flush','Straight Flush (Excluding Royal Flush)','Four of a Kind','Full House','Flush (Excluding Royal Flush and Straight Flush)','Straight (Excluding Royal Flush and Straight Flush)','Three of a Kind','Two Pairs', 'One Pair','High Card (No Pair)','Total'),
                             #"Distinct_Hands"=c(1,9,156,156,1277,10,858,858,2860,1277,7462),
                             #"Frequency"=c(4,36,624,3744,5108,10200,54912,123552,1098240,1302540,2598960),
                             #"Probability"=c('0.000154%','0.00139%','0.02401%','0.1441%','0.1965%','0.3925%','2.1128%','4,7539%','42.2569%','50.1177%','100%'))

#View(Probability_Hand)

#################################################################################################################################################################

Your_Card_Result <- data.frame('Hand' = rep(NA,trials),
                               'Result' = rep(NA,trials),
                               'Outcome'=rep(NA,trials))

Rank <- data.frame('Hand' = c('Royal Flush', 'Straight Flush', 'Four of a Kind', 'Full House', 'Flush', 'Straight', 'Three of a Kind', 'Two Pairs', 'One Pair', 'High Card'),
                   'Rank' = c(22.00, 21.00, 20.00, 19.00, 18.00, 17.00, 16.00, 15.00, 14.00, 13.00))

for (i in 1:trials) {
  print(i)
  # Collection for all the cards in the deck
  Deck <- data.frame('Rank' = rep(c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K'), each = 4),
                     'Suit' = rep(c('♥', '♦', '♠', '♣'), times = 13),
                     'Card' = character(52)
  )

  Deck$Card <- paste0(Deck$Rank, Deck$Suit)
  
  # Number of cards per player
  Number_of_Cards <- 2
  
  # Your Hand
  Your_Card <- data.frame('Card' = sample(Deck$Card, Number_of_Cards, replace = FALSE))

  # Adding Rank and Suit columns to Your_Card
  Your_Card$Rank <- substr(Your_Card$Card, 1, ifelse(substr(Your_Card$Card, 2, 2) %in% c("♥", "♦", "♠", "♣"), 1, 2))
  Your_Card$Suit <- substr(Your_Card$Card, ifelse(substr(Your_Card$Card, 2, 2) %in% c("♥", "♦", "♠", "♣"), 2, 3), 3)
  
  # Indexing Your Card from the Deck
  Index_Your_Card <- match(Your_Card$Card, Deck$Card)
  
  # Delete Your Card from the Deck
  Deck <- Deck[-Index_Your_Card, ]

  # River
  AllRiver <- data.frame('Card' = sample(Deck$Card, Community_Table_Card, replace = FALSE))
  
  # Adding Rank and Suit columns to Opponent3_Card
  AllRiver$Rank <- substr(AllRiver$Card, 1, ifelse(substr(AllRiver$Card, 2, 2) %in% c("♥", "♦", "♠", "♣"), 1, 2))
  AllRiver$Suit <- substr(AllRiver$Card, ifelse(substr(AllRiver$Card, 2, 2) %in% c("♥", "♦", "♠", "♣"), 2, 3), 3)
  
  # Combine the Hand with the River
  Your_AllCard <- rbind(Your_Card, AllRiver)
  
  
  #Counting the Number of Rank and Suits
  Count_Your_Card_Rank<-table(Your_AllCard$Rank)
  Count_Your_Card_Suit<-table(Your_AllCard$Suit)
  Pair_Your_Card_Rank<-Count_Your_Card_Rank[Count_Your_Card_Rank==2]
  Max_Straight<-0
  
  isAceStraight <- function(Ace_rank) {
    sortedHand2 <- sort(Ace_rank)
    isAceStraight <- FALSE
    
    for (i in 1:(length(sortedHand2)-4)) {
      subHand2 <- sortedHand2[i:(i+4)]
      if (all(diff(subHand2) == 1)) {
        isAceStraight <- TRUE
        Max_Straight<-max(Ace_rank)
        break
      }
    }
    
    return(isAceStraight)
  }
  
  Ace_rank <- Your_AllCard$Rank  
  Ace_rank <- gsub("J", 11, Ace_rank) 
  Ace_rank <- gsub("Q", 12, Ace_rank) 
  Ace_rank <- gsub("K", 13, Ace_rank)  
  Ace_rank <- gsub("A", 1, Ace_rank) 
  Ace_rank <- as.numeric(Ace_rank) 
  isAceHandStraight<-isAceStraight(Ace_rank)
  
  if (isAceHandStraight==TRUE) {
    Max_Straight<-max(Ace_rank)
  }

  isStraight <- function(hand_rank) {
    sortedHand1 <- sort(hand_rank)
    isStraight <- FALSE
    
    for (i in 1:(length(sortedHand1)-4)) {
      subHand1 <- sortedHand1[i:(i+4)]
      if (all(diff(subHand1) == 1)) {
        isStraight <- TRUE
        Max_Straight<-max(hand_rank)
        break
      }
    }
    
    return(isStraight)
  }
  
  hand_rank <- Your_AllCard$Rank  
  hand_rank <- gsub("J", 11, hand_rank) 
  hand_rank <- gsub("Q", 12, hand_rank) 
  hand_rank <- gsub("K", 13, hand_rank)  
  hand_rank <- gsub("A", 14, hand_rank) 
  hand_rank <- as.numeric(hand_rank) 
  isHandStraight<-isStraight(hand_rank)
  
  if (isHandStraight==TRUE) {
    Max_Straight<-max(hand_rank)
  }
  
  if ((sum(Your_AllCard$Rank %in% c('10', 'J', 'Q', 'K', 'A') & Your_AllCard$Suit == '♥') >= 5) ||
      (sum(Your_AllCard$Rank %in% c('10', 'J', 'Q', 'K', 'A') & Your_AllCard$Suit == '♦') >= 5) ||
      (sum(Your_AllCard$Rank %in% c('10', 'J', 'Q', 'K', 'A') & Your_AllCard$Suit == '♠') >= 5) ||
      (sum(Your_AllCard$Rank %in% c('10', 'J', 'Q', 'K', 'A') & Your_AllCard$Suit == '♣') >= 5)) {
    Your_Card_Result$Hand[i] <- Rank$Hand[1]
    Your_Card_Result$Result[i] <- Rank$Rank[1]
  } else if ((isHandStraight==TRUE||isAceHandStraight==TRUE)&(any(Count_Your_Card_Suit>=5))){
    Your_Card_Result$Hand[i] <- Rank$Hand[2]
    Your_Card_Result$Result[i] <- Rank$Rank[2]
  } else if (any(Count_Your_Card_Rank == 4)) {
    Your_Card_Result$Hand[i] <- Rank$Hand[3]
    Your_Card_Result$Result[i] <- Rank$Rank[3]
  } else if (any(Count_Your_Card_Rank==3)  && any(Count_Your_Card_Rank==2)){
    Your_Card_Result$Hand[i] <- Rank$Hand[4]
    Your_Card_Result$Result[i] <- Rank$Rank[4]
  } else if (any(Count_Your_Card_Suit >= 5)){
    Your_Card_Result$Hand[i] <- Rank$Hand[5]
    Your_Card_Result$Result[i] <- Rank$Rank[5]
  } else if (isHandStraight==TRUE||isAceHandStraight==TRUE){
    Your_Card_Result$Hand[i] <- Rank$Hand[6]
    Your_Card_Result$Result[i] <- Rank$Rank[6]
  } else if (any(Count_Your_Card_Rank == 3)) {
    Your_Card_Result$Hand[i] <- Rank$Hand[7]
    Your_Card_Result$Result[i] <- Rank$Rank[7]
  } else if (length(Pair_Your_Card_Rank) >= 2) {
    Your_Card_Result$Hand[i] <- Rank$Hand[8]
    Your_Card_Result$Result[i] <- Rank$Rank[8]
  } else if (length(Pair_Your_Card_Rank) == 1) {
    Your_Card_Result$Hand[i] <- Rank$Hand[9]
    Your_Card_Result$Result[i] <- Rank$Rank[9]
  } else if (all(Pair_Your_Card_Rank) == 1) {
    Your_Card_Result$Hand[i] <- Rank$Hand[10]
    Your_Card_Result$Result[i] <- Rank$Rank[10]
  }
  
  if (Your_Card_Result$Result[i]==Rank$Rank[2]){
    if (Max_Straight==14) {
      Your_Card_Result$Result[i]<-21.00
    } else if (Max_Straight==13) {
      Your_Card_Result$Result[i]<-20.95
    } else if (Max_Straight==12) {
      Your_Card_Result$Result[i]<-20.90
    } else if (Max_Straight==11) {
      Your_Card_Result$Result[i]<-20.85
    } else if (Max_Straight==10) {
      Your_Card_Result$Result[i]<-20.80
    } else if (Max_Straight==9) {
      Your_Card_Result$Result[i]<-20.75
    } else if (Max_Straight==8) {
      Your_Card_Result$Result[i]<-20.70
    } else if (Max_Straight==7) {
      Your_Card_Result$Result[i]<-20.65
    } else if (Max_Straight==6) {
      Your_Card_Result$Result[i]<-20.60
    } else if (Max_Straight==5) {
      Your_Card_Result$Result[i]<-20.55
    } else if (Max_Straight==4) {
      Your_Card_Result$Result[i]<-20.50
    } else if (Max_Straight==3) {
      Your_Card_Result$Result[i]<-20.45
    } else if (Max_Straight==2) {
      Your_Card_Result$Result[i]<-20.40
    } else if (Max_Straight==1) {
      Your_Card_Result$Result[i]<-20.35
    }
  }
  
  if (Your_Card_Result$Result[i]==Rank$Rank[6]){
    if (Max_Straight==14) {
      Your_Card_Result$Result[i]<-17.00
    } else if (Max_Straight==13) {
      Your_Card_Result$Result[i]<-16.95
    } else if (Max_Straight==12) {
      Your_Card_Result$Result[i]<-16.90
    } else if (Max_Straight==11) {
      Your_Card_Result$Result[i]<-16.85
    } else if (Max_Straight==10) {
      Your_Card_Result$Result[i]<-16.80
    } else if (Max_Straight==9) {
      Your_Card_Result$Result[i]<-16.75
    } else if (Max_Straight==8) {
      Your_Card_Result$Result[i]<-16.70
    } else if (Max_Straight==7) {
      Your_Card_Result$Result[i]<-16.65
    } else if (Max_Straight==6) {
      Your_Card_Result$Result[i]<-16.60
    } else if (Max_Straight==5) {
      Your_Card_Result$Result[i]<-16.55
    } else if (Max_Straight==4) {
      Your_Card_Result$Result[i]<-16.50
    } else if (Max_Straight==3) {
      Your_Card_Result$Result[i]<-16.45
    } else if (Max_Straight==2) {
      Your_Card_Result$Result[i]<-16.40
    } else if (Max_Straight==1) {
      Your_Card_Result$Result[i]<-16.35
    }
  }
  
  if (Your_Card_Result$Result[i] == Rank$Rank[10]) {
    if (any(Your_Card$Rank == 'A')) {
      Your_Card_Result$Result[i] <- 13.00
    } else if (any(Your_Card$Rank == 'K')) {
      Your_Card_Result$Result[i] <- 12.00
    } else if (any(Your_Card$Rank == 'Q')) {
      Your_Card_Result$Result[i] <- 11.00
    } else if (any(Your_Card$Rank == 'J')) {
      Your_Card_Result$Result[i] <- 10.00 
    } else if (any(Your_Card$Rank == '10')) {
      Your_Card_Result$Result[i] <- 9.00
    } else if (any(Your_Card$Rank == '9')) {
      Your_Card_Result$Result[i] <- 8.00 
    } else if (any(Your_Card$Rank == '8')) {
      Your_Card_Result$Result[i] <- 7.00 
    } else if (any(Your_Card$Rank == '7')) {
      Your_Card_Result$Result[i] <- 6.00
    } else if (any(Your_Card$Rank == '6')) {
      Your_Card_Result$Result[i] <- 5.00
    } else if (any(Your_Card$Rank == '5')) {
      Your_Card_Result$Result[i] <- 4.00
    } else if (any(Your_Card$Rank == '4')) {
      Your_Card_Result$Result[i] <- 3.00
    } else if (any(Your_Card$Rank == '3')) {
      Your_Card_Result$Result[i] <- 2.00
    } else {
      Your_Card_Result$Result[i] <- 1.00
    }
  }
  
  Your_Card_Result$Card[i] <- paste(Your_AllCard$Rank, Your_AllCard$Suit, sep = "", collapse = " ")
}

Count_Hand<-as.data.frame(table(Your_Card_Result$Hand))

Count_Hand$Var1 <- reorder(Count_Hand$Var1, -Count_Hand$Freq)

ggplot(aes(Var1,Freq,fill=Var1),data=Count_Hand)+
  geom_bar(stat='identity')+
  xlab('Hand')+
  ylab('Frequency')+
  scale_fill_discrete(name='Hand')

Result <- data.frame('Hand' = Count_Hand$Var1,
                     'Frequency' = Count_Hand$Freq,
                     'Total' = trials,
                     'Probability' = paste0((Count_Hand$Freq / trials)*100,'%')
)

# Define the Desired Order of Hands
Desired_Order <- c('High Card', 'One Pair', 'Two Pairs', 'Three of a Kind', 'Straight', 'Flush', 'Full House', 'Four of a Kind', 'Straight Flush', 'Royal Flush')

# Reorder the Rows of the Result Dataframe Based on the Desired Order
Result <- Result[match(Desired_Order, Result$Hand), ]

#View the Result Table
Result








