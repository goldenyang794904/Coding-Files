Rank <- data.frame('Hand' = c('Royal Flush', 'Straight Flush', 'Four of a Kind', 'Full House', 'Flush', 'Straight', 'Three of a Kind', 'Two Pairs', 'One Pair', 'High Card_A', 'High Card_K', 'High Card_Q', 'High Card_J', 'High Card_10', 'High Card_9', 'High Card_8', 'High Card_7', 'High Card_6', 'High Card_5', 'High Card_4', 'High Card_3', 'High Card_2'),
                   'Rank' = c(22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1))

trials <- 1000

Your_Card_Result <- data.frame('Hand' = character(trials),
                               'Result' = character(trials))

Deck <- data.frame('Rank' = character(52),
                   'Suit' = character(52),
                   'Card' = character(52)
)

Deck$Rank <- rep(c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K'), each = 4)
Deck$Suit <- rep(c('H', 'D', 'S', 'C'), times = 13)
Deck$Card <- paste0(Deck$Rank, Deck$Suit)

# Number of cards per player
Number_of_Cards <- 2

# Your Hand
Your_Card <- data.frame('Card' = sample(Deck$Card, Number_of_Cards, replace = FALSE))

# Adding Rank and Suit columns to Your_Card
Your_Card$Rank <- substr(Your_Card$Card, 1, ifelse(substr(Your_Card$Card, 2, 2) %in% c("H", "D", "S", "C"), 1, 2))
Your_Card$Suit <- substr(Your_Card$Card, ifelse(substr(Your_Card$Card, 2, 2) %in% c("H", "D", "S", "C"), 2, 3), 3)

# Indexing Your Card from the Deck
Index_Your_Card <- match(Your_Card$Card, Deck$Card)

# Delete Your Card from the Deck
Deck <- Deck[-Index_Your_Card, ]

# Opponent1's Hand
Opponent1_Card <- data.frame('Card' = sample(Deck$Card, Number_of_Cards, replace = FALSE))

# Adding Rank and Suit columns to Opponent1_Card
Opponent1_Card$Rank <- substr(Opponent1_Card$Card, 1, ifelse(substr(Opponent1_Card$Card, 2, 2) %in% c("H", "D", "S", "C"), 1, 2))
Opponent1_Card$Suit <- substr(Opponent1_Card$Card, ifelse(substr(Opponent1_Card$Card, 2, 2) %in% c("H", "D", "S", "C"), 2, 3), 3)

# Indexing Opponent1's Card from the Deck
Index_Opponent1_Card <- match(Opponent1_Card$Card, Deck$Card)

# Delete Opponent1's Card from the Deck
Deck <- Deck[-Index_Opponent1_Card, ]

# Opponent2's Hand
Opponent2_Card <- data.frame('Card' = sample(Deck$Card, Number_of_Cards, replace = FALSE))

# Adding Rank and Suit columns to Opponent2_Card
Opponent2_Card$Rank <- substr(Opponent2_Card$Card, 1, ifelse(substr(Opponent2_Card$Card, 2, 2) %in% c("H", "D", "S", "C"), 1, 2))
Opponent2_Card$Suit <- substr(Opponent2_Card$Card, ifelse(substr(Opponent2_Card$Card, 2, 2) %in% c("H", "D", "S", "C"), 2, 3), 3)

# Indexing Opponent2's Card from the Deck
Index_Opponent2_Card <- match(Opponent2_Card$Card, Deck$Card)

# Delete Opponent2's Card from the Deck
Deck <- Deck[-Index_Opponent2_Card, ]

# Opponent3's Hand
Opponent3_Card <- data.frame('Card' = sample(Deck$Card, Number_of_Cards, replace = FALSE))

# Adding Rank and Suit columns to Opponent3_Card
Opponent3_Card$Rank <- substr(Opponent3_Card$Card, 1, ifelse(substr(Opponent3_Card$Card, 2, 2) %in% c("H", "D", "S", "C"), 1, 2))
Opponent3_Card$Suit <- substr(Opponent3_Card$Card, ifelse(substr(Opponent3_Card$Card, 2, 2) %in% c("H", "D", "S", "C"), 2, 3), 3)

# Indexing Opponent3's Card from the Deck
Index_Opponent3_Card <- match(Opponent3_Card$Card, Deck$Card)

# Delete Opponent3's Card from the Deck
Deck <- Deck[-Index_Opponent3_Card, ]

# River
AllRiver <- data.frame('Card' = sample(Deck$Card, 5, replace = FALSE))

# Adding Rank and Suit columns to Opponent3_Card
AllRiver$Rank <- substr(AllRiver$Card, 1, ifelse(substr(AllRiver$Card, 2, 2) %in% c("H", "D", "S", "C"), 1, 2))
AllRiver$Suit <- substr(AllRiver$Card, ifelse(substr(AllRiver$Card, 2, 2) %in% c("H", "D", "S", "C"), 2, 3), 3)

# Combine the Hand with the River
Your_AllCard <- rbind(Your_Card, AllRiver)
Opponent1_AllCard <- rbind(Opponent1_Card, AllRiver)
Opponent2_AllCard <- rbind(Opponent2_Card, AllRiver)
Opponent3_AllCard <- rbind(Opponent3_Card, AllRiver)

#Counting the Number of Rank and Suits
Count_Your_Card_Rank<-table(Your_AllCard$Rank)
Count_Your_Card_Suit<-table(Your_AllCard$Suit)
Pair_Your_Card_Rank<-Count_Your_Card_Rank[Count_Your_Card_Rank==2]

if (sum(Your_AllCard$Rank %in% c('10', 'J', 'Q', 'K', 'A') && Your_AllCard$Suit == 'H') >= 5 ||
    sum(Your_AllCard$Rank %in% c('10', 'J', 'Q', 'K', 'A') && Your_AllCard$Suit == 'D') >= 5 ||
    sum(Your_AllCard$Rank %in% c('10', 'J', 'Q', 'K', 'A') && Your_AllCard$Suit == 'S') >= 5 ||
    sum(Your_AllCard$Rank %in% c('10', 'J', 'Q', 'K', 'A') && Your_AllCard$Suit == 'C') >= 5) {
  Your_Card_Result$Hand <- Rank$Hand[1]
  Your_Card_Result$Result <- Rank$Rank[1]
} else if (any(Count_Your_Card_Rank == 4)) {
  Your_Card_Result$Hand <- Rank$Hand[3]
  Your_Card_Result$Result <- Rank$Rank[3]
} else if (any(Count_Your_Card_Rank == 3)) {
  Your_Card_Result$Hand <- Rank$Hand[7]
  Your_Card_Result$Result <- Rank$Rank[7]
} else if (length(Pair_Your_Card_Rank) >= 2) {
  Your_Card_Result$Hand <- Rank$Hand[8]
  Your_Card_Result$Result <- Rank$Rank[8]
} else if (length(Pair_Your_Card_Rank) == 1) {
  Your_Card_Result$Hand <- Rank$Hand[9]
  Your_Card_Result$Result <- Rank$Rank[9]
} else if (all(Pair_Your_Card_Rank) == 1) {
  Your_Card_Result$Hand <- Rank$Hand[22]
  Your_Card_Result$Result <- Rank$Rank[22]
}
