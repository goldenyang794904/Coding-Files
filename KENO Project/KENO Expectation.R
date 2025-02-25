library(ggplot2)
library(usmap)
library(dplyr)
library(gridExtra)
library(gt)

KENO <- function(spots) {
  # Define the KW and prize vectors based on the number of spots
  if (spots == 12){
    #Base
    prize_CA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_CT <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_DE <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_GA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_IN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_KS <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_KY <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_MD <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_MA <- c(4, 0, 0, 0, 0, 0, 5, 25, 150, 1000, 2500, 25000, 1000000)
    prize_MI <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_MO <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_NH <- c(4, 0, 0, 0, 0, 0, 5, 25, 150, 1000, 2500, 25000, 1000000)
    prize_NJ <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_NY <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_NC <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_OH <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_OR <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_PA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_RI <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_TN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_VA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_WA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_WD <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_WV <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_WY <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    #Bullseye
    prizeBW_CA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_GA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_IN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_KS <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_KY <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_MO <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_NJ <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_OR <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_TN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
  } else if (spots == 11){
    #Base
    prize_CA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_CT <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_DE <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_GA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_IN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_KS <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_KY <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_MD <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_MA <- c(2, 0, 0, 0, 0, 1, 10, 50, 250, 1500, 15000, 500000)
    prize_MI <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_MO <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_NH <- c(2, 0, 0, 0, 0, 1, 10, 50, 250, 1500, 15000, 500000)
    prize_NJ <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_NY <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_NC <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_OH <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_OR <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_PA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_RI <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_TN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_VA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_WA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_WD <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_WV <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_WY <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    #Bullseye
    prizeBW_CA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_GA <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_IN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_KS <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_KY <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_MO <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_NJ <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_OR <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_TN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
  } else if (spots == 10) {
    #Base
    prize_CA <- c(2, 0, 0, 0, 0, 3, 17, 42, 575, 5000, 100000)
    prize_CT <- c(4, 0, 0, 0, 0, 2, 15, 50, 500, 5000, 100000)
    prize_DE <- c(4, 0, 0, 0, 0, 2, 10, 50, 400, 4000, 100000)
    prize_GA <- c(5, 0, 0, 0, 0, 2, 10, 50, 500, 5000, 100000)
    prize_IN <- c(1, 0, 0, 0, 0, 5, 15, 35, 250, 4000, 300000)
    prize_KS <- c(5, 0, 0, 0, 0, 1, 10, 50, 250, 2000, 100000)
    prize_KY <- c(5, 0, 0, 0, 0, 2, 10, 50, 500, 5000, 100000)
    prize_MD <- c(4, 0, 0, 0, 0, 2, 10, 50, 400, 4000, 100000)
    prize_MA <- c(2, 0, 0, 0, 0, 2, 20, 80, 500, 10000, 100000)
    prize_MI <- c(5, 0, 0, 0, 0, 2, 10, 50, 500, 5000, 100000)
    prize_MO <- c(5, 0, 0, 0, 0, 2, 10, 45, 300, 5000, 100000)
    prize_NH <- c(2, 0, 0, 0, 0, 2, 20, 80, 500, 10000, 100000)
    prize_NJ <- c(5, 0, 0, 0, 0, 2, 10, 45, 300, 5000, 100000)
    prize_NY <- c(5, 0, 0, 0, 0, 2, 10, 45, 300, 5000, 100000)
    prize_NC <- c(5, 0, 0, 0, 0, 2, 15, 40, 450, 4250, 100000)
    prize_OH <- c(5, 0, 0, 0, 0, 2, 10, 50, 500, 5000, 100000)
    prize_OR <- c(5, 0, 0, 0, 0, 2, 10, 55, 500, 4500, 200000)
    prize_PA <- c(4, 0, 0, 0, 0, 2, 15, 50, 500, 5000, 100000)
    prize_RI <- c(5, 0, 0, 0, 0, 2, 15, 30, 450, 4250, 100000)
    prize_TN <- c(4, 0, 0, 0, 0, 2, 10, 50, 400, 4000, 100000)
    prize_VA <- c(2, 0, 0, 0, 1, 3, 10, 50, 500, 5000, 100000)
    prize_WA <- c(3, 0, 0, 0, 0, 2, 5, 50, 500, 5000, 100000)
    prize_WD <- c(5, 0, 0, 0, 0, 2, 10, 46, 500, 5000, 100000)
    prize_WV <- c(5, 0, 0, 0, 0, 2, 10, 50, 400, 4000, 100000)
    prize_WY <- c(5, 0, 0, 0, 0, 2, 10, 50, 500, 10000, 200000)
    
    #Bullseye
    prizeBW_CA <- c(0, 5, 2, 2, 2, 9, 60, 215, 1400, 15000, 300000)
    prizeBW_GA <- c(0, 5, 2, 2, 3, 5, 25, 100, 1500, 20000, 200000)
    prizeBW_IN <- c(0, 5, 3, 2, 3, 5, 20, 90, 750, 6000, 200000)
    prizeBW_KS <- c(0, 6, 2, 2, 2, 5, 20, 100, 1000, 10000, 200000)
    prizeBW_KY <- c(0, 5, 2, 2, 4, 6, 20, 175, 1500, 15000, 200000)
    prizeBW_MO <- c(0, 5, 2, 2, 3, 7, 35, 145, 1300, 25000, 300000)
    prizeBW_NJ <- c(0, 5, 2, 2, 3, 4, 15, 105, 1200, 15000, 200000)
    prizeBW_OR <- c(0, 5, 2, 2, 5, 5, 25, 95, 1000, 15500, 300000)
    prizeBW_TN <- c(0, 5, 2, 2, 3, 5, 15, 100, 1100, 11000, 200000)
    

  } else if (spots == 9) {
    #Base
    prize_CA <- c(1, 0, 0, 0, 0, 6, 30, 135, 2750, 30000)
    prize_CT <- c(2, 0, 0, 0, 0, 5, 25, 130, 3000, 30000)
    prize_DE <- c(2, 0, 0, 0, 0, 5, 20, 100, 2500, 25000)
    prize_GA <- c(0, 0, 0, 0, 1, 5, 25, 150, 3000, 30000)
    prize_IN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_KS <- c(0, 0, 0, 0, 1, 5, 20, 100, 2000, 25000)
    prize_KY <- c(0, 0, 0, 0, 2, 5, 20, 100, 2000, 25000)
    prize_MD <- c(2, 0, 0, 0, 0, 5, 20, 100, 2500, 25000)
    prize_MA <- c(0, 0, 0, 0, 1, 5, 25, 200, 4000, 40000)
    prize_MI <- c(0, 0, 0, 0, 2, 5, 20, 100, 2000, 25000)
    prize_MO <- c(0, 0, 0, 0, 1, 5, 20, 125, 3000, 30000)
    prize_NH <- c(0, 0, 0, 0, 1, 5, 25, 200, 4000, 40000)
    prize_NJ <- c(0, 0, 0, 0, 1, 5, 22, 125, 3000, 30000)
    prize_NY <- c(2, 0, 0, 0, 0, 5, 20, 125, 3000, 30000)
    prize_NC <- c(0, 0, 0, 0, 1, 6, 25, 150, 3000, 30000)
    prize_OH <- c(0, 0, 0, 0, 2, 5, 20, 100, 2000, 25000)
    prize_OR <- c(0, 0, 0, 0, 1, 4, 25, 215, 3000, 50000)
    prize_PA <- c(2, 0, 0, 0, 0, 5, 25, 130, 3000, 30000)
    prize_RI <- c(0, 0, 0, 0, 1, 6, 25, 130, 3000, 30000)
    prize_TN <- c(0, 0, 0, 0, 1, 5, 20, 125, 2500, 25000)
    prize_VA <- c(1, 0, 0, 0, 2, 5, 20, 100, 2000, 50000)
    prize_WA <- c(0, 0, 0, 0, 1, 5, 10, 100, 2500, 25000)
    prize_WD <- c(2, 0, 0, 0, 1, 5, 15, 100, 2000, 20000)
    prize_WV <- c(0, 0, 0, 0, 1, 4, 25, 200, 2500, 25000)
    prize_WY <- c(0, 0, 0, 0, 1, 5, 20, 200, 3500, 75000)
    
    #Bullseye
    prizeBW_CA <- c(0, 5, 2, 2, 5, 20, 90, 475, 6000, 65000)
    prizeBW_GA <- c(0, 5, 2, 2, 4, 15, 45, 350, 4000, 50000)
    prizeBW_IN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_KS <- c(0, 6, 2, 2, 3, 6, 40, 500, 2000, 50000)
    prizeBW_KY <- c(0, 5, 2, 2, 4, 15, 45, 400, 6000, 55000)
    prizeBW_MO <- c(0, 5, 2, 2, 6, 15, 60, 525, 8000, 80000)
    prizeBW_NJ <- c(0, 5, 2, 2, 4, 15, 38, 175, 2000, 40000)
    prizeBW_OR <- c(0, 5, 2, 2, 5, 11, 50, 345, 5000, 100000)
    prizeBW_TN <- c(0, 5, 2, 2, 4, 11, 30, 275, 3500, 55000)

  } else if (spots == 8) {
    #Base
    prize_CA <- c(1, 0, 0, 0, 0, 12, 80, 550, 10000)
    prize_CT <- c(0, 0, 0, 0, 2, 10, 75, 500, 10000)
    prize_DE <- c(0, 0, 0, 0, 2, 10, 50, 500, 10000)
    prize_GA <- c(0, 0, 0, 0, 2, 10, 75, 500, 10000)
    prize_IN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
    prize_KS <- c(0, 0, 0, 0, 2, 10, 50, 250, 10000)
    prize_KY <- c(0, 0, 0, 0, 2, 15, 50, 300, 10000)
    prize_MD <- c(0, 0, 0, 0, 2, 10, 50, 500, 10000)
    prize_MA <- c(0, 0, 0, 0, 2, 10, 50, 1000, 15000)
    prize_MI <- c(0, 0, 0, 0, 2, 15, 50, 300, 10000)
    prize_MO <- c(0, 0, 0, 0, 2, 6, 75, 550, 10000)
    prize_NH <- c(0, 0, 0, 0, 2, 10, 50, 1000, 15000)
    prize_NJ <- c(0, 0, 0, 0, 2, 10, 60, 400, 10000)
    prize_NY <- c(2, 0, 0, 0, 0, 6, 75, 550, 10000)
    prize_NC <- c(0, 0, 0, 0, 2, 12, 50, 750, 10000)
    prize_OH <- c(0, 0, 0, 0, 2, 15, 50, 300, 10000)
    prize_OR <- c(0, 0, 0, 0, 2, 10, 60, 600, 15000)
    prize_PA <- c(0, 0, 0, 0, 2, 10, 70, 500, 10000)
    prize_RI <- c(0, 0, 0, 0, 2, 12, 50, 650, 10500)
    prize_TN <- c(0, 0, 0, 0, 2, 10, 40, 500, 10000)
    prize_VA <- c(1, 0, 0, 0, 2, 15, 50, 250, 10000)
    prize_WA <- c(0, 0, 0, 0, 2, 5, 50, 500, 10000)
    prize_WD <- c(2, 0, 0, 0, 1, 7, 50, 500, 10000)
    prize_WV <- c(0, 0, 0, 0, 2, 10, 50, 500, 10000)
    prize_WY <- c(0, 0, 0, 0, 2, 10, 60, 800, 15000)
    
    #Bullseye
    prizeBW_CA <- c(0, 5, 2, 2, 7, 50, 200, 1300, 30000)
    prizeBW_GA <- c(0, 5, 2, 2, 8, 20, 125, 900, 40000)
    prizeBW_IN <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_KS <- c(0, 5, 2, 2, 5, 30, 50, 1000, 20000)
    prizeBW_KY <- c(0, 5, 2, 2, 8, 20, 150, 1200, 40000)
    prizeBW_MO <- c(0, 5, 2, 2, 7, 26, 200, 1800, 50000)
    prizeBW_NJ <- c(0, 5, 2, 2, 6, 20, 140, 800, 15000)
    prizeBW_OR <- c(0, 5, 2, 2, 5, 27, 140, 1400, 40000)
    prizeBW_TN <- c(0, 5, 2, 2, 7, 15, 85, 700, 40000)
    
  } else if (spots == 7) {
    #Base
    prize_CA <- c(0, 0, 0, 1, 3, 12, 190, 2000)
    prize_CT <- c(0, 0, 0, 1, 2, 20, 100, 5000)
    prize_DE <- c(0, 0, 0, 1, 3, 15, 100, 2500)
    prize_GA <- c(0, 0, 0, 1, 3, 15, 125, 4000)
    prize_IN <- c(0, 0, 0, 0, 0, 0, 0, 0)
    prize_KS <- c(0, 0, 0, 1, 3, 10, 100, 2000)
    prize_KY <- c(0, 0, 0, 1, 5, 11, 100, 2000)
    prize_MD <- c(0, 0, 0, 1, 3, 15, 100, 2500)
    prize_MA <- c(0, 0, 0, 1, 3, 20, 100, 5000)
    prize_MI <- c(0, 0, 0, 1, 5, 11, 100, 2000)
    prize_MO <- c(0, 0, 0, 1, 2, 15, 100, 5000)
    prize_NH <- c(0, 0, 0, 1, 3, 20, 100, 5000)
    prize_NJ <- c(0, 0, 0, 1, 3, 15, 100, 2500)
    prize_NY <- c(1, 0, 0, 0, 2, 20, 100, 5000)
    prize_NC <- c(0, 0, 0, 1, 3, 17, 100, 4500)
    prize_OH <- c(0, 0, 0, 1, 5, 11, 100, 2000)
    prize_OR <- c(0, 0, 0, 1, 2, 15, 150, 5500)
    prize_PA <- c(0, 0, 0, 1, 2, 20, 100, 5000)
    prize_RI <- c(0, 0, 0, 1, 3, 17, 85, 4500)
    prize_TN <- c(0, 0, 0, 1, 3, 12, 100, 2500)
    prize_VA <- c(1, 0, 0, 1, 3, 15, 100, 2500)
    prize_WA <- c(0, 0, 0, 1, 2, 10, 100, 2500)
    prize_WD <- c(1, 0, 0, 0, 2, 20, 150, 5000)
    prize_WV <- c(0, 0, 0, 1, 2, 15, 125, 5000)
    prize_WY <- c(0, 0, 0, 1, 2, 16, 100, 8000)
    
    #Bullseye
    prizeBW_CA <- c(0, 5, 2, 3, 20, 68, 575, 10000)
    prizeBW_GA <- c(0, 5, 2, 4, 12, 65, 275, 6000)
    prizeBW_IN <- c(0, 0, 0, 0, 0, 0, 0, 0)
    prizeBW_KS <- c(0, 5, 2, 4, 10, 50, 300, 5000)
    prizeBW_KY <- c(0, 4, 2, 4, 10, 74, 400, 8000)
    prizeBW_MO <- c(0, 5, 2, 4, 12, 75, 500, 12500)
    prizeBW_NJ <- c(0, 5, 2, 4, 11, 45, 200, 5500)
    prizeBW_OR <- c(0, 5, 2, 3, 10, 60, 400, 12000)
    prizeBW_TN <- c(0, 5, 2, 4, 9, 48, 200, 7500)
    
  } else if (spots == 6) {
    #Base
    prize_CA <- c(0, 0, 0, 1, 6, 67, 900)
    prize_CT <- c(0, 0, 0, 1, 7, 50, 1300)
    prize_DE <- c(0, 0, 0, 1, 5, 50, 1000)
    prize_GA <- c(0, 0, 0, 1, 7, 50, 1200)
    prize_IN <- c(0, 0, 0, 0, 0, 0, 0)
    prize_KS <- c(0, 0, 0, 1, 5, 50, 1000)
    prize_KY <- c(0, 0, 0, 1, 7, 57, 1100)
    prize_MD <- c(0, 0, 0, 1, 5, 50, 1000)
    prize_MA <- c(0, 0, 0, 1, 7, 50, 1600)
    prize_MI <- c(0, 0, 0, 1, 7, 57, 1100)
    prize_MO <- c(0, 0, 0, 1, 6, 55, 1000)
    prize_NH <- c(0, 0, 0, 1, 7, 50, 1600)
    prize_NJ <- c(0, 0, 0, 1, 7, 45, 1000)
    prize_NY <- c(0, 0, 0, 1, 6, 55, 1000)
    prize_NC <- c(0, 0, 0, 1, 8, 50, 1100)
    prize_OH <- c(0, 0, 0, 1, 7, 57, 1100)
    prize_OR <- c(0, 0, 0, 1, 5, 55, 1600)
    prize_PA <- c(0, 0, 0, 1, 7, 50, 1200)
    prize_RI <- c(0, 0, 0, 1, 7, 55, 1200)
    prize_TN <- c(0, 0, 0, 1, 5, 50, 1100)
    prize_VA <- c(1, 0, 0, 1, 5, 50, 1000)
    prize_WA <- c(0, 0, 0, 1, 4, 40, 1000)
    prize_WD <- c(0, 0, 0, 1, 5, 53, 1500)
    prize_WV <- c(0, 0, 0, 1, 5, 50, 1500)
    prize_WY <- c(0, 0, 0, 1, 5, 50, 2000)
    
    #Bullseye
    prizeBW_CA <- c(0, 5, 2, 7, 40, 250, 2000)
    prizeBW_GA <- c(0, 5, 3, 9, 23, 90, 1800)
    prizeBW_IN <- c(0, 0, 0, 0, 0, 0, 0)
    prizeBW_KS <- c(0, 5, 3, 5, 20, 100, 2000)
    prizeBW_KY <- c(0, 4, 4, 9, 23, 93, 1900)
    prizeBW_MO <- c(0, 5, 3, 6, 31, 155, 3500)
    prizeBW_NJ <- c(0, 5, 3, 7, 18, 85, 1500)
    prizeBW_OR <- c(0, 4, 3, 9, 20, 95, 3200)
    prizeBW_TN <- c(0, 5, 3, 7, 20, 60, 1900)
    
  } else if (spots == 5) {
    #Base
    prize_CA <- c(0, 0, 0, 2, 15, 435)
    prize_CT <- c(0, 0, 0, 2, 16, 450)
    prize_DE <- c(0, 0, 0, 2, 15, 300)
    prize_GA <- c(0, 0, 0, 2, 17, 400)
    prize_IN <- c(0, 0, 0, 0, 0, 0)
    prize_KS <- c(0, 0, 0, 2, 10, 400)
    prize_KY <- c(0, 0, 0, 2, 18, 410)
    prize_MD <- c(0, 0, 0, 2, 15, 300)
    prize_MA <- c(0, 0, 0, 2, 20, 450)
    prize_MI <- c(0, 0, 0, 2, 18, 410)
    prize_MO <- c(0, 0, 0, 2, 20, 330)
    prize_NH <- c(0, 0, 0, 2, 20, 450)
    prize_NJ <- c(0, 0, 0, 2, 20, 300)
    prize_NY <- c(0, 0, 0, 2, 20, 300)
    prize_NC <- c(0, 0, 0, 2, 18, 420)
    prize_OH <- c(0, 0, 0, 2, 18, 410)
    prize_OR <- c(0, 0, 0, 2, 15, 465)
    prize_PA <- c(0, 0, 0, 2, 15, 450)
    prize_RI <- c(0, 0, 0, 2, 18, 410)
    prize_TN <- c(0, 0, 0, 2, 15, 350)
    prize_VA <- c(0, 0, 0, 2, 20, 450)
    prize_WA <- c(0, 0, 0, 2, 17, 200)
    prize_WD <- c(0, 0, 0, 3, 10, 400)
    prize_WV <- c(0, 0, 0, 2, 15, 400)
    prize_WY <- c(0, 0, 0, 2, 16, 500)
    
    #Bullseye
    prizeBW_CA <- c(0, 3, 6, 16, 80, 1000)
    prizeBW_GA <- c(0, 5, 5, 13, 53, 600)
    prizeBW_IN <- c(0, 0, 0, 0, 0, 0)
    prizeBW_KS <- c(0, 5, 4, 10, 50, 500)
    prizeBW_KY <- c(0, 4, 6, 13, 62, 590)
    prizeBW_MO <- c(0, 5, 4, 12, 80, 930)
    prizeBW_NJ <- c(0, 5, 5, 13, 30, 450)
    prizeBW_OR <- c(0, 4, 4, 10, 70, 1035)
    prizeBW_TN <- c(0, 5, 5, 10, 35, 650)
    
  } else if (spots == 4) {
    #Base
    prize_CA <- c(0, 0, 1, 4, 78)
    prize_CT <- c(0, 0, 1, 3, 100)
    prize_DE <- c(0, 0, 1, 5, 50)
    prize_GA <- c(0, 0, 1, 5, 70)
    prize_IN <- c(0, 0, 0, 0, 0)
    prize_KS <- c(0, 0, 1, 4, 50)
    prize_KY <- c(0, 0, 1, 5, 72)
    prize_MD <- c(0, 0, 1, 5, 50)
    prize_MA <- c(0, 0, 1, 4, 100)
    prize_MI <- c(0, 0, 1, 5, 72)
    prize_MO <- c(0, 0, 1, 5, 60)
    prize_NH <- c(0, 0, 1, 4, 100)
    prize_NJ <- c(0, 0, 1, 5, 55)
    prize_NY <- c(0, 0, 1, 5, 55)
    prize_NC <- c(0, 0, 1, 5, 75)
    prize_OH <- c(0, 0, 1, 5, 72)
    prize_OR <- c(0, 0, 1, 5, 72)
    prize_PA <- c(0, 0, 1, 3, 100)
    prize_RI <- c(0, 0, 1, 5, 72)
    prize_TN <- c(0, 0, 1, 5, 45)
    prize_VA <- c(0, 0, 1, 5, 100)
    prize_WA <- c(0, 0, 1, 5, 24)
    prize_WD <- c(0, 0, 1, 5, 65)
    prize_WV <- c(0, 0, 1, 5, 50)
    prize_WY <- c(0, 0, 1, 5, 80)
    
    #Bullseye
    prizeBW_CA <- c(0, 4, 11, 37, 300)
    prizeBW_GA <- c(0, 5, 11, 20, 280)
    prizeBW_IN <- c(0, 0, 0, 0, 0)
    prizeBW_KS <- c(0, 7, 7, 20, 200)
    prizeBW_KY <- c(0, 6, 9, 27, 253)
    prizeBW_MO <- c(0, 5, 11, 25, 300)
    prizeBW_NJ <- c(0, 5, 9, 15, 245)
    prizeBW_OR <- c(0, 5, 10, 20, 318)
    prizeBW_TN <- c(0, 5, 9, 10, 305)
    
  } else if (spots == 3) {
    #Base
    prize_CA <- c(0, 0, 2, 25)
    prize_CT <- c(0, 0, 2, 27)
    prize_DE <- c(0, 0, 2, 25)
    prize_GA <- c(0, 0, 2, 25)
    prize_IN <- c(0, 0, 0, 0)
    prize_KS <- c(0, 0, 2, 20)
    prize_KY <- c(0, 0, 2, 27)
    prize_MD <- c(0, 0, 2, 25)
    prize_MA <- c(0, 0, 2.5, 25)
    prize_MI <- c(0, 0, 2, 27)
    prize_MO <- c(0, 0, 2, 25)
    prize_NH <- c(0, 0, 2, 30)
    prize_NJ <- c(0, 0, 2, 23)
    prize_NY <- c(0, 0, 2, 23)
    prize_NC <- c(0, 0, 2, 27)
    prize_OH <- c(0, 0, 2, 27)
    prize_OR <- c(0, 0, 2, 27)
    prize_PA <- c(0, 0, 2, 25)
    prize_RI <- c(0, 0, 2, 27)
    prize_TN <- c(0, 0, 2, 20)
    prize_VA <- c(0, 0, 2, 30)
    prize_WA <- c(0, 0, 2, 16)
    prize_WD <- c(0, 0, 2, 0)
    prize_WV <- c(0, 0, 2, 25)
    prize_WY <- c(0, 0, 2.5, 24)
    
    #Bullseye
    prizeBW_CA <- c(0, 6, 20, 155)
    prizeBW_GA <- c(0, 8, 18, 100)
    prizeBW_IN <- c(0, 0, 0, 0)
    prizeBW_KS <- c(0, 9, 10, 100)
    prizeBW_KY <- c(0, 10, 16, 98)
    prizeBW_MO <- c(0, 8, 17, 125)
    prizeBW_NJ <- c(0, 8, 16, 77)
    prizeBW_OR <- c(0, 8, 15, 128)
    prizeBW_TN <- c(0, 7, 13, 105)

  } else if (spots == 2) {
    #Base
    prize_CA <- c(0, 0, 10)
    prize_CT <- c(0, 0, 11)
    prize_DE <- c(0, 0, 10)
    prize_GA <- c(0, 0, 10)
    prize_IN <- c(0, 0, 0)
    prize_KS <- c(0, 0, 9)
    prize_KY <- c(0, 0, 11)
    prize_MD <- c(0, 0, 10)
    prize_MA <- c(0, 1, 5)
    prize_MI <- c(0, 0, 11)
    prize_MO <- c(0, 0, 10)
    prize_NH <- c(0, 1, 5)
    prize_NJ <- c(0, 0, 10)
    prize_NY <- c(0, 0, 10)
    prize_NC <- c(0, 0, 11)
    prize_OH <- c(0, 0, 11)
    prize_OR <- c(0, 0, 11)
    prize_PA <- c(0, 0, 10)
    prize_RI <- c(0, 0, 11)
    prize_TN <- c(0, 0, 10)
    prize_VA <- c(0, 0, 12)
    prize_WA <- c(0, 0, 8)
    prize_WD <- c(0, 0, 10)
    prize_WV <- c(0, 0, 10)
    prize_WY <- c(0, 1, 5)
    
    #Bullseye
    prizeBW_CA <- c(0, 15, 70)
    prizeBW_GA <- c(0, 15, 55)
    prizeBW_IN <- c(0, 0, 0)
    prizeBW_KS <- c(0, 10, 55)
    prizeBW_KY <- c(0, 15, 59)
    prizeBW_MO <- c(0, 15, 62)
    prizeBW_NJ <- c(0, 15, 45)
    prizeBW_OR <- c(0, 15, 60)
    prizeBW_TN <- c(0, 15, 45)

  } else if (spots == 1) {
    #Base
    prize_CA <- c(0, 2)
    prize_CT <- c(0, 2.5)
    prize_DE <- c(0, 2)
    prize_GA <- c(0, 2)
    prize_IN <- c(0, 0)
    prize_KS <- c(0, 2)
    prize_KY <- c(0, 2)
    prize_MD <- c(0, 2)
    prize_MA <- c(0, 2.5)
    prize_MI <- c(0, 2)
    prize_MO <- c(0, 2)
    prize_NH <- c(0, 2)
    prize_NJ <- c(0, 2)
    prize_NY <- c(0, 2)
    prize_NC <- c(0, 2)
    prize_OH <- c(0, 2)
    prize_OR <- c(0, 2.5)
    prize_PA <- c(0, 2.5)
    prize_RI <- c(0, 2.5)
    prize_TN <- c(0, 2)
    prize_VA <- c(0, 3)
    prize_WA <- c(0, 2)
    prize_WD <- c(0, 2.5)
    prize_WV <- c(0, 2)
    prize_WY <- c(0, 2.5)
    
    #Bullseye
    prizeBW_CA <- c(0, 54)
    prizeBW_GA <- c(0, 48)
    prizeBW_IN <- c(0, 0)
    prizeBW_KS <- c(0, 43)
    prizeBW_KY <- c(0, 48)
    prizeBW_MO <- c(0, 50)
    prizeBW_NJ <- c(0, 44)
    prizeBW_OR <- c(0, 49.5)
    prizeBW_TN <- c(0, 43)
    
  }
  k <- 0:spots
  
  #Base
  expected_values_CA <- numeric(length(k))
  expected_values_CT <- numeric(length(k))
  expected_values_DE <- numeric(length(k))
  expected_values_GA <- numeric(length(k))
  expected_values_IN <- numeric(length(k))
  expected_values_KS <- numeric(length(k))
  expected_values_KY <- numeric(length(k))
  expected_values_MD <- numeric(length(k))
  expected_values_MA <- numeric(length(k))
  expected_values_MI <- numeric(length(k))
  expected_values_MO <- numeric(length(k))
  expected_values_NH <- numeric(length(k))
  expected_values_NJ <- numeric(length(k))
  expected_values_NY <- numeric(length(k))
  expected_values_NC <- numeric(length(k))
  expected_values_OH <- numeric(length(k))
  expected_values_OR <- numeric(length(k))
  expected_values_PA <- numeric(length(k))
  expected_values_RI <- numeric(length(k))
  expected_values_TN <- numeric(length(k))
  expected_values_VA <- numeric(length(k))
  expected_values_WA <- numeric(length(k))
  expected_values_WD <- numeric(length(k))
  expected_values_WV <- numeric(length(k))
  expected_values_WY <- numeric(length(k))
  
  for (i in seq_along(k)) {
    expected_values_CA[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_CA[i]
    expected_values_CT[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_CT[i]
    expected_values_DE[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_DE[i]
    expected_values_GA[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_GA[i]
    expected_values_IN[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_IN[i]
    expected_values_KS[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_KS[i]
    expected_values_KY[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_KY[i]
    expected_values_MD[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_MD[i]
    expected_values_MA[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_MA[i]
    expected_values_MI[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_MI[i]
    expected_values_MO[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_MO[i]
    expected_values_NH[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_NH[i]
    expected_values_NJ[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_NJ[i]
    expected_values_NY[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_NY[i]
    expected_values_NC[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_NC[i]
    expected_values_OH[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_OH[i]
    expected_values_OR[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_OR[i]
    expected_values_PA[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_PA[i]
    expected_values_RI[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_RI[i]
    expected_values_TN[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_TN[i]
    expected_values_VA[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_VA[i]
    expected_values_WA[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_WA[i]
    expected_values_WD[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_WD[i]
    expected_values_WV[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_WV[i]
    expected_values_WY[i] <- (choose(20, k[i]) * choose(60, spots - k[i])) / choose(80, spots) * prize_WY[i]
  }
  
  expected_value_CA <- sum(expected_values_CA)
  expected_value_CT <- sum(expected_values_CT)
  expected_value_DE <- sum(expected_values_DE)
  expected_value_GA <- sum(expected_values_GA)
  expected_value_IN <- sum(expected_values_IN)
  expected_value_KS <- sum(expected_values_KS)
  expected_value_KY <- sum(expected_values_KY)
  expected_value_MD <- sum(expected_values_MD)
  expected_value_MA <- sum(expected_values_MA)
  expected_value_MI <- sum(expected_values_MI)
  expected_value_MO <- sum(expected_values_MO)
  expected_value_NH <- sum(expected_values_NH)
  expected_value_NJ <- sum(expected_values_NJ)
  expected_value_NY <- sum(expected_values_NY)
  expected_value_NC <- sum(expected_values_NC)
  expected_value_OH <- sum(expected_values_OH)
  expected_value_OR <- sum(expected_values_OR)
  expected_value_PA <- sum(expected_values_PA)
  expected_value_RI <- sum(expected_values_RI)
  expected_value_TN <- sum(expected_values_TN)
  expected_value_VA <- sum(expected_values_VA)
  expected_value_WA <- sum(expected_values_WA)
  expected_value_WD <- sum(expected_values_WD)
  expected_value_WV <- sum(expected_values_WV)
  expected_value_WY <- sum(expected_values_WY)
  
  #Bullseye
  expected_valuesBE_CA <- numeric(length(k))
  expected_valuesBE_GA <- numeric(length(k))
  expected_valuesBE_IN <- numeric(length(k))
  expected_valuesBE_KS <- numeric(length(k))
  expected_valuesBE_KY <- numeric(length(k))
  expected_valuesBE_MO <- numeric(length(k))
  expected_valuesBE_NJ <- numeric(length(k))
  expected_valuesBE_OR <- numeric(length(k))
  expected_valuesBE_TN <- numeric(length(k))
  
  for (i in seq_along(k)){
    expected_valuesBE_CA[i]<-prize_CA[i]*((choose(1,0)*choose(19,k[i])*choose(60,spots-k[i]))/choose(80,spots)) + prizeBW_CA[i] * ((choose(1,1)*choose(19,k[i]-1)*choose(60,spots-k[i]))/choose(80,spots))
    expected_valuesBE_GA[i]<-prize_GA[i]*((choose(1,0)*choose(19,k[i])*choose(60,spots-k[i]))/choose(80,spots)) + prizeBW_GA[i] * ((choose(1,1)*choose(19,k[i]-1)*choose(60,spots-k[i]))/choose(80,spots))
    expected_valuesBE_IN[i]<-prize_IN[i]*((choose(1,0)*choose(19,k[i])*choose(60,spots-k[i]))/choose(80,spots)) + prizeBW_IN[i] * ((choose(1,1)*choose(19,k[i]-1)*choose(60,spots-k[i]))/choose(80,spots))
    expected_valuesBE_KS[i]<-prize_KS[i]*((choose(1,0)*choose(19,k[i])*choose(60,spots-k[i]))/choose(80,spots)) + prizeBW_KS[i] * ((choose(1,1)*choose(19,k[i]-1)*choose(60,spots-k[i]))/choose(80,spots))
    expected_valuesBE_KY[i]<-prize_KY[i]*((choose(1,0)*choose(19,k[i])*choose(60,spots-k[i]))/choose(80,spots)) + prizeBW_KY[i] * ((choose(1,1)*choose(19,k[i]-1)*choose(60,spots-k[i]))/choose(80,spots))
    expected_valuesBE_MO[i]<-prize_MO[i]*((choose(1,0)*choose(19,k[i])*choose(60,spots-k[i]))/choose(80,spots)) + prizeBW_MO[i] * ((choose(1,1)*choose(19,k[i]-1)*choose(60,spots-k[i]))/choose(80,spots))
    expected_valuesBE_NJ[i]<-prize_NJ[i]*((choose(1,0)*choose(19,k[i])*choose(60,spots-k[i]))/choose(80,spots)) + prizeBW_NJ[i] * ((choose(1,1)*choose(19,k[i]-1)*choose(60,spots-k[i]))/choose(80,spots))
    expected_valuesBE_OR[i]<-prize_OR[i]*((choose(1,0)*choose(19,k[i])*choose(60,spots-k[i]))/choose(80,spots)) + prizeBW_OR[i] * ((choose(1,1)*choose(19,k[i]-1)*choose(60,spots-k[i]))/choose(80,spots))
    expected_valuesBE_TN[i]<-prize_TN[i]*((choose(1,0)*choose(19,k[i])*choose(60,spots-k[i]))/choose(80,spots)) + prizeBW_TN[i] * ((choose(1,1)*choose(19,k[i]-1)*choose(60,spots-k[i]))/choose(80,spots))
  }
  
  expected_valueBE_CA<-sum(expected_valuesBE_CA)
  expected_valueBE_GA<-sum(expected_valuesBE_GA)
  expected_valueBE_IN<-sum(expected_valuesBE_IN)
  expected_valueBE_KS<-sum(expected_valuesBE_KS)
  expected_valueBE_KY<-sum(expected_valuesBE_KY)
  expected_valueBE_MO<-sum(expected_valuesBE_MO)
  expected_valueBE_NJ<-sum(expected_valuesBE_NJ)
  expected_valueBE_OR<-sum(expected_valuesBE_OR)
  expected_valueBE_TN<-sum(expected_valuesBE_TN)
  
  return(list(#Base
              Expected_Value_CA = expected_value_CA,
              Expected_Value_CT = expected_value_CT,
              Expected_Value_DE = expected_value_DE,
              Expected_Value_GA = expected_value_GA,
              Expected_Value_IN = expected_value_IN,
              Expected_Value_KS = expected_value_KS,
              Expected_Value_KY = expected_value_KY,
              Expected_Value_MD = expected_value_MD,
              Expected_Value_MA = expected_value_MA,
              Expected_Value_MI = expected_value_MI,
              Expected_Value_MO = expected_value_MO,
              Expected_Value_NH = expected_value_NH,
              Expected_Value_NJ = expected_value_NJ,
              Expected_Value_NY = expected_value_NY,
              Expected_Value_NC = expected_value_NC,
              Expected_Value_OH = expected_value_OH,
              Expected_Value_OR = expected_value_OR,
              Expected_Value_PA = expected_value_PA,
              Expected_Value_RI = expected_value_RI,
              Expected_Value_TN = expected_value_TN,
              Expected_Value_VA = expected_value_VA,
              Expected_Value_WA = expected_value_WA,
              Expected_Value_WD = expected_value_WD,
              Expected_Value_WV = expected_value_WV,
              Expected_Value_WY = expected_value_WY,
              
              #Bullseye
              Expected_ValueBE_CA = expected_valueBE_CA,
              Expected_ValueBE_GA = expected_valueBE_GA,
              Expected_ValueBE_IN = expected_valueBE_IN,
              Expected_ValueBE_KS = expected_valueBE_KS,
              Expected_ValueBE_KY = expected_valueBE_KY,
              Expected_ValueBE_MO = expected_valueBE_MO,
              Expected_ValueBE_NJ = expected_valueBE_NJ,
              Expected_ValueBE_OR = expected_valueBE_OR,
              Expected_ValueBE_TN = expected_valueBE_TN
              ))
}

#KENO result from each state
#California
result_california_base<-numeric(12)
result_california_bullseye<-numeric(12)
#Connecticut
result_connecticut_base<-numeric(12)
#Delaware
result_delaware_base<-numeric(12)
#Georgia
result_georgia_base<-numeric(12)
result_georgia_bullseye<-numeric(12)
#Indiana
result_indiana_base<-numeric(12)
result_indiana_bullseye<-numeric(12)
#Kansas
result_kansas_base<-numeric(12)
result_kansas_bullseye<-numeric(12)
#Kentucky
result_kentucky_base<-numeric(12)
result_kentucky_bullseye<-numeric(12)
#Maryland
result_maryland_base<-numeric(12)
#Massachusetts
result_massachusetts_base<-numeric(12)
#Michigan
result_michigan_base<-numeric(12)
#Missouri
result_missouri_base<-numeric(12)
result_missouri_bullseye<-numeric(12)
#New Hampshire
result_newhampshire_base<-numeric(12)
#New Jersey
result_newjersey_base<-numeric(12)
result_newjersey_bullseye<-numeric(12)
#New York
result_newyork_base<-numeric(12)
#North Carolina
result_northcarolina_base<-numeric(12)
#Ohio
result_ohio_base<-numeric(12)
#Oregon
result_oregon_base<-numeric(12)
result_oregon_bullseye<-numeric(12)
#Pennsylvania
result_pennsylvania_base<-numeric(12)
#Rhode Island
result_rhodeisland_base<-numeric(12)
#Tennessee
result_tennessee_base<-numeric(12)
result_tennessee_bullseye<-numeric(12)
#Virginia
result_virginia_base<-numeric(12)
#Washington
result_washington_base<-numeric(12)
#Washington DC
result_washingtondc_base<-numeric(12)
#West Virginia
result_westvirginia_base<-numeric(12)
#Wyoming
result_wyoming_base<-numeric(12)

for (i in 1:12) {
  results<-KENO(i)
  #California
  result_california_base[i] <- results$Expected_Value_CA
  result_california_bullseye[i] <- results$Expected_ValueBE_CA
  #Connecticut
  result_connecticut_base[i] <- results$Expected_Value_CT
  #Delaware
  result_delaware_base[i] <- results$Expected_Value_DE
  #Georgia
  result_georgia_base[i] <- results$Expected_Value_GA
  result_georgia_bullseye[i] <- results$Expected_ValueBE_GA
  #Indiana
  result_indiana_base[i] <- results$Expected_Value_IN
  result_indiana_bullseye[i] <- results$Expected_ValueBE_IN
  #Kansas
  result_kansas_base[i] <- results$Expected_Value_KS
  result_kansas_bullseye[i] <- results$Expected_ValueBE_KS
  #Kentucky
  result_kentucky_base[i] <- results$Expected_Value_KY
  result_kentucky_bullseye[i] <- results$Expected_ValueBE_KY
  #Maryland
  result_maryland_base[i] <- results$Expected_Value_MD
  #Massachusetts
  result_massachusetts_base[i] <- results$Expected_Value_MA
  #Michigan
  result_michigan_base[i] <- results$Expected_Value_MI
  #Missouri
  result_missouri_base[i] <- results$Expected_Value_MO
  result_missouri_bullseye[i] <- results$Expected_ValueBE_MO
  #New Hampshire
  result_newhampshire_base[i] <- results$Expected_Value_NH
  #New Jersey
  result_newjersey_base[i] <-results$Expected_Value_NJ
  result_newjersey_bullseye[i] <-results$Expected_ValueBE_NJ
  #New York
  result_newyork_base[i] <-results$Expected_Value_NY
  #North Carolina
  result_northcarolina_base[i] <-results$Expected_Value_NC
  #Ohio
  result_ohio_base[i] <- results$Expected_Value_OH
  #Oregon
  result_oregon_base[i] <- results$Expected_Value_OR
  result_oregon_bullseye[i] <- results$Expected_ValueBE_OR
  #Pennsylvania
  result_pennsylvania_base[i] <-results$Expected_Value_PA
  #Rhode Island
  result_rhodeisland_base[i] <-results$Expected_Value_RI
  #Tennessee
  result_tennessee_base[i] <- results$Expected_Value_TN
  result_tennessee_bullseye[i] <- results$Expected_ValueBE_TN
  #Virginia
  result_virginia_base[i] <-results$Expected_Value_VA
  #Washington
  result_washington_base[i] <-results$Expected_Value_WA
  #Washington DC
  result_washingtondc_base[i] <-results$Expected_Value_WD
  #West Virginia
  result_westvirginia_base[i] <-results$Expected_Value_WV
  #Wyoming
  result_wyoming_base[i] <-results$Expected_Value_WY
}


#California
base_keno_california<-max(as.numeric(result_california_base))
base_keno_california_index<-which.max(as.numeric(result_california_base))

bullseye_keno_california<-max(as.numeric(result_california_bullseye))
bullseye_keno_california_index<-which.max(as.numeric(result_california_bullseye))
#Connecticut
base_keno_conneticut<-max(as.numeric(result_connecticut_base))
base_keno_conneticut_index<-which.max(as.numeric(result_connecticut_base))
#Delaware
base_keno_delaware<-max(as.numeric(result_delaware_base))
base_keno_delaware_index<-which.max(as.numeric(result_delaware_base))
#Georgia
base_keno_georgia<-max(as.numeric(result_georgia_base))
base_keno_georgia_index<-which.max(as.numeric(result_georgia_base))

bullseye_keno_georgia<-max(as.numeric(result_georgia_bullseye))
bullseye_keno_georgia_index<-which.max(as.numeric(result_georgia_bullseye))
#Indiana
base_keno_indiana<-max(as.numeric(result_indiana_base))
base_keno_indiana_index<-which.max(as.numeric(result_indiana_base))

bullseye_keno_indiana<-max(as.numeric(result_indiana_bullseye))
bullseye_keno_indiana_index<-which.max(as.numeric(result_indiana_bullseye))
#Kansas
base_keno_kansas<-max(as.numeric(result_kansas_base))
base_keno_kansas_index<-which.max(as.numeric(result_kansas_base))

bullseye_keno_kansas<-max(as.numeric(result_kansas_bullseye))
bullseye_keno_kansas_index<-which.max(as.numeric(result_kansas_bullseye))
#Kentucky
base_keno_kentucky<-max(as.numeric(result_kentucky_base))
base_keno_kentucky_index<-which.max(as.numeric(result_kentucky_base))

bullseye_keno_kentucky<-max(as.numeric(result_kentucky_bullseye))
bullseye_keno_kentucky_index<-which.max(as.numeric(result_kentucky_bullseye))
#Maryland
base_keno_maryland<-max(as.numeric(result_maryland_base))
base_keno_maryland_index<-which.max(as.numeric(result_maryland_base))
#Massachusetts
base_keno_massachusetts<-max(as.numeric(result_massachusetts_base))
base_keno_massachusetts_index<-which.max(as.numeric(result_massachusetts_base))
#Michigan
base_keno_michigan<-max(as.numeric(result_michigan_base))
base_keno_michigan_index<-which.max(as.numeric(result_michigan_base))
#Missouri
base_keno_missouri<-max(as.numeric(result_missouri_base))
base_keno_missouri_index<-which.max(as.numeric(result_missouri_base))

bullseye_keno_missouri<-max(as.numeric(result_missouri_bullseye))
bullseye_keno_missouri_index<-which.max(as.numeric(result_missouri_bullseye))
#New Hampshire
base_keno_newhampshire<-max(as.numeric(result_newhampshire_base))
base_keno_newhampshire_index<-which.max(as.numeric(result_newhampshire_base))
#New Jersey
base_keno_newjersey<-max(as.numeric(result_newjersey_base))
base_keno_newjersey_index<-which.max(as.numeric(result_newjersey_base))

bullseye_keno_newjersey<-max(as.numeric(result_newjersey_bullseye))
bullseye_keno_newjersey_index<-which.max(as.numeric(result_newjersey_bullseye))
#New York
base_keno_newyork<-max(as.numeric(result_newyork_base))
base_keno_newyork_index<-which.max(as.numeric(result_newyork_base))
#North Carolina
base_keno_northcarolina<-max(as.numeric(result_northcarolina_base))
base_keno_northcarolina_index<-which.max(as.numeric(result_northcarolina_base))
#Ohio
base_keno_ohio<-max(as.numeric(result_ohio_base))
base_keno_ohio_index<-which.max(as.numeric(result_ohio_base))
#Oregon
base_keno_oregon<-max(as.numeric(result_oregon_base))
base_keno_oregon_index<-which.max(as.numeric(result_oregon_base))

bullseye_keno_oregon<-max(as.numeric(result_oregon_bullseye))
bullseye_keno_oregon_index<-which.max(as.numeric(result_oregon_bullseye))
#Pennsylvania
base_keno_pennsylvania<-max(as.numeric(result_pennsylvania_base))
base_keno_pennsylvania_index<-which.max(as.numeric(result_pennsylvania_base))
#Rhode Island
base_keno_rhodeisland<-max(as.numeric(result_rhodeisland_base))
base_keno_rhodeisland_index<-which.max(as.numeric(result_rhodeisland_base))
#Tennessee
base_keno_tennessee<-max(as.numeric(result_tennessee_base))
base_keno_tennessee_index<-which.max(as.numeric(result_tennessee_base))

bullseye_keno_tennessee<-max(as.numeric(result_tennessee_bullseye))
bullseye_keno_tennessee_index<-which.max(as.numeric(result_tennessee_bullseye))
#Virginia
base_keno_virginia<-max(as.numeric(result_virginia_base))
base_keno_virginia_index<-which.max(as.numeric(result_virginia_base))
#Washington
base_keno_washington<-max(as.numeric(result_washington_base))
base_keno_washington_index<-which.max(as.numeric(result_washington_base))
#Washington DC
base_keno_washingtondc<-max(as.numeric(result_washingtondc_base))
base_keno_washingtondc_index<-which.max(as.numeric(result_washingtondc_base))
#West Virginia
base_keno_westvirginia<-max(as.numeric(result_westvirginia_base))
base_keno_westvirginia_index<-which.max(as.numeric(result_westvirginia_base))
#Wyoming
base_keno_wyoming<-max(as.numeric(result_wyoming_base))
base_keno_wyoming_index<-which.max(as.numeric(result_wyoming_base))

data<-data.frame(state = c("CA", "CT", "DE", "GA", "IN", "KS", "KY", "MD", "MA", "MI", "MO", "NH", "NJ", "NY", "NC", "OH", "OR", "PA", "RI", "TN", "VA", "WA", "DC", "WV", "WY"),
                 Base_Expected = c(base_keno_california, base_keno_conneticut, base_keno_delaware, base_keno_georgia, base_keno_indiana,base_keno_kansas, base_keno_kentucky, base_keno_maryland, base_keno_massachusetts, base_keno_michigan, base_keno_missouri, base_keno_newhampshire, base_keno_newjersey, base_keno_newyork, base_keno_northcarolina, base_keno_ohio, base_keno_oregon,base_keno_pennsylvania, base_keno_rhodeisland, base_keno_tennessee,base_keno_virginia, base_keno_washington, base_keno_washingtondc, base_keno_westvirginia, base_keno_wyoming),
                 Base_Max_Spots = c(base_keno_california_index, base_keno_conneticut_index, base_keno_delaware_index, base_keno_georgia_index, base_keno_indiana_index, base_keno_kansas_index, base_keno_kentucky_index, base_keno_maryland_index, base_keno_massachusetts_index, base_keno_michigan_index, base_keno_missouri_index, base_keno_newhampshire_index, base_keno_newjersey_index, base_keno_newyork_index, base_keno_northcarolina_index, base_keno_ohio_index, base_keno_oregon_index, base_keno_pennsylvania_index, base_keno_rhodeisland_index, base_keno_tennessee_index, base_keno_virginia_index, base_keno_washington_index, base_keno_washingtondc_index, base_keno_westvirginia_index, base_keno_wyoming_index),
                 Bullseye_Expected = c(bullseye_keno_california, NA, NA, bullseye_keno_georgia, bullseye_keno_indiana, bullseye_keno_kansas, bullseye_keno_kentucky, NA, NA, NA, bullseye_keno_missouri, NA, bullseye_keno_newjersey, NA, NA, NA, bullseye_keno_oregon, NA, NA, bullseye_keno_tennessee, NA, NA, NA, NA, NA),
                 Bullseye_Max_Spots = c(base_keno_california_index, NA, NA, bullseye_keno_georgia_index, bullseye_keno_indiana_index, bullseye_keno_kansas_index, bullseye_keno_kentucky_index, NA, NA, NA, bullseye_keno_missouri_index, NA, bullseye_keno_newjersey_index, NA, NA, NA, bullseye_keno_oregon_index, NA, NA, bullseye_keno_tennessee_index, NA, NA, NA, NA, NA)
                 )

# Create the table
data %>%
  gt() %>%
  tab_header(title = "Keno Expected Value by State") %>%
  cols_label(
    Base_Expected = "Base Expected",
    Base_Max_Spots = "Base Max Spots",
    Bullseye_Expected = "Bullseye Expected",
    Bullseye_Max_Spots = "Bullseye Max Spots"
  ) %>%
  fmt_missing(columns = everything(), rows = everything(), missing_text = "N/A") %>%
  tab_options(table.width = pct(100), table.align = "center")

#map
plot1<-plot_usmap(data = data, values = "Base_Expected", labels = TRUE) + 
  scale_fill_gradient(name = "Max Expected Value", label = scales::comma) + 
  theme(legend.position = "right")+
  ggtitle("Base KENO Expected Values Map")


# Create a dataframe with all missing states, replacing values with NA
missing_states <- c("AL", "AK", "AZ", "AR", "CO", "FL", "HI", "ID", "IL", "IA", "LA", 
                    "ME", "MN", "MS", "MT", "NE", "NV", "NM", "ND", "OK", "SC", "SD", "TX", "UT", "VT", "WI")

# Add missing states to data
missing_data <- data.frame(
  state = missing_states,
  Base_Expected = rep(NA, length(missing_states)),
  Base_Max_Spots = rep(NA, length(missing_states)),
  Bullseye_Expected = rep(NA, length(missing_states)),
  Bullseye_Max_Spots = rep(NA, length(missing_states)))

# Add missing states to the existing data
data_updated <- rbind(data, missing_data)

# Ensure Bullseye_Expected is numeric where it is not NA or "State that does not offer KENO"
data_updated$Bullseye_Expected <- as.numeric(as.character(data_updated$Bullseye_Expected))

# Create a function to categorize the Bullseye_Expected values
categorize_bullseye <- function(x) {
  if (is.na(x) || x == "State that does not offer Bullseye KENO") {
    return("State that does not offer Bullseye KENO")
  } else if (x < 1.05) {
    return("Below 1.05")
  } else if (x >= 1.05 & x < 1.10) {
    return("1.05 - 1.10")
  } else if (x >= 1.10 & x < 1.15) {
    return("1.10 - 1.15")
  } else if (x >= 1.15 & x < 1.20) {
    return("1.15 - 1.20")
  } else if (x >= 1.20 & x < 1.25) {
    return("1.20 - 1.25")
  } else if (x >= 1.25) {
    return("Above 1.25")
  } else {
    return("State that does not offer Bullseye KENO")  # Default for unexpected values
  }
}

# Apply the categorization function
data_updated$Bullseye_Category <- sapply(data_updated$Bullseye_Expected, categorize_bullseye)

# Convert to factor to maintain the correct order in the legend
data_updated$Bullseye_Category <- factor(data_updated$Bullseye_Category, 
                                         levels = c("Below 1.05", "1.05 - 1.10", "1.10 - 1.15", 
                                                    "1.15 - 1.20", "1.20 - 1.25", "Above 1.25", 
                                                    "State that does not offer Bullseye KENO"))

# Plot the map
plot2 <- plot_usmap(data = data_updated, values = "Bullseye_Category", labels = TRUE) + 
  scale_fill_manual(
    name = "Max Expected Value", 
    values = c("Below 1.05" = "#d0ebff",  
               "1.05 - 1.10" = "#a5d8ff",  
               "1.10 - 1.15" = "#74c0fc",  
               "1.15 - 1.20" = "#4dabf7",  
               "1.20 - 1.25" = "#1c7ed6",  
               "Above 1.25" = "#0b5394",
               "State that does not offer Bullseye KENO" = "dark gray"),  # Color for NA states explicitly
    na.value = "dark gray"  # Color for NA states
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 15, face = "bold"),  
    legend.text = element_text(size = 15),  
    legend.key.size = unit(0.8, "cm")
  )

plot2
