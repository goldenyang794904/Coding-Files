data <- data.frame("1" = c(65,53,62,64,65,66),
                   "2" = c(58,54,68,62,65,65),
                   "3" = c(62,63,65,58,59,60),
                   "4" = c(66,67,64,68,66,61))


stack(data[, 1:4])

options(digits = 10)
result<-aov(values ~ ind, data = stack(data[, 1:4])) 
summary(result)

critical_F <- qf(0.95, df1 = 3, df2 = 20)
critical_F
