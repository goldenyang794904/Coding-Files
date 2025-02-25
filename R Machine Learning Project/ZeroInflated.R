library(ggplot2)
library(pscl)
claims <- 0:10
prob_vector <- c(0.5, 0.2, 0.2, 0.03, 0.02, 0.02, 0.01, 0.01, 0.005, 0.0025, 0.0025)
claim <- sample(claims, 1000, replace = TRUE, prob = prob_vector)

amount <- sapply(claim, function(x) if (x != 0) sample(0:1000, 1) else 0)

data <- data.frame(claim, amount)


model1<-glm(claim ~ amount, family = "poisson")
summary(model1)

model2<-zeroinfl(claim~amount)
summary(model2)

AIC(model1, model2)
