library(LearnBayes)
library(ggplot2)

#Binomial Jeffery Prior
alpha<-0.5
beta<-0.5
n<-1000
y<-400

pi<-seq(0.005, 0.995, length = 1000)
prior<-dbeta(pi, alpha, beta)
likelihood<-dbinom(y, size = n, prob = pi)
posterior<-dbeta(pi,alpha+y, beta+n-y)

data<-data.frame(
  pi = pi,
  Prior = prior,
  Likelihood = likelihood,
  Posterior = posterior
)

ggplot(data, aes(x = pi)) +
  geom_line(aes(y = Prior, color = "Prior"), linewidth = 1) +
  geom_line(aes(y = Posterior, color = "Posterior"), linewidth = 1) +
  labs(
    title = "Prior and Posterior Distributions",
    x = expression(pi),
    y = "Density",
    color = "Distribution"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Prior" = "red", "Posterior" = "blue"))

#Poisson Jeffery Prior
lambda<-seq(0,10,by=0.01)
x<-c(rep(5,100), rep(10,124))


prior<-1/sqrt(lambda)
likelihood<-exp(-length(x)*lambda)*lambda^(length(x)*mean(x))
posterior<-dgamma(lambda, shape = length(x)*mean(x)+1/2, rate = length(x))

data <- data.frame(
  lambda = lambda,
  Prior = prior,
  Likelihodd = likelihood,
  Posterior = posterior
) 

interval<-qgamma(c(0.025, 0.975), shape = length(x)*mean(x) + 1/2, rate = length(x))

ggplot(data, aes(x = lambda)) +
  geom_line(aes(y = Prior, color = "Prior"), linewidth = 1) +
  geom_line(aes(y = Posterior, color = "Posterior"), linewidth = 1) +
  geom_vline(xintercept = interval, linetype = "dashed", color = "black") +
  annotate("text", x = interval[1], y = 0, label = sprintf("%.3f", interval[1]), 
           hjust = 1, vjust = -0.5, color = "black") +
  annotate("text", x = interval[2], y = 0, label = sprintf("%.3f", interval[2]), 
           hjust = 0, vjust = 1.5, color = "black") +
  labs(
    title = "Prior and Posterior Distributions",
    x = expression(lambda),
    y = "Density",
    color = "Distribution"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Prior" = "red", "Posterior" = "blue"))

