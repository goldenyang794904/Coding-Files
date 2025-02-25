library(ggplot2)
library(tidyr) 
library(LearnBayes)

#Binomial Beta Posterior
pi <- seq(0.005, 0.995, length = 1000)
alpha <- 1
beta <- 1
n <- 10
y <- 7

prior <- dbeta(pi, alpha, beta)
likelihood<-dbinom(y, size = n, prob = pi)
plot(pi, likelihood, type = "l")
posterior <- dbeta(pi, alpha + y, beta + n - y)

data <- data.frame(
  pi = pi,
  Prior = prior,
  Likelihodd = likelihood,
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

#Beta-Geometric Posterior
pi <- seq(0.005, 0.995, length = 1000)
alpha<-2.5
beta<-4
n<-10
x_bar<-4

prior<-dbeta(pi, alpha, beta)
geometric_likelihood<-function(pi,n,x_bar){
  (pi^n)*((1-pi)^(n*(x_bar -1)))
}
likelihood<-geometric_likelihood(pi, n, x_bar)
plot(pi,likelihood, type = "l")

posterior<-dbeta(pi, alpha+n, beta+n*(x_bar-1))

data <- data.frame(
  pi = pi,
  Prior = prior,
  Likelihodd = likelihood,
  Posterior = posterior
) 

ggplot(data, aes(x = pi)) +
  geom_line(aes(y = Prior, color = "Prior"), size = 1) +
  geom_line(aes(y = Posterior, color = "Posterior"), size = 1) +
  labs(
    title = "Prior and Posterior Distributions",
    x = expression(pi),
    y = "Density",
    color = "Distribution"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Prior" = "red", "Posterior" = "blue"))

#Gamma-Poisson 
lambda<-seq(0, 10, by = 0.01)
alpha<-2.5
beta<-10
n<-10
x_bar<-8

prior<-dgamma(lambda, shape = alpha, rate = beta)
poisson_likelihood<-function(lambda, n, x_bar){
  exp(-n*lambda)*lambda^(n*x_bar)
}

likelihood<-poisson_likelihood(lambda, n, x_bar)
posterior<-dgamma(lambda, shape = alpha+n*x_bar, rate = beta+n)

data <- data.frame(
  lambda = lambda,
  Prior = prior,
  Likelihodd = likelihood,
  Posterior = posterior
) 

ggplot(data, aes(x = lambda)) +
  geom_line(aes(y = Prior, color = "Prior"), size = 1) +
  geom_line(aes(y = Posterior, color = "Posterior"), size = 1) +
  labs(
    title = "Prior and Posterior Distributions",
    x = expression(lambda),
    y = "Density",
    color = "Distribution"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Prior" = "red", "Posterior" = "blue"))




