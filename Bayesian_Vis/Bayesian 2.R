library(ggplot2)

pi <- seq(0.005, 0.995, length = 1000)
alpha <- 8
beta <- 44
n <- 4
y <- 2

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

interval <- qbeta(c(0.025, 0.975), alpha + y, beta + n - y)

ggplot(data, aes(x = pi)) +
  geom_line(aes(y = Prior, color = "Prior"), linewidth = 1) +
  geom_line(aes(y = Posterior, color = "Posterior"), linewidth = 1) +
  geom_vline(xintercept = interval, linetype = "dashed", color = "black") +
  annotate("text", x = interval[1], y = 0, label = sprintf("%.3f", interval[1]), 
           hjust = 1, vjust = 0, color = "black") +
  annotate("text", x = interval[2], y = 0, label = sprintf("%.3f", interval[2]), 
           hjust = 0, vjust = 1.5, color = "black") +
  labs(
    title = "Prior and Posterior Distributions",
    x = expression(pi),
    y = "Density",
    color = "Distribution"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Prior" = "red", "Posterior" = "blue"))

#If you're referring to a confidence interval, you could say: "We are 95% confident that the true parameter lies between 0.5 and 0.8."
#If you're referring to a credible set (Bayesian), you could say: "Given the observed data, there is a 95% probability that the true parameter lies between 0.5 and 0.8."
#The key difference is that confidence intervals are based on repeated sampling interpretations, while credible sets reflect a direct probability statement about the parameter itself.

#Hypothesis Testing
(prob<-pbeta(0.1, alpha + y, beta + n - y))

#Predictive Probability
#New Sample
n1<-25
y1<-0:25

predict <- function(alpha, beta, n1, y1) {
  prob <- choose(n1, y1) * 
    (gamma(alpha + beta) / (gamma(alpha) * gamma(beta))) * 
    ((gamma(alpha + y1) * gamma(beta + n1 - y1)) / gamma(alpha + beta + n1))
  return(prob)
}


predicted_prob<-predict(alpha, beta, n1, y1)

sum(predicted_prob)





