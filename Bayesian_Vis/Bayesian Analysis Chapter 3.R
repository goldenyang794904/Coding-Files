library(LearnBayes)
density<-dbinom(length,10,.2)
length<-seq(0,10,length = 11)
plot(density)

triplot(c(5,20), c(10,7))

x <- seq( qnorm( 0.005, 20,5 ), qnorm( 0.995, 20, 5), length = 100)
y <- dnorm( x, 20, 5)
plot( x,y, type="l")
