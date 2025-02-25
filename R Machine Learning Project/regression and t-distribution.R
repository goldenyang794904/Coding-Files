regression <- function(x,y){
  meanx <- mean(x)
  meany <- mean(y)
  total1 <- sum((x - meanx)*(y-meany))
  total2 <- sum((x - meanx)^2)
  m <- total1/total2
  return(m)
}

slope <- function(x,y){
  return(cov(x,y)/var(x))
}

intercept <- function(x,y,m){
  b0<- mean(y)-(m*mean(x))
  return(b0)
}

regression(x=c(10,15,20,25,35,40),y=c(5,8,7,9,10,2))
slope(x=c(10,15,20,25,35,40),y=c(5,8,7,9,10,2))
intercept(x=c(10,15,20,25,35,40),y=c(5,8,7,9,10,2),-0.03850932)

confidence <- function(x,a){
  n<-length(x)
  t <- qt(1-a/2,n-1)
  lower <- mean(x)-t*sd(x)/sqrt(n)
  upper <- mean(x)+t*sd(x)/sqrt(n)
  return(c('Lower=',lower,'Upper=',upper))
}

confidence(data$mpg,.9)

confidence(x=c(5,10,15,20,22,23,24,25),.99)

data <- datasets::mtcars
data
ci <- function(x,a){
  n <- length(x)
  t <- qt((1-a)/2,n-1)
  lower <- mean(x)-(t*sd(x)/sqrt(n))
  print(paste('Lower=',lower))
}
confidence(data$mpg,.9)

a <- function(x, alpha){
  n <- length(x)
  t <- qt((1-alpha)/2, n-1)
  lower <- mean(x) + ((t*sd(x))/sqrt(n))
  upper <- mean(x) - ((t*sd(x))/sqrt(n))
  return(c("lower = ", lower, "upper = ", upper))
}
a(data$mpg,.9)
ab <- datasets::airquality
ab
newdata<-na.omit(ab)

r <- function(c,d){
  meanc <- mean(c)
  meand <- mean(d)
  total1 <- sum((c - meanc)*(d-meand))
  total2 <- sum((c - meanc)^2)
  m <- total1/total2
  return(m)
}

s<-function(x,y){
  return(cov(x,y)/var(x))
}

s(newdata$Ozone,newdata$Solar.R)

b <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}

# the slope formula is just covariance(x, y) / variance(x)
c <- function(x, y){
  return(cov(x, y)/var(x))
}

d <- function(x, y, m){
  b <- mean(y) - (m * mean(x))
  return(b)
}

c(newdata$Solar.R,newdata$Ozone)
