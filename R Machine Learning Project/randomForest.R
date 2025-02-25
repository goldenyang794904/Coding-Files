library(tree)
library(randomForest)
library(np)


set.seed(10)
n=128
training_example1<-data.frame(matrix(NA,   
                                     nrow = n,
                                     ncol = 50),
                              'A'=sample(c(0,1,2),n,replace=TRUE),
                              'R0'=0,
                              'error'=rnorm(n))

for (i in 1:50) {
  training_example1[, i] <- runif(n, -1, 1)
}

training_example1_optfunction<-((1+0.5*training_example1$X1+0.5*training_example1$X2)-training_example1$A)^2

training_example1$R0<-8+(4*training_example1$X1)-(2*training_example1$X2)-(2*training_example1$X3)-(25*training_example1_optfunction)+training_example1$error

training_example1<-subset(training_example1, select = -error)

tree<-randomForest(R0~., data=training_example1, importance = TRUE, mtry=10)

group_example1<-data.frame(matrix(NA,   
                                  nrow = 10000,
                                  ncol = 50),
                           'A0'=0,
                           'A1'=1,
                           'A2'=2,
                           'R0'=0,
                           'error'=rnorm(10000))

for (i in 1:50) {
  group_example1[, i] <- runif(10000, -1, 1)
}

newgroup_example1_A0 <- data.frame(group_example1[, c(paste0("X", 1:50))],A=group_example1$A0)

group_example1$Q0<-predict(tree,newdata=newgroup_example1_A0)

newgroup_example1_A1 <- data.frame(group_example1[, c(paste0("X", 1:50))],A=group_example1$A1)

group_example1$Q1<-predict(tree,newdata=newgroup_example1_A1)

newgroup_example1_A2 <- data.frame(group_example1[, c(paste0("X", 1:50))],A=group_example1$A2)

group_example1$Q2<-predict(tree,newdata=newgroup_example1_A2)

group_example1$Treatment<-NA

# Create a vector to store the treatment values
group_example1$Treatment <- max.col(group_example1[, c("Q0", "Q1", "Q2")]) - 1

group_example1$R0<-8+(4*group_example1$X1)-(2*group_example1$X2)-(2*group_example1$X3)-(25*((1+0.5*group_example1$X1+0.5*group_example1$X2)-group_example1$Treatment)^2)+group_example1$error
group_example1$R1<-8+(4*group_example1$X1)-(2*group_example1$X2)-(2*group_example1$X3)-(25*((1+0.5*group_example1$X1+0.5*group_example1$X2)-group_example1$A0)^2)+group_example1$error
group_example1$R2<-8+(4*group_example1$X1)-(2*group_example1$X2)-(2*group_example1$X3)-(25*((1+0.5*group_example1$X1+0.5*group_example1$X2)-group_example1$A1)^2)+group_example1$error
group_example1$R3<-8+(4*group_example1$X1)-(2*group_example1$X2)-(2*group_example1$X3)-(25*((1+0.5*group_example1$X1+0.5*group_example1$X2)-group_example1$A2)^2)+group_example1$error

group_example1$optimal <- NA

group_example1$optimal <- max.col(group_example1[, c("R1", "R2", "R3")]) - 1

group_example1$optimalR<-8+(4*group_example1$X1)-(2*group_example1$X2)-(2*group_example1$X3)-(25*((1+0.5*group_example1$X1+0.5*group_example1$X2)-group_example1$optimal)^2)+group_example1$error

mean(group_example1$R0)
