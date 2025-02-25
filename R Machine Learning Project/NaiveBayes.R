library(e1071)
library(caTools)
library(caret)
library(ISLR)

data<-data.frame(x=rnorm(100),
                 y=sample(c("Yes", "No"), prob = c(0.3,0.7), replace = TRUE, size = 100))

split<-sample(nrow(data), 0.8*nrow(data))
training_set<-data[split,]
test_set<-data[-split,]

set.seed(1)
classifier<-naiveBayes(y ~., data = training_set)

pred<-predict(classifier, newdata = test_set)

cm<-table(pred, test_set$y)





