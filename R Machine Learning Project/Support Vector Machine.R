library(e1071)
x1<-log(rnorm(1000,100,1))+runif(1000,0,2)
x2<-log(rnorm(1000,10,1))+runif(1000,0,1)
y<-as.factor(ifelse(x1>=5 & x2<3, "yes", "no"))
data<-data.frame(x1,x2,y)
split<-sample(nrow(data),0.8*nrow(data))
training_set<-data[split,]
test_set<-data[-split,]
plot(x2,x1,col=y)
model<-svm(y~., data=training_set, cost=1, kernel="polynomial",scale=TRUE)
plot(model, training_set)
prediction<-predict(model, newdata=test_set)
conf<-table(test_set$y,prediction)
mis<-1-(sum(diag(conf))/sum(conf))

tune<-tune(svm, y~., data=training_set, kernel="polynomial", ranges = list(cost=c(0.01,0.1,1,10,100,1000)))
best_model<-tune$best.model

plot(best_model, training_set)
prediction2<-predict(best_model, newdata=test_set)
conf2<-table(test_set$y,prediction2)
mis2<-1-(sum(diag(conf2))/sum(conf2))



