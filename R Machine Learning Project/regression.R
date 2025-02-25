library(car)
library(ggplot2)
library(tidyverse)
library(ISLR)
data<-Wage
View(data)
summary(data)

#manipulating data
Education<-rep(1,nrow(data))
Education[data$education=='1. <HS Grad']<-1
Education[data$education=='2. HS Grad']<-2
Education[data$education=='3. Some College']<-3
Education[data$education=='4. College Grad']<-4
Education[data$education=='5. Advanced Degree']<-5
Education<-as.factor(Education)
data<-data.frame(data,Education)
marriage<-rep(1,nrow(data))
marriage[data$maritl=='1. Never Married']<-1
marriage[data$maritl=='2. Married']<-2
marriage[data$maritl=='3. Widowed']<-3
marriage[data$maritl=='4. Divorced']<-4
Health<-rep(1,nrow(data))
Health[data$health=='1. <=Good']<-1
Health[data$health=='2. >=Very Good']<-2
data<-data.frame(marriage,data)
data<-data.frame(Health,data)
data$marriage<-as.numeric(data$marriage)
data$Education<-as.numeric(data$Education)
data$Health<-as.numeric(data$Health)
Overall_Score<-rep(1,nrow(data))
Overall_Score<-data$Health+data$marriage+data$Education
data<-data.frame(Overall_Score,data)

#testing regression
regression<-lm(wage~Education+Health+marriage+age,data=data)
r<-lm(Education~marriage+wage+Health,data=data)
summary(r)
summary(regression,correation=TRUE)
plot(regression)
predict(regression,newdata=data.frame(age=20,marriage=2,Health=2,Education=2),
                 interval='prediction')
residuals(regression)
?Wage

plot(data$Overall_Score,data$wage)
abline(data$wage,data$Overall_Score)
plot(data$wage,data$age)
abline(data$wage,data$age)

?lm

library(olsrr)
ols_step_forward_p(regression,prem=.5,detail=T)
plot(regression)

#correlation
par(mfrow=c(2,2))
cor(data$wage,data$)
plot(data$wage,data$age)
plot(data$wage,data$Health)
plot(data$wage,data$Education)
plot(data$wage,data$marriage)

#graph
regression%>%
  ggplot(aes(regression$fitted.values,regression$residuals))+
  geom_point()+
  labs(x='Fitted Values',y='Residuals',title='Versus Fits')+
  theme(plot.title=element_text(hjust=.5))
  
