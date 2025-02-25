set.seed(123)
library(randomForest)
library(caret)
library(Metrics)

outcome <- traindata$SalePrice
partition <- createDataPartition(y = outcome,
                                 p = 0.5,
                                 list = F)
training <- traindata[partition,]
testing <-traindata[-partition,]

training_linear <- training[, -which(names(training) %in% c("Id","MSZoning","LandContour","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","Foundation","Heating","CentralAir","Electrical","GarageType","PavedDrive","MiscFeature","SaleType"))]

X <- training[, -which(names(training) %in% c("Id","SalePrice"))]
Y <- training$SalePrice 

#Linear Model
lm_model<-lm(SalePrice ~MSSubClass+LotArea+OverallQual+OverallCond+MasVnrArea+ExterQual+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+`1stFlrSF`+`2ndFlrSF`+BsmtFullBath+FullBath+BedroomAbvGr+KitchenQual+TotRmsAbvGrd+Functional+GarageCars+ScreenPorch+SaleCondition, data=training_linear)
summary(lm_model)

Linear_prediction <- predict(lm_model, testing, type="response")
model_output <- cbind(testing, Linear_prediction)

rmse(model_output$SalePrice,model_output$Linear_prediction)


#Random Forest Model
rfmodel <- randomForest(x=training_linear,y=Y, ntree = 50)

Forest_prediction <- predict(rfmodel, testing)
model_output <- cbind(model_output, Forest_prediction)

rmse(model_output$SalePrice, model_output$Forest_prediction)

