training <- traindata[partition,]
testing <-traindata[-partition,]

training_linear <- training[, -which(names(training) %in% c("Id","MSZoning","LandContour","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","Foundation","Heating","CentralAir","Electrical","GarageType","PavedDrive","MiscFeature","SaleType"))]

lm_model_test<-lm(SalePrice ~MSSubClass+LotArea+OverallQual+OverallCond+MasVnrArea+ExterQual+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+`1stFlrSF`+`2ndFlrSF`+BsmtFullBath+FullBath+BedroomAbvGr+KitchenQual+TotRmsAbvGrd+Functional+GarageCars+ScreenPorch+SaleCondition, data=training_linear)
summary(lm_model_test)
SalePrice <- predict(lm_model, testdata, type="response")
model_output_test <- cbind(testdata, SalePrice)

X_test <- traindata[, -which(names(training) %in% c("Id","SalePrice"))]
Y_test <- traindata$SalePrice 

rfmodel_test <- randomForest(SalePrice ~MSSubClass+LotArea+OverallQual+OverallCond+MasVnrArea+ExterQual+BsmtQual+BsmtExposure+BsmtFinType1+BedroomAbvGr+KitchenQual+TotRmsAbvGrd+GarageCars+ScreenPorch, data=training_linear, ntree = 1000)

SalePrice <- predict(rfmodel_test, testdata)
model_output_test <- cbind(testdata, SalePrice)

Result1<-model_output_test[, which(names(model_output_test) %in% c("Id","SalePrice"))]

write.csv(Result1, "Result4.csv", row.names = FALSE)

