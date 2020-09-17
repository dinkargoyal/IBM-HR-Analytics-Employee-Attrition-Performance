#install packages
#install.packages('randomForest')
library(randomForest)
library(caret)
#install.packages('e1071')
library(e1071)


#1st model and prediction
model1 <- randomForest(formula = Attrition ~ . , ntree = 20, data = train, importance = TRUE)
model1

pred1 <- predict(model1, newdata = test)
confusionMatrix(test$Attrition, pred1)

#area under the curve
auc(test$Attrition, as.numeric(pred1))
ROCRpred1 <- prediction(as.numeric(pred1),test$Attrition)
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')
plot(ROCRperf1, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)


#2nd model
model2 <- randomForest(formula = Attrition ~ . , ntree = 100, data = train, importance = TRUE)
model2

pred2 <- predict(model2, newdata = test)
confusionMatrix(test$Attrition, pred2)

#area under the curve
auc(test$Attrition, as.numeric(pred2))
ROCRpred2 <- prediction(as.numeric(pred2),test$Attrition)
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
plot(ROCRperf2, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)


#3rd model
model3 <- randomForest(formula = Attrition ~ . , ntree = 400, mtry = 10, data = train, importance = TRUE)
model3

pred3 <- predict(model3, newdata = test)
confusionMatrix(test$Attrition, pred3)

#area under the curve
auc(test$Attrition, as.numeric(pred3))
ROCRpred3 <- prediction(as.numeric(pred3),test$Attrition)
ROCRperf3 <- performance(ROCRpred3, 'tpr','fpr')
plot(ROCRperf3, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

#4th model
model4 <- randomForest(formula = Attrition ~ . , ntree = 1000, mtry = 20, data = train, importance = TRUE)
model4

pred4 <- predict(model4, newdata = test)
confusionMatrix(test$Attrition, pred4)

#area under the curve
auc(test$Attrition, as.numeric(pred4))
ROCRpred4 <- prediction(as.numeric(pred4),test$Attrition)
ROCRperf4 <- performance(ROCRpred4, 'tpr','fpr')
plot(ROCRperf4, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)


#5th model
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
                              
model5 <- randomForest(formula = Attrition ~ . , ntree = 800, mtry = 30, data = train, trControl = train.control, importance = TRUE)
model5

pred5 <- predict(model5, newdata = test)
confusionMatrix(test$Attrition, pred5)

#area under the curve
auc(test$Attrition, as.numeric(pred5))
ROCRpred5 <- prediction(as.numeric(pred5),test$Attrition)
ROCRperf5 <- performance(ROCRpred5, 'tpr','fpr')
plot(ROCRperf5, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)







