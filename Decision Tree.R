#importing libraries
library(rpart)
library(rpart.plot)

#model 1
model1 <- rpart(Attrition ~ ., data = train, method = 'class')
model1

#plotting
rpart.plot(model1)

pred1 <- predict(model1, newdata = test, type = 'class')
confusionMatrix(test$Attrition, pred1)

#area under the curve
auc(test$Attrition, as.numeric(pred1))
ROCRpred1 <- prediction(as.numeric(pred1),test$Attrition)
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')
plot(ROCRperf1, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

#2nd model
model2 <- rpart(Attrition ~ . , data = train, control = list(minsplit = 10, xval = 10, maxdepth = 50), method = 'class')
model2
rpart.plot(model2)
pred2 <- predict(model2, newdata = test, type = 'class')
confusionMatrix(test$Attrition, pred2)

#area under the curve
auc(test$Attrition, as.numeric(pred2))
ROCRpred2 <- prediction(as.numeric(pred2),test$Attrition)
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
plot(ROCRperf2, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)


#3rd model
model3 <- rpart(Attrition ~ . , data = train, control = rpart.control(minsplit = 5, xval = 20, maxdepth = 30), method = 'class')
model3
rpart.plot(model3)
pred3 <- predict(model3, newdata = test, type = 'class')
confusionMatrix(test$Attrition, pred3)

#area under the curve
auc(test$Attrition, as.numeric(pred3))
ROCRpred3 <- prediction(as.numeric(pred3),test$Attrition)
ROCRperf3 <- performance(ROCRpred3, 'tpr','fpr')
plot(ROCRperf3, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

#4th model
model4 <- rpart(Attrition ~ . , data = train, control = rpart.control(minsplit = 5, xval = 5, maxdepth = 30), method = 'class')
model4
rpart.plot(model4)
pred4 <- predict(model4, newdata = test, type = 'class')
confusionMatrix(test$Attrition, pred4)

#area under the curve
auc(test$Attrition, as.numeric(pred4))
ROCRpred4 <- prediction(as.numeric(pred4),test$Attrition)
ROCRperf4 <- performance(ROCRpred4, 'tpr','fpr')
plot(ROCRperf4, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

