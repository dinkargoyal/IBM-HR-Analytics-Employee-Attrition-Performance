#install.packages('ROSE')
library(ROSE)
#install.packages('DMwR')
library(DMwR)
library(caret)
library(e1071)
library(kernlab)
#install.packages('boot')
library(boot)
library(ggplot2)
library(caTools)
library(ROCR)
library(pROC)
library(rpart)
library(rpart.plot)


att <- SMOTE(Attrition ~ ., data = att, perc.over = 200, k = 5, perc.under = 250)
table(att$Attrition)

#splitting the data 
set.seed(118)
split = sample.split(att,SplitRatio = 0.75)
train <- subset(att, split ==TRUE)
test <- subset(att, split == FALSE)

View(train)
View(test)



#model1
model1 <- glm(Attrition ~ ., family = 'binomial',  data = train )
model1 

pred1 = predict(model1, type = 'response', newdata = test)
y_pred1 = ifelse(pred1 > 0.5, 1, 0)


#confusion matrix
cm1 = table(test[,2], y_pred1 > 0.5)
cm1

#accuracy
acc1 <- sum(diag(cm1))/sum(cm1)
acc1

#precision
prec1 <- diag(cm1)/colSums(cm1,2)
prec1

#recall
rc1 <- diag(cm1)/rowSums(cm1,2)
rc1

#f1 score
f1_score1 <- 2*prec1*rc1/(prec1+rc1)
f1_score1

#auc

auc(test$Attrition, pred1)
roc1 <- roc(test$Attrition, pred1 , quiet = FALSE)

#ROCR Curve

ROCRpred1 <- prediction(pred1,test$Attrition)
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')

plot(ROCRperf1, colorize = TRUE, text.adj = c(-0.2,1.7))

#model2
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model2 <- randomForest(formula = Attrition ~ . , ntree = 300, mtry = 20, trControl = train.control, data = train, importance = TRUE)
model2

pred2 <- predict(model2, newdata = test)
confusionMatrix(test$Attrition, pred2)

#area under the curve
auc(test$Attrition, as.numeric(pred2))
ROCRpred2 <- prediction(as.numeric(pred2),test$Attrition)
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
plot(ROCRperf2, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)
varImpPlot(model2)
y <- importance(model2)
y
z = y[,4]
par(mar = c(5,17,4,1)+.2)
barplot(z[order(z)], horiz = TRUE, xlim = c(0,150), xlab = 'Importance', ylim = c(0,45), col = "Blue", las = 1)

#model3
train.control <- trainControl(method = "repeatedcv", number = 15, repeats = 5)
model3 <- randomForest(formula = Attrition ~ . , ntree = 500, mtry = 30,  trControl = train.control, data = train, importance = TRUE)
model3

pred3 <- predict(model3, newdata = test)
confusionMatrix(test$Attrition, pred3)

#area under the curve
auc(test$Attrition, as.numeric(pred3))
ROCRpred3 <- prediction(as.numeric(pred3),test$Attrition)
ROCRperf3 <- performance(ROCRpred3, 'tpr','fpr')
plot(ROCRperf3, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

#model4
train.control <- trainControl(method = "repeatedcv", number = 15, repeats = 5)
model4 <- randomForest(formula = Attrition ~ . , ntree = 1000, mtry = 30, trControl = train.control,  data = train, importance = TRUE)
model4

pred4 <- predict(model4, newdata = test)
confusionMatrix(test$Attrition, pred4)

#area under the curve
auc(test$Attrition, as.numeric(pred4))
ROCRpred4 <- prediction(as.numeric(pred4),test$Attrition)
ROCRperf4 <- performance(ROCRpred4, 'tpr','fpr')
plot(ROCRperf4, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

#model 5
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model5 <- train(Attrition ~ . , data = train, method = "svmLinear", trControl=train.control, preProcess = c("center", "scale"),  tuneLength = 10)
model5                  

pred5 <- predict(model5, newdata = test)
confusionMatrix(test$Attrition, pred5)

#area under the curve
auc(test$Attrition, as.numeric(pred5))
ROCRpred5 <- prediction(as.numeric(pred5),test$Attrition)
ROCRperf5 <- performance(ROCRpred5, 'tpr','fpr')
plot(ROCRperf5, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)


#model 6
model6 <- rpart(Attrition ~ ., data = train, method = 'class')
model6

#plotting
rpart.plot(model6)

pred6 <- predict(model6, newdata = test, type = 'class')
confusionMatrix(test$Attrition, pred6)

#area under the curve
auc(test$Attrition, as.numeric(pred6))
ROCRpred6 <- prediction(as.numeric(pred6),test$Attrition)
ROCRperf6 <- performance(ROCRpred6, 'tpr','fpr')
plot(ROCRperf6, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

# model 7
model7 <- rpart(Attrition ~ . , data = train, control = rpart.control(minsplit = 10, xval = 10, maxdepth = 30), method = 'class')
model7
rpart.plot(model7)
pred7 <- predict(model7, newdata = test, type = 'class')
confusionMatrix(test$Attrition, pred7)

#area under the curve
auc(test$Attrition, as.numeric(pred7))
ROCRpred7 <- prediction(as.numeric(pred7),test$Attrition)
ROCRperf7 <- performance(ROCRpred7, 'tpr','fpr')
plot(ROCRperf7, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

# model 8
model8 <- rpart(Attrition ~ . , data = train, control = rpart.control(minsplit = 5, xval = 20, maxdepth = 30), method = 'class')
model8
rpart.plot(model8)
pred8 <- predict(model8, newdata = test, type = 'class')
confusionMatrix(test$Attrition, pred8)

#area under the curve
auc(test$Attrition, as.numeric(pred8))
ROCRpred8 <- prediction(as.numeric(pred8),test$Attrition)
ROCRperf8 <- performance(ROCRpred8, 'tpr','fpr')
plot(ROCRperf8, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

# model 9
model9 <- rpart(Attrition ~ . , data = train, control = rpart.control(minsplit = 5, xval = 5, maxdepth = 30), method = 'class')
model9
rpart.plot(model9)
pred9 <- predict(model9, newdata = test, type = 'class')
confusionMatrix(test$Attrition, pred9)

#area under the curve
auc(test$Attrition, as.numeric(pred9))
ROCRpred9 <- prediction(as.numeric(pred9),test$Attrition)
ROCRperf9 <- performance(ROCRpred9, 'tpr','fpr')
plot(ROCRperf9, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

                   
#model10
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model10 <- randomForest(formula = Attrition ~ . , ntree = 1000, mtry = 30, trControl = train.control,  data = train, importance = TRUE)
model10

pred10 <- predict(model10, newdata = test)
confusionMatrix(test$Attrition, pred10)

#area under the curve
auc(test$Attrition, as.numeric(pred10))
ROCRpred10 <- prediction(as.numeric(pred10),test$Attrition)
ROCRperf10 <- performance(ROCRpred10, 'tpr','fpr')
plot(ROCRperf10, colorize = TRUE, text.adj = c(-0.2,1.7), print.auc = TRUE)

#model11
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model11 <- randomForest(formula = Attrition ~ . , ntree = 450, mtry = 15, trControl = train.control,  data = train, importance = TRUE)
model11

pred11 <- predict(model11, newdata = test)
confusionMatrix(test$Attrition, pred11)

#area under the curve
auc(test$Attrition, as.numeric(pred11))
ROCRpred11 <- prediction(as.numeric(pred11),test$Attrition)
ROCRperf11 <- performance(ROCRpred11, 'tpr','fpr')
plot(ROCRperf11, colorize = TRUE, text.adj = c(-0.2,1.7))

varImpPlot(model11)
y <- importance(model11)
y
z = y[,4]
par(mar = c(5,17,4,1)+.2)
barplot(z[order(z)], horiz = TRUE, xlim = c(0,150), xlab = 'Importance', ylim = c(0,45), col = "Blue", las = 1)



