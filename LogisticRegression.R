# 1st model and prediction
model1 <- glm(Attrition ~ ., family = 'binomial', data = train)
summary(model1)

pred1 = predict(model1, type = 'response', newdata = test)
y_pred1 = ifelse(pred1 > 0.5, 1, 0)


#confusion matrix
cm1 = table(test[,2], y_pred1 > 0.5)
cm1

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
library(pROC)
auc(test$Attrition, pred1)
roc1 <- roc(test$Attrition, pred1 , quiet = FALSE)

#ROCR Curve
library(ROCR)
ROCRpred1 <- prediction(pred1,test$Attrition)
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')


plot(ROCRperf1, colorize = TRUE, text.adj = c(-0.2,1.7))


#removal higher p value column
train <- train[,-24]
train <- train[,-6]
test <- test[,-24]
test <- test[,-6]


#2nd model and prediction
model2 <- glm(Attrition ~ ., family = 'binomial', data = train)
summary(model2)

pred2 = predict(model2, type = 'response', newdata = test)
y_pred2 = ifelse(pred2 > 0.5, 1, 0)

#confusion matrix
cm2 = table(test[,2], y_pred2 > 0.5)
cm2

acc2 <- sum(diag(cm2))/sum(cm2)
acc2

#precision
prec2 <- diag(cm2)/colSums(cm2,2)
prec2

#recall
rc2 <- diag(cm2)/rowSums(cm2,2)
rc2

#f1 score
f1_score2 <- 2*prec2*rc2/(prec2+rc2)
f1_score2

#auc
#library(pROC)
auc(test$Attrition, pred2)
roc2 <- roc(test$Attrition, pred2 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred2 <- prediction(pred2,test$Attrition)
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')


plot(ROCRperf2, colorize = TRUE, text.adj = c(-0.2,1.7))


#3rd model and prediction
model3 <- glm(Attrition ~ ., family = 'binomial', data = train)
summary(model3)

pred3 = predict(model3, type = 'response', newdata = test)
y_pred3 = ifelse(pred3 > 0.5, 1, 0)

#confusion matrix
cm3 = table(test[,2], y_pred3 > 0.5)
cm3

#accuracy
acc3 <- sum(diag(cm3))/sum(cm3)
acc3

#precision
prec3 <- diag(cm3)/colSums(cm3,2)
prec3

#recall
rc3 <- diag(cm3)/rowSums(cm3,2)
rc3

#f1 score
f1_score3 <- 2*prec3*rc3/(prec3+rc3)
f1_score3

#auc
#library(pROC)
auc(test$Attrition, pred3)
roc3 <- roc(test$Attrition, pred3 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred3 <- prediction(pred3,test$Attrition)
ROCRperf3 <- performance(ROCRpred3, 'tpr','fpr')


plot(ROCRperf3, colorize = TRUE, text.adj = c(-0.2,1.7))

#removal higher p value column
train <- train[,-15]
test <- test[,-15]


#4th model and prediction
model4 <- glm(Attrition ~ ., family = 'binomial', data = train)
summary(model4)

pred4 = predict(model4, type = 'response', newdata = test)
y_pred4 = ifelse(pred4 > 0.5, 1, 0)


#confusion matrix
cm4 = table(test[,2], y_pred4 > 0.5)
cm4

#accuracy
acc4 <- sum(diag(cm4))/sum(cm4)
acc4

#precision
prec4 <- diag(cm4)/colSums(cm4,2)
prec4

#recall
rc4 <- diag(cm4)/rowSums(cm4,2)
rc4

#f1 score
f1_score4 <- 2*prec4*rc4/(prec4+rc4)
f1_score4

#auc
#library(pROC)
auc(test$Attrition, pred4)
roc4 <- roc(test$Attrition, pred4 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred4 <- prediction(pred4,test$Attrition)
ROCRperf4 <- performance(ROCRpred4, 'tpr','fpr')

plot(ROCRperf4, colorize = TRUE, text.adj = c(-0.2,1.7))

#removal higher p value column
train <- train[,-8]
test <- test[,-8]


#5th model and prediction
model5 <- glm(Attrition ~ ., family = 'binomial', data = train)
summary(model5)

pred5 = predict(model5, type = 'response', newdata = test)
y_pred5 = ifelse(pred5 > 0.5, 1, 0)

#confusion matrix
cm5 = table(test[,2], y_pred5 > 0.5)
cm5

#accuracy
acc5 <- sum(diag(cm5))/sum(cm5)
acc5

#precision
prec5 <- diag(cm5)/colSums(cm5,2)
prec5

#recall
rc5 <- diag(cm5)/rowSums(cm5,2)
rc5

#f1 score
f1_score5 <- 2*prec5*rc5/(prec5+rc5)
f1_score5

#auc
#library(pROC)
auc(test$Attrition, pred5)
roc5 <- roc(test$Attrition, pred5 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred5 <- prediction(pred5,test$Attrition)
ROCRperf5 <- performance(ROCRpred5, 'tpr','fpr')

plot(ROCRperf5, colorize = TRUE, text.adj = c(-0.2,1.7))

#removal higher p value column
train <- train[,-25]
test <- test[,-25]


#6th model and prediction
model6 <- glm(Attrition ~ ., family = 'binomial', data = train)
summary(model6)

pred6 = predict(model6, type = 'response', newdata = test)
y_pred6 = ifelse(pred6 > 0.5, 1, 0)

#confusion matrix
cm6 = table(test[,2], y_pred6 > 0.5)
cm6

#accuracy
acc6 <- sum(diag(cm6))/sum(cm6)
acc6

#precision
prec6 <- diag(cm6)/colSums(cm6,2)
prec6

#recall
rc6 <- diag(cm6)/rowSums(cm6,2)
rc6

#f1 score
f1_score6 <- 2*prec6*rc6/(prec6+rc6)
f1_score6

#auc
library(pROC)
auc(test$Attrition, pred6)
roc6 <- roc(test$Attrition, pred6 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred6 <- prediction(pred6,test$Attrition)
ROCRperf6 <- performance(ROCRpred6, 'tpr','fpr')

plot(ROCRperf6, colorize = TRUE, text.adj = c(-0.2,1.7), reuse.auc = TRUE, print.auc = TRUE)

