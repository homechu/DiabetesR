install.packages("AER")
library(AER)

diabete.glmtrain <- diabete.train
diabete.glmtest <- diabete.test
diabete.glmtrain$Class <- ifelse(diabete.glmtrain$Class == "diabete", 1, 0);
diabete.glmtest$Class <- ifelse(diabete.glmtest$Class == "diabete", 1, 0);


# 重新建立羅吉斯迴歸模型Diabete

diabete.glm <- glm(Class ~ ., family = "binomial", data = diabete.glmtrain)
train_predict <-predict(diabete.glm,newdata=diabete.glmtrain,type = "response")
result_Approved <- ifelse(train_predict > 0.5, 1, 0)
tableTrain <- table(diabete.glmtrain$Class, result_Approved, dnn = c("actual", "predict"))
result[10,2] <-paste0(round((sum(tableTrain)-sum(diag(tableTrain)))*100/sum(tableTrain),2),"%")
result

test_predict <-predict(diabete.glm,newdata=diabete.glmtest,type = "response")
result_Approved <- ifelse(test_predict > 0.5, 1, 0)
tableTest <- table(diabete.glmtest$Class, result_Approved, dnn = c("actual", "predict"))
result[10,3] <-paste0(round((sum(tableTest)-sum(diag(tableTest)))*100/sum(tableTest),2),"%")

#畫ROC曲線
install.packages("ROCR")
library("ROCR")
pred <- prediction(result, testdata$card)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#計算AUC
auc <- performance(pred, "auc")

#畫圖
plot(perf, col = rainbow(7), main = "ROC curve", xlab = "Specificity(FPR)", ylab = "Sensitivity(TPR)")
#AUC = 0.5
abline(0, 1)
#實際AUC值
text(0.5, 0.5, as.character(auc@y.values[[1]]))