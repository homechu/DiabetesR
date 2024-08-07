install.packages("AER")
library(AER)

diabetes <-read.csv("D:/Download/糖尿病資料庫論文/pima_indians_diabetes.csv")
head(diabetes)

#(2)跑羅吉斯模型
card_glm <- glm(formula = Class ~ ., family = "binomial", data = diabetes)

#(3)單筆資料預測
#30歲無信用不良紀錄，收入10萬美金，有自有住宅
new <- data.frame( 
                   pregnant = 0, glucose = 120, 
                   pressure = 75, Triceps = 25,
                   serum_insulin = 175,BMI = 35,
                   Diabetes_pedigree = 1.446,age = 24
                   )
result <- predict(card_glm, newdata = new, type = "response")
result


#(4)測試模型
#將資料分為訓練與測試組

#取得總筆數
n <- nrow(diabetes)
#設定隨機數種子
set.seed(1117)
#將數據順序重新排列
newdiabetes <- diabetes[sample(n),]

#取出樣本數的idx
t_idx <- sample(seq_len(n), size = round(0.7 * n))

#訓練資料與測試資料比例: 70%建模，30%驗證
traindata <- newdiabetes[t_idx,]
testdata <- newdiabetes[ - t_idx,]

# 重新建立羅吉斯迴歸模型
card_glm2 <- glm(formula = Class ~ ., family = "binomial", data = traindata)
result <- predict(card_glm2, newdata = testdata, type = "response")

#(5)建立混淆矩陣(confusion matrix)觀察模型表現

#r假設我們認定核準率60%以上才視為核卡，其餘是為拒件或補件
result_Approved <- ifelse(result > 0.5, 1, 0)
cm <- table(testdata$Class, result_Approved, dnn = c("實際", "預測"))
cm 

#(6)準確率
#計算核準卡正確率
cm[4] / sum(cm[, 2])

#計算拒補件正確率
cm[1] / sum(cm[, 1])

#整體準確率(對角線元素總和/所有觀察值總和)
accuracy <- sum(diag(cm)) / sum(cm)
accuracy

#畫ROC曲線
install.packages("ROCR")
library("ROCR")
pred <- prediction(result, testdata$Class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#計算AUC
auc <- performance(pred, "auc")

#畫圖
plot(perf, col = rainbow(7), main = "ROC curve", xlab = "Specificity(FPR)", ylab = "Sensitivity(TPR)")
#AUC = 0.5
abline(0, 1)
#實際AUC值
text(0.5, 0.5, as.character(auc@y.values[[1]]))
