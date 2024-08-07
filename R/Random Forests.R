install.packages("C50")
install.packages("party")
install.packages("partykit")
install.packages("caret")
library(C50)
library(party)
library(partykit)
library(caret)

diabete <- read.table("./pima.csv", header = TRUE, sep = ",", encoding = "UTF-8")
diabete$Class<-as.factor(diabete$Class)
str(diabete$Class)

n=0.25*nrow(diabete)
test.index=sample(1:nrow(diabete),n)
diabete.train=diabete[-test.index,]
diabete.test=diabete[test.index,]

result <- data.frame(model=c("naiveBayes","C5.0","CART","ctree","SVM","ANN","bagging","xgboost","random forest","Logistic"),errTrain=rep(0,10),errTest=rep(0,10))
result.10 <-data.frame(model=c("决策树","随机森林","人工神经网络"),errTrain_lm=rep(0,3),errTest_lm=rep(0,3),errTrain_rf=rep(0,3),errTest_rf=rep(0,3))


diabete.naiveBayes <- naiveBayes(Class~.,data = diabete.train)
train_predict <- predict(diabete.naiveBayes,newdata=diabete.train)
test_predict <- predict(diabete.naiveBayes,newdata=diabete.test)
tableTrain <- table(actual=diabete.train$Class,predict=train_predict)
tableTest <- table(actual=diabete.test$Class,predict=test_predict)
errTrain <-paste0(round((sum(tableTrain)-sum(diag(tableTrain)))*100/sum(tableTrain),2),"%")
errTest <-paste0(round((sum(tableTest)-sum(diag(tableTest)))*100/sum(tableTest),2),"%")
result[1,switch(1,2,4)] <-errTrain
result[1,switch(1,3,5)] <-errTest

diabete.C5.0=C5.0(Class~ . ,data=diabete.train)
diabete.rpart <-rpart(Class~.,data=diabete.train)
diabete.ctree <- ctree(Class~.,data=diabete.train)
for(i in 1:3){
  train_predict <-predict(switch(i,diabete.C5.0,diabete.rpart,diabete.ctree),newdata=diabete.train,type=switch(i,"class","class","response"))
  tableTrain <- table(actual=diabete.train$Class,predict=train_predict)
  result[i+1,switch(1,2,4)] <-paste0(round((sum(tableTrain)-sum(diag(tableTrain)))*100/sum(tableTrain),2),"%")

  test_predict <-predict(switch(i,diabete.C5.0,diabete.rpart,diabete.ctree),newdata=diabete.test,type=switch(i,"class","class","response"))
  tableTest <- table(actual=diabete.test$Class,predict=test_predict)
  result[i+1,switch(1,3,5)]  <-paste0(round((sum(tableTest)-sum(diag(tableTest)))*100/sum(tableTest),2),"%")
}

summary(rpart.model)
plot(rpart.model)