install.packages("xgboost")
install.packages("adabag")
install.packages("randomForest")
install.packages("list")
library(xgboost)
library(adabag)
library(randomForest)
library(Matrix)
library(list)


diabete.Xtrain <- diabete.train
diabete.Xtest <- diabete.test
xdata = sparse.model.matrix(Class ~ .-1, data = diabete.Xtrain)
xdata.test = sparse.model.matrix(Class ~ .-1, data = diabete.Xtest)
m = nlevels(diabete.Xtrain$Class)
Y = as.integer(diabete.Xtrain$Class) - 1
set.seed(12345)
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m
)
diabete.xgboost <- xgboost(param=param, data=xdata, label=Y, nrounds=20)
Ypred = predict(diabete.xgboost,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
Ypred = levels(diabete.Xtrain$Class)[max.col(Ypred)]
tableTrain <- table(actual=diabete.Xtrain$Class,predict=Ypred)
result[8,switch(1,2,4)] <-paste0(round((sum(tableTrain)-sum(diag(tableTrain)))*100/sum(tableTrain),2),"%")

Ypred = predict(diabete.xgboost,xdata.test)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
Ypred = levels(diabete.Xtrain$Class)[max.col(Ypred)]
tableTest <- table(actual=diabete.Xtest$Class,predict=Ypred)
result[8,switch(1,3,5)]  <-paste0(round((sum(tableTest)-sum(diag(tableTest)))*100/sum(tableTest),2),"%")

diabete.bagging<- bagging(Class~.,data=diabete.train)
diabete.randomForest<- randomForest(Class~.,data=diabete.train)


for(i in 1:2){
  train_predict <-predict(switch(i,diabete.bagging,diabete.randomForest),newdata=diabete.train)
  tableTrain <- table(actual=diabete.train$Class,predict=switch(i,train_predict$class,train_predict))
  result[5+(2*i),switch(1,2,4)] <-paste0(round((sum(tableTrain)-sum(diag(tableTrain)))*100/sum(tableTrain),2),"%")
  
  test_predict <-predict(switch(i,diabete.bagging,diabete.randomForest),newdata=diabete.test)
  tableTest <- table(actual=diabete.test$Class,predict=switch(i,test_predict$class,test_predict))
  result[5+(2*i),switch(1,3,5)]  <-paste0(round((sum(tableTest)-sum(diag(tableTest)))*100/sum(tableTest),2),"%")
}


