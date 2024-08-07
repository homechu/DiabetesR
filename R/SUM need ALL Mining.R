install.packages("e1071")
library(e1071)

svm.model <- svm(Class ~ ., data = diabete.train, type='C-classification', cost = 10, gamma = 10)
summary(svm.model)

tuned <- tune.svm(Class ~., data = diabete.train, gamma = 10^(-3:-1), cost = 10^(-1:1)) 
summary(tuned)

svm.model.new <- svm(Class ~ ., data = diabete.train, type='C-classification', cost = 10, gamma = 0.01)
svm.pred.new <- predict(svm.model.new, newdata=diabete.train,type="class") 
tableTrain <- table(actual=diabete.train$Class,predict=svm.pred.new)
result[5,switch(1,2,4)] <-paste0(round((sum(tableTrain)-sum(diag(tableTrain)))*100/sum(tableTrain),2),"%")

svm.pred.new <- predict(svm.model.new, newdata=diabete.test,type="class") 
tableTest <- table(actual=diabete.test$Class,predict=svm.pred.new)
result[5,switch(1,3,5)]  <-paste0(round((sum(tableTest)-sum(diag(tableTest)))*100/sum(tableTest),2),"%")
