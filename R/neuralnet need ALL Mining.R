install.packages("neuralnet")
library("neuralnet")

nnet.model <- nnet::nnet(Class~.,data = diabete.train,size=5,decay=0.1)
train_predict<- predict(nnet.model, newdata=diabete.train,type="class") 
tableTrain <- table(actual=diabete.train$Class,predict=train_predict)
result[6,switch(1,2,4)]  <-paste0(round((sum(tableTest)-sum(diag(tableTest)))*100/sum(tableTest),2),"%")

test_predict <- predict(nnet.model, newdata=diabete.test,type="class") 
tableTest <- table(actual=diabete.test$Class,predict=test_predict)
result[6,switch(1,3,5)]  <-paste0(round((sum(tableTest)-sum(diag(tableTest)))*100/sum(tableTest),2),"%")

Var1 <- runif(100, 0, 100)
sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1))
net.sqrt <- neuralnet(Sqrt~Var1, sqrt.data, hidden=10,threshold=0.01)
print(net.sqrt)
plot(net.sqrt)

expected.output <- as.data.frame(sqrt((1:10)^2))
#expected.output=cbind(expected.output)

testdata <- as.data.frame((1:10)^2)
nn.result <- compute(net.sqrt, testdata)
print(nn.result)

install.packages("DMwR")
library(DMwR)
regr.eval(expected.output,nn.result$net.result[,1],stats=c('mae','rmse'))
