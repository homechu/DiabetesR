install.packages("C50")
install.packages("party")
install.packages("partykit")
install.packages("caret")
library(C50)
library(party)
library(partykit)


diabete <- read.table("./pima_text.csv", header = TRUE, sep = ",", encoding = "UTF-8") # encoding 是指定檔案的文字編碼
diabete$Class<-as.factor(diabete$Class)
str(diabete$Class)

n=0.25*nrow(diabete)
test.index=sample(1:nrow(diabete),n)
diabete.train=diabete[-test.index,]
diabete.test=diabete[test.index,]

#nontraindata
diabete.tree=C5.0(Class~ . ,data=diabete)
plot(diabete.tree)
#


diabete.tree=C5.0(Class~ . ,data=diabete.train)
summary(diabete.tree)#tree details

Class.test=diabete$Class[test.index]
test.pred=predict(diabete.tree,diabete.test,type='class')
table.test=table(Class.test,test.pred)
table.test
cat("Total records(test)=",nrow(diabete.test),"\n")
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")


