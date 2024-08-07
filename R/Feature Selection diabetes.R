install.packages('caret')
install.packages('klaR')
install.packages('randomForest')
library("caret")
library("klaR")
library("randomForest")
#使用過濾法構建sbf函數的控制參數(使用樸素貝葉斯函數和BootStrap抽樣方法)

#使用sbf函數進行特徵選擇
sbfControls_nb <- sbfControl(  functions = nbSBF,  method = 'boot')
fs_nb <- sbf(x = diabete.train[,-9],y = diabete.train[,9],sbfControl = sbfControls_nb)
fs_nb$optVariables
#sbf十字交叉
sbfControls_rf <- sbfControl(functions = rfSBF,method = 'cv',repeats = 5)
fs_rf <- sbf(x = diabete.train[,-9],y = diabete.train[,9],sizes = seq(4,19,2),sbfControl = sbfControls_rf)
plot(fs_rf, type = c('g','o'))

#使用封裝法構建rfe函數的控制參數(使用隨機森林函數和BootStrap抽樣方法)
rfeControls_nb <- rfeControl(functions = nbFuncs, method = 'boot')
fs_nbf <- rfe(x = diabete.train[,-9], y = diabete.train[,9], sizes = seq(4,19,2),rfeControl = rfeControls_nb)
fs_nbf
plot(fs_nb, type = c('g','o'))
# rfe十字交叉
rfeControls_rf <- rfeControl(functions = rfFuncs,method = 'cv',repeats = 5)

#rfe(),gafs(),safs():即遞歸特徵刪減法、遺傳算法、蟻群算法

fs_rf <- rfe(x = diabete.train[,-9],y = diabete.train[,9],sizes = seq(4,19,2),rfeControl = rfeControls_rf)
plot(fs_rf, type = c('g','o'))

for(i in 1:2){
  gafsControls <- gafsControl(functions = switch(1,rfGA,treebagGA),
                              method = switch(i,"boot","repeatedcv","cv","LOOCV","LGOCV"),
                              repeats = 5)
  if(i==1) {RFGA<- gafs(x = diabete.train[,-9],y = diabete.train[,9],sizes = seq(4,19,2),gafsControl = gafsControls)}
  if(i==2) {TBCGA<- gafs(x = diabete.train[,-9],y = diabete.train[,9],sizes = seq(4,19,2),gafsControl = gafsControls)}
}

plot(TBCGA)
plot(RFGA)
train_predict <-predict(RFGA, diabete.test,type=switch(3,"class","class","response"))
tableTrain <- table(actual=diabete.test$Class,predict=train_predict)

plot(TBCGA,output = "lattice",auto.key = list(columns = 2))
plot_data <- plot(TBCGA, output = "data")
summary(plot_data)
