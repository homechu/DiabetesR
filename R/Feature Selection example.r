install.packages('caret')
install.packages('klaR')
library("caret")
library("klaR")

#使用過濾法構建sbf函數的控制參數(使用樸素貝葉斯函數和BootStrap抽樣方法)
data(churn)
#使用sbf函數進行特徵選擇
sbfControls_nb <- sbfControl(  functions = nbSBF,  method = 'boot')
fs_nb <- sbf(x = churnTrain[,-20],y = churnTrain[,20],sbfControl = sbfControls_nb)
fs_nb$optVariables
#sbf十字交叉
sbfControls_rf <- sbfControl(functions = rfSBF,method = 'cv',repeats = 5)
fs_rf <- sbf(x = churnTrain[,-20],y = churnTrain[,20],sizes = seq(4,19,2),sbfControl = sbfControls_rf)
plot(fs_rf, type = c('g','o'))

#使用封裝法構建rfe函數的控制參數(使用隨機森林函數和BootStrap抽樣方法)
rfeControls_nb <- rfeControl(functions = nbFuncs, method = 'boot')
fs_nb <- rfe(x = churnTrain[,-20], y = churnTrain[,20], sizes = seq(4,19,2),rfeControl = rfeControls_nb)
fs_nb
plot(fs_nb, type = c('g','o'))
# rfe十字交叉
rfeControls_rf <- rfeControl(functions = rfFuncs,method = 'cv',repeats = 5)
#rfe(),gafs(),safs():即遞歸特徵刪減法、遺傳算法、蟻群算法
fs_rf <- rfe(x = churnTrain[,-20],y = churnTrain[,20],sizes = seq(4,19,2),rfeControl = rfeControls_rf)
plot(fs_rf, type = c('g','o'))