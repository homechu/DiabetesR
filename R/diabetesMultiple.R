#載入資料
install.packages("ggplot2")
library(ggplot2)
data(mtcars)


diabetes <-read.csv("D:/Download/糖尿病資料庫論文/pima_indians_diabetes.csv")
#觀察資料
head(mtcars)
#觀察Correlation coefficient
cor(mydata)[1,]

#整理資料
mydata <- diabetes
#調整欄位值
mydata$change <- ifelse(mydata$change == "No", 0, 1);
mydata$readmitted <- ifelse(mydata$readmitted == "NO", 0, 1);
#增加總排氣量
mycars$tdisp <- mtcars$cyl * mtcars$disp
#整理資料
mycars <- mtcars
#調整欄位值
mycars$am <- ifelse(mtcars$am == 0, "automatic", "manual");
#增加總排氣量
mycars$tdisp <- mtcars$cyl * mtcars$disp

#散佈圖 車重 vs 油耗 
ggplot(mydata, aes(x = BMI, y = age)) + geom_point(aes(color = age))
#散佈圖 馬力 vs 油耗 
ggplot(mycars, aes(x = hp, y = mpg)) + geom_point(aes(color = hp))
#箱型圖觀察 自手排 vs 油耗
ggplot(mycars, aes(x = am, y = mpg)) + geom_boxplot(aes(fill = am))

#multiple regression
mlm <- lm(glucose ~ pregnant+Class+pressure+Triceps+serum_insulin+BMI+Diabetes_pedigree+age , data =mydata)
summary(mlm)#P值有點少

#預測  
df <-data.frame( 
  Class=1,pregnant = 0,  
  pressure = 75, Triceps = 25,
  serum_insulin = 175,BMI = 35,
  Diabetes_pedigree = 1.446,age = 24
) 
result <- predict(mlm, newdata = df)
result 
#換算一加侖跑幾英里-> 一公升幾公里
trans <- result * ((result/10)/3.78541178)
trans

