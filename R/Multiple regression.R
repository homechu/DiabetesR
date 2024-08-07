#載入資料
library(ggplot2)
data(mtcars)

#觀察資料
head(mtcars)
#觀察Correlation coefficient
cor(mtcars)[1,]

#整理資料
mycars <- mtcars
#調整欄位值
mycars$am <- ifelse(mtcars$am == 0, "automatic", "manual");
#增加總排氣量
mycars$tdisp <- mtcars$cyl * mtcars$disp
#整理資料
mycars <- mtcars
#調整欄位值
mycars$am <- ifelse(mtcars$am == 0, "automatic", "manual");
#增加總排氣量
mycars$tdisp <- mtcars$cyl * mtcars$disp

#散佈圖 車重 vs 油耗 
ggplot(mycars, aes(x = wt, y = mpg)) + geom_point(aes(color = am))
#散佈圖 馬力 vs 油耗 
ggplot(mycars, aes(x = hp, y = mpg)) + geom_point(aes(color = hp))
#箱型圖觀察 自手排 vs 油耗
ggplot(mycars, aes(x = am, y = mpg)) + geom_boxplot(aes(fill = am))

#multiple regression
mlm <- lm(mpg ~ wt + hp + am + tdisp, data = mycars)
summary(mlm)#P值有點少

#預測  
df <- data.frame(wt = 0.3,hp=14,am="manual",tdisp=150) #光陽酷龍預測值
result <- predict(mlm, newdata = df)
result 
#換算一加侖跑幾英里-> 一公升幾公里
trans <- result * ((result/10)/3.78541178)
trans

mlm <- lm(diabete$Class, data = diabete)
summary(mlm)
