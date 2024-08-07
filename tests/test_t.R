mongo<-mongo.create(host = "127.0.0.1:27017" ,db="mydb",username="",password="")

rawData <- mongo.find.all(mongo, "mydb.AirqualityUCi",fields =  list('_id'= 0,'NO2(GT)' = 1),data.frame= T)

##°}¦C©î¸Ñ
NO2<-unlist(rawData)

##String to Integer
NO2<-as.numeric(as.character(NO2))


boxplot(NO2)
hist(NO2,7)
plot(NO2,xlim=c(8000,8120), ylim=c(50,350), type="s", pch=1)




