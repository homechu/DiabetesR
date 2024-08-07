T<-matrix(0,8,8)
T[1,8]<-1
T[-1,-8]<-diag(1,7,7)
options(digits=11)
h<-matrix(c(0.332670553,0.8068915093,0.4598775021,-0.13501102,-0.0854412739,0.0352262919,0,0),8,1)
g<-matrix(c(0,-0.0352262919,-0.0854412739,0.13501102,0.4598775021,-0.8068915093,0.332670553,0),8,1)
"%^%" <- function(mat, pow) {
  if (!is.matrix(mat)) mat <- as.matrix(mat)
  stopifnot(!diff(dim(mat)))
  if (pow < 0) {
    pow <- -pow
    mat <- solve(mat)
  }
  pow <- round(pow)
  switch(pow + 1, return(diag(1, nrow(mat))), return(mat))
  get.exponents <- function(pow)
    if (pow == 0) NULL else c(k <- 2^floor(log2(pow)), get.exponents(pow - k))
  ans <- diag(nrow(mat))
  dlog2exp <- rev(-diff(c(log2(get.exponents(pow)), 0)))
  for (j in 1:length(dlog2exp)) {
    if (dlog2exp[j]) for (i in 1:dlog2exp[j]) mat <- mat %*% mat
    ans <- ans %*% mat
  }
  ans
}
H<-matrix(c(T%^%0 %*% h,T%^%2 %*% h,T%^%4 %*% h,T%^%6 %*% h,T%^%8 %*% h,T%^%10 %*% h,T%^%12 %*% h,T%^%14 %*% h),8,8)
G<-matrix(c(T%^%0 %*% g,T%^%2 %*% g,T%^%4 %*% g,T%^%6 %*% g,T%^%8 %*% g,T%^%10 %*% g,T%^%12 %*% g,T%^%14 %*% g),8,8)

p<-matrix(0,3,8)        #p1~p8
Pmatrix<- matrix(0,8,8) #暫存矩陣
post=0                  #暫存變數
for(j in c(-2:0))       #J值
for(x in c(1:8)){
  Pmatrix <- H%^%(2+j)%*%G        
      for(y in c(1:switch(j+3,4,2,1))){
      post <- post+Pmatrix[x,y]
      }
   p[j+3,x] <- post
   post = 0
   
}

x<-matrix(c(0,0,1,0,1,0,1,0),1,8) #x'
for(num in c(1:7)){
  xx<-matrix(0,1,8)          #空的矩陣(運算不會受x干擾)
  
  if(x[num,1]==1){           #x的第一個數值=1的情況
   for(k in c(1:length(x[1,])-1)){
     xx[8]<-x[num,1]         #第一個數值移到xx的最後
       xx[k]<-x[num,k+1]     #後一個數值儲存到xx中(往前推)
    }
     x<-rbind(x,xx)          #增加到x中
  }
  
  if(x[num,1]==0){           #x的第一個數值=0的情況
    for(k in c(1:length(x[1,])-1))
      xx[k]<-x[num,k+1]
    
    x<-rbind(x,xx)          #增加到x中
   }
  
}


CV0<-matrix(0,8,1)
CV1<-matrix(0,8,1)
CV2<-matrix(0,8,1)
for(y in c(1:8)){
  CV0[y] <- round(cos((y-1)*pi/4),3) #取小數
  CV1[y] <- round(cos((y-1)*pi/2),3)
  CV2[y] <- cos((y-1)*pi)
}


SV0<-matrix(0,8,1)
SV1<-matrix(0,8,1)
SV2<-matrix(0,8,1)
for(y in c(1:8)){
  SV0[y] <- round(sin((y-1)*pi/4),3) #取小數
  SV1[y] <- round(sin((y-1)*pi/2),3)
  SV2[y] <-  round(sin((y-1)*pi),3)
  
}
j=0
thetajc2<-matrix(0,1,3)
for(j in c(-2:0)){
  n<-p[j+3,] %*% solve(x) %*% switch(j+3,SV2,SV1,SV0) #分子 
  d<-p[j+3,] %*% solve(x) %*% switch(j+3,CV2,CV1,CV0) #分母
  x<-atan(n/d)
  thetajc2[,j+3]<-x
}
thetajc2

θ <- function(x,y,num){
  dot <- x %*% y[num,] 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")a8
  theta <- acos(dot / (norm.x * norm.y))
  as.numeric(theta)
  return(theta)
}
degree<-matrix(0,3,8)

for(j in c(-2:0)){
for(num in c(1:8)){
x <- θ(p[j+3,],x,num)
degree[j+3,num] <- x #改逕度* 180 / 3.14
}
}
degree
floor(degree[1,1]+degree[1,2])


# j=0>>>thetajc2[,3]>>>p[3,]
#點積方程式crossprod()
X1_P0pl<-(x%^%-1)%*%(CV0%*%cos(thetajc2[,3])+SV0%*%sin(thetajc2[,3]))
lpjl<-0
for(num in c(1:8))
  lpjl<-lpjl+(pj1[num]^2)

X1_P0pl<-X1_P0pl/sqrt(lpjl)
X1theta_jc1<-cos(crossprod(p[3,],X1_P0pl))



install.packages("jpeg")
library(jpeg)
pic<-readJPEG('C:/0000.jpg')
pic[,,1]*255 #R
pic[,,2]*255 #G
pic[,,3]*255 #B
write.csv(pic[,,1]*255,file="D:\\DATA_1.csv",row.names = FALSE)
