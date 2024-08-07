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
H
G
H%*%G
H%^%2 %*%G
p<-matrix(0,1,8)         #j=-2????
Pmatrix<- matrix(0,8,8) 
post=0                  
j=-1                   
period = 4              

for(x in c(1:8)){
  Pmatrix <- H%^%(2+j)%*%G        
  for(y in c(1:period)){
    post <- post+Pmatrix[x,y]
  }
  p[x] <- post
  post = 0
  
}
p

p<-matrix(0,1,8)        #j=-1????
Pmatrix<- matrix(0,8,8) 
post=0                  
j=-1                    
period = 2              
for(x in c(1:8)){
  Pmatrix <- H%^%(2+j)%*%G        
  for(y in c(1:period)){
    post <- post+Pmatrix[x,y]
  }
  p[x] <- post
  post = 0
  
}
p  
x<-matrix(c(1,0,0,0,1,0,1,0),1,8)    #?Ĥ@??x0~x8????
for(num in c(1:7)){
  xx<-matrix(0,1,8)                  #?ůx?}(?B?⤣??x?z?Z)
  
  if(x[num,1]==1){                   #x?Ĥ@?Ӽƭ?=1?????p
    for(k in c(1:length(x[1,])-1)){
      xx[8]<-x[num,1]                #?Ĥ@?ӼƭȲ??ʨ?xx???̫?  
      xx[k]<-x[num,k+1]              #???@?ӼƭȦs??xx??(???e??)
    }
    x<-rbind(x,xx)                   #?W?[??x??
  }
  
  if(x[num,1]==0){                   #x???Ĥ@?Ӽƭ?=0?????p
    for(k in c(1:length(x[1,])-1))
      xx[k]<-x[num,k+1]
    
    x<-rbind(x,xx)                   #?W?[??x??
  }
  
}
θ <- function(x,y,num){
  dot <- x %*% y[num,] 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot / (norm.x * norm.y))
  as.numeric(theta)
  return(theta)
}
for(num in c(1:8)){
  theta <- θ(p,x,num)
  degree <- theta * 180 / 3.14
  print(degree)
}
degree<-matrix(0,1,8)
for(num in c(1:8)){
  theta<-θ(p,x,num)
  degree[num]<-theta * 180 / 3.14
}
floor(degree[1]+degree[2])


x<-matrix(c(1,0,0,0,1,0,1,0),1,8)    #?Ĥ@??x0~x8????
for(num in c(1:7)){
  xx<-matrix(0,1,8)                  #?ůx?}(?B?⤣??x?z?Z)
  
  if(x[num,1]==1){                   #x?Ĥ@?Ӽƭ?=1?????p
    for(k in c(1:length(x[1,])-1)){
      xx[8]<-x[num,1]                #?Ĥ@?ӼƭȲ??ʨ?xx???̫?  
      xx[k]<-x[num,k+1]              #???@?ӼƭȦs??xx??(???e??)
    }
    x<-rbind(x,xx)                   #?W?[??x??
  }
  
  if(x[num,1]==0){                   #x???Ĥ@?Ӽƭ?=0?????p
    for(k in c(1:length(x[1,])-1))
      xx[k]<-x[num,k+1]
    
    x<-rbind(x,xx)                   #?W?[??x??
  }
  
}
θ <- function(x,y,num){
  dot <- x %*% y[num,] 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot / (norm.x * norm.y))
  as.numeric(theta)
  return(theta)
}
for(num in c(1:8)){
  theta <- θ(p,x,num)
  degree <- theta * 180 / 3.14
  print(degree)
}
degree<-matrix(0,1,8)
for(num in c(1:8)){
  theta<-θ(p,x,num)
  degree[num]<-theta * 180 / 3.14
}
floor(degree[1]+degree[3])
floor(degree[2]+degree[4])


p<-matrix(0,1,8)        #j=0????
Pmatrix<- matrix(0,8,8) 
post=0                  
j=0                    
period = 1              

for(x in c(1:8)){
  Pmatrix <- H%^%(2+j)%*%G        
  for(y in c(1:period)){
    post <- post+Pmatrix[x,y]
  }
  p[x] <- post
  post = 0
  
}
p        
x<-matrix(c(1,0,0,0,1,0,1,0),1,8)    #?Ĥ@??x0~x8????
for(num in c(1:7)){
  xx<-matrix(0,1,8)                  #?ůx?}(?B?⤣??x?z?Z)
  
  if(x[num,1]==1){                   #x?Ĥ@?Ӽƭ?=1?????p
    for(k in c(1:length(x[1,])-1)){
      xx[8]<-x[num,1]                #?Ĥ@?ӼƭȲ??ʨ?xx???̫?  
      xx[k]<-x[num,k+1]              #???@?ӼƭȦs??xx??(???e??)
    }
    x<-rbind(x,xx)                   #?W?[??x??
  }
  
  if(x[num,1]==0){                   #x???Ĥ@?Ӽƭ?=0?????p
    for(k in c(1:length(x[1,])-1))
      xx[k]<-x[num,k+1]
    
    x<-rbind(x,xx)                   #?W?[??x??
  }
  
}
θ <- function(x,y,num){
  dot <- x %*% y[num,] 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot / (norm.x * norm.y))
  as.numeric(theta)
  return(theta)
}
for(num in c(1:8)){
  theta <- θ(p,x,num)
  degree <- theta * 180 / 3.14
  print(degree)
}
degree<-matrix(0,1,8)
for(num in c(1:8)){
  theta<-θ(p,x,num)
  degree[num]<-theta * 180 / 3.14
}
floor(degree[1]+degree[5])
floor(degree[2]+degree[6])
floor(degree[3]+degree[7])
floor(degree[4]+degree[8])

x<-matrix(c(0,0,1,0,1,0,1,0),1,8)    #x'
for(num in c(1:7)){
  xx<-matrix(0,1,8)                  #?ůx?}(?B?⤣??x?z?Z)
  
  if(x[num,1]==1){                   #x?Ĥ@?Ӽƭ?=1?????p
    for(k in c(1:length(x[1,])-1)){
      xx[8]<-x[num,1]                    #?Ĥ@?ӼƭȲ??ʨ?xx???̫?  
      xx[k]<-x[num,k+1]              #???@?ӼƭȦs??xx??(???e??)
    }
    x<-rbind(x,xx)                   #?W?[??x??
  }
  
  if(x[num,1]==0){                   #x???Ĥ@?Ӽƭ?=0?????p
    for(k in c(1:length(x[1,])-1))
      xx[k]<-x[num,k+1]
    
    x<-rbind(x,xx)                   #?W?[??x??
  }
  
}
CV<-matrix(0,3,8)
SV<-matrix(0,3,8)
for(x in c(1:3))       
  for(y in c(1:8)){
    CV[x,y] <- cos(p[x,y])
    SV[x,y] <- sin(p[x,y])
  }
CV<-t(CV)
SV<-t(SV)