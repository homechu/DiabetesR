install.packages("rattle")
library(rattle)
rattle()

write.csv(result, file = "", row.names = FALSE)

pima8<-result
pima7<-result
pima6<-result
pima5<-result
pima4<-result


install.packages("penalizedSVM")
library(penalizedSVM)
fit.scad<- svm.fs(x = diabete.train[,-9],y=diabete.train[,9], fs.method="scad", cross.outer= 0,
                   grid.search = "discrete", lambda1.set=lambda1.scad[1:3], show="none",
                   parms.coding = "none", maxIter=10,
                   inner.val.method = "cv", cross.inner= 5, seed=seed, verbose=FALSE)
print(model)
print.1norm.svm(model)