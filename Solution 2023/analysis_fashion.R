#library(rhdf5)
#library(MASS)
#library(fBasics) #tr() function to compute the trace of a square matrix

library(HDclassif)
library(mclust)

setwd("C:\\Users\\u0010822\\Box Sync\\onderwijs\\multivariate_statistics_2022_2023\\assignment_2\\fashion")
load("fashion.Rdata")

#save.image("results_fashion.Rdata")

#center training and test data
ctrain.data<-scale(train.data,center=TRUE,scale=FALSE)

#PCA
prcomp.out<-prcomp(ctrain.data)
totvar<- sum(apply(ctrain.data,2,var))
propvar<-prcomp.out$sdev^2/totvar
cumpropvar<-cumsum(propvar)
cumpropvar[6]
cumpropvar[50]

#compute first 6 unstandardized principal components
ncomp<-6
train.comp<-ctrain.data%*%prcomp.out$rotation[,1:ncomp]

#example code hddc 
#hddc.out<-hddc(ctrain.data,K=3,com_dim=2,model="AkjBkQkD")
#adjustedRandIndex(hddc1.out$class,train.target)

#conduct k-means to obtain starting point of hddc
km.init<-kmeans(ctrain.data, centers=3, iter.max = 100, nstart = 20, algorithm = c("Hartigan-Wong"), trace=FALSE)

#make matrix with hddc models that need to be estimated
resulthddc<-data.frame(model=c(rep("AkjBkQkD",5),rep("AkjBQkD",5)),com_dim=c(2:6,2:6),ari=rep(NA,10))
#estimate hddc models and compute ari
for (i in 1:10){
hddc.out<-hddc(ctrain.data,K=3,com_dim=resulthddc[i,2],model=resulthddc[i,1],init.vector=km.init$cluster)
resulthddc[i,3]<-round(adjustedRandIndex(hddc.out$class,train.target),3)}

#example code mclust
#mclust.out<-Mclust(train.comp[,1:2],G=3,modelNames="VVE")
#adjustedRandIndex(train.target,mclust.out$classification)

#make matrix with mclust models that need to be estimated
mclust.options(subset=2000)
ari.runs<-matrix(rep(NA,20*20),ncol=20)
resultmclust<-data.frame(model=c(rep("VVE",5),rep("VEV",5),rep("EVV",5),rep("VVV",5)),ncomp=c(2:6,2:6,2:6,2:6),ari=ari.runs)
for (i in 1:20){
for (j in 1:20){
mclust.out<-Mclust(train.comp[,1:resultmclust[i,2]],G=3,modelNames=resultmclust[i,1])
resultmclust[i,2+j]<-round(adjustedRandIndex(train.target,mclust.out$classification),3)}}
resultmclust$best<-apply(resultmclust[,3:22],1,max)

plot(-1,-1,xlim=c(1,5),ylim=c(0.7,1),axes=FALSE,xlab="com_dim",ylab="ARI")
axis(1,at=c(1:5),labels=c(2:6))
axis(2)
points(c(1:5),resulthddc[1:5,3],col="red",pch=1)
lines(c(1:5),resulthddc[1:5,3],col="red",lty=1)

points(c(1:5),resulthddc[6:10,3],col="red",pch=2)
lines(c(1:5),resulthddc[6:10,3],col="red",lty=2)

points(c(1:5),resultmclust[1:5,"best"],col="blue",pch=1)
lines(c(1:5),resultmclust[1:5,"best"],col="blue",lty=1)

points(c(1:5),resultmclust[6:10,"best"],col="blue",pch=2)
lines(c(1:5),resultmclust[6:10,"best"],col="blue",lty=2)

points(c(1:5),resultmclust[11:15,"best"],col="blue",pch=3)
lines(c(1:5),resultmclust[11:15,"best"],col="blue",lty=3)

points(c(1:5),resultmclust[16:20,"best"],col="blue",pch=4)
lines(c(1:5),resultmclust[16:20,"best"],col="blue",lty=4)

legend("bottomright",c("HDDC AkjBkQkD","HDDC AkjBQkD","MCLUST VVE","MCLUST VEV","MCLUST EVV", "MCLUST VVV"),
col=c("red","red","blue","blue","blue","blue"),
pch=c(1,2,1,2,3,4),lty=c(1,2,1,2,3,4))


#best model is Mclust model EVV on fitted on the first 6 principal components
set.seed(5) #choose seed so that ARI=.924
mclust.options(subset=2000)
mclust.out<-Mclust(train.comp[,1:6],G=3,modelNames="EVV")
print(round(adjustedRandIndex(train.target,mclust.out$classification),3))

#present observed and predicted class labels in the space of the first two principal components

plot(train.comp[,1:2],pch='',main="observed class labels")
points(train.comp[train.target==0,1:2],col="red",pch=19,cex=0.6)
points(train.comp[train.target==1,1:2],col="blue",pch=19,cex=0.6)
points(train.comp[train.target==7,1:2],col="black",pch=19,cex=0.6)
legend("topleft",c("T-shirt/top","Trouser","Sneaker"),pch=c(19,19,19),col=c("red","blue","black"))


plot(train.comp[,1:2],pch='',main="predicted class labels")
points(train.comp[mclust.out$classification==1,1:2],col="blue",pch=19,cex=0.6)
points(train.comp[mclust.out$classification==2,1:2],col="black",pch=19,cex=0.6)
points(train.comp[mclust.out$classification==3,1:2],col="red",pch=19,cex=0.6)
legend("topleft",c("T-shirt/top","Trouser","Sneaker"),pch=c(19,19,19),col=c("red","blue","black"))





