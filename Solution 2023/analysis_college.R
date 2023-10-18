library(ISLR2)
library(randomForest)
library(MASS)
library(class) ##knn()
library(car)

data(College)

target<-College[,1]
data.raw.center<-scale(College[,2:18],center=TRUE,scale=FALSE)
data.raw.std<-scale(College[,2:18],center=TRUE,scale=TRUE)

data.trans<-College[,2:18]
data.trans[,c(1,2,3,4,6,7,10,11,16)]<-log(data.trans[,c(1,2,3,4,6,7,10,11,16)])
data.trans.center<-scale(data.trans,center=TRUE,scale=FALSE)
data.trans.std<-scale(data.trans,center=TRUE,scale=TRUE)

classif<-function(data,target){

performance<-function(tab){
hitrate<-sum(diag(tab))/sum(tab)
sensitivity<-tab[2,2]/(tab[2,1]+tab[2,2])
specificity<-tab[1,1]/(tab[1,1]+tab[1,2])
performance<-c(hitrate=hitrate, sensitivity=sensitivity,specificity=specificity)
}



# LOOCV error bayes classifier for LDA
pred.test<-lda(data,target,CV=TRUE)
lda.test<-performance(table(target,pred.test$class))

#LOOCV error bayes classifier for QDA
pred.test<-qda(data,target,CV=TRUE)
qda.test<-performance(table(target,pred.test$class))

#Bagging
set.seed(1)
bag.mod=randomForest(target~.,data=data.frame(target,data),mtry=17, ntree=5000,importance=TRUE)
#OOB error bayes classifier
class.test<-ifelse(bag.mod$votes[,2]>0.5,1,0)
bag.test<-performance(table(target,class.test))

#Random Forest
set.seed(1)
rf.mod=randomForest(target~.,data=data.frame(target,data),mtry=4, ntree=5000,importance=TRUE)
#OOB error bayes classifier
class.test<-ifelse(rf.mod$votes[,2]>0.5,1,0)
rf.test<-performance(table(target,class.test))


#knn
knnmax<-100
outknn<-matrix(rep(0,knnmax*3),nrow=knnmax)
set.seed(1)
for (j in 1:knnmax){
predknn<- knn.cv(data, target, k=j)
outknn[j,]<-performance(table(target,predknn))
}
ksel<-which.min(1-outknn[,1])
knn.test<-outknn[ksel,]

classif<-rbind(lda.test,qda.test,bag.test,rf.test,knn.test)
}

raw.center.out<-classif(data.raw.center,target)
raw.std.out<-classif(data.raw.std,target)
trans.center.out<-classif(data.trans.center,target)
trans.std.out<-classif(data.trans.std,target)

#visualize results
plotresult<-function(column,statistic,lower,upper)
{ 
plot(-1,-1,xlim=c(1,5),ylim=c(lower,upper),axes=FALSE,xlab="method",ylab=statistic,main=statistic)
axis(1,at=c(1:5),labels=c("LDA","QDA","Bagging","RF","KNN"))
axis(2)
points(c(1:5),raw.center.out[,column],col="red",pch=1)
lines(c(1:5),raw.center.out[,column],col="red",lty=1)

points(c(1:5),raw.std.out[,column],col="red",lty=2,pch=2)
lines(c(1:5),raw.std.out[,column],col="red",lty=2)

points(c(1:5),trans.center.out[,column],col="blue",pch=1)
lines(c(1:5),trans.center.out[,column],col="blue",lty=1)


points(c(1:5),trans.std.out[,column],col="blue",pch=2)
lines(c(1:5),trans.std.out[,column],col="blue",lty=2)

legend("bottomleft",c("raw.center","raw.std","trans.center","trans.std"),col=c("red","red","blue","blue"),pch=c(1,2,1,2),lty=c(1,2,1,2))
}

plotresult(1,"Hitrate",0.8,1)
plotresult(2,"sensitivity",0.85,1)
plotresult(3,"specificity",0.5,1)











