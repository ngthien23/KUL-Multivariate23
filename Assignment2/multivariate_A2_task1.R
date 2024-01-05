library(MASS)
library(randomForest)
library(class)
library(rpart)
library(MASS)
library(e1071)
library(HDclassif)

set.seed(8517)

errorrate <- function(observed, predicted) {
  tab<-table(observed, predicted)
  errorrate<-1-sum(diag(tab))/sum(tab)
  return(errorrate)
}

model <- function(model.out, train.data, train.target, test.data, test.target) {
  pred.train <- predict(model.out, train.data)
  pred.test <- predict(model.out, test.data)
  train.errorrate <- errorrate(train.target, pred.train$class)
  test.errorrate <- errorrate(test.target, pred.test$class)
  return(list(train.errorrate = train.errorrate, test.errorrate = test.errorrate))
}


mnist_task1 <- load("mnist_task1.Rdata")
traindata<-scale(train.data,center=TRUE,scale=FALSE)
testdata<-scale(test.data,center=TRUE,scale=FALSE)

#a) PCA
pctraindata <- prcomp(traindata)
pctestdata <- prcomp(testdata)

totvar<- sum(apply(traindata,2,var))
eigenvalues<-pctraindata$sdev^2
propvar<-eigenvalues/totvar 
cumpropvar<-cumsum(propvar)
cumpropvar

#scenario 1: components account for 80% of the variance in the training data
scen1 <- 43
pcatrain1<-traindata%*%pctraindata$rotation[,1:scen1]
pcatest1<-testdata%*%pctraindata$rotation[,1:scen1] 
#scenario 2: components account for 90% of the variance in the training data
scen2 <- 86
pcatrain2<-traindata%*%pctraindata$rotation[,1:scen2] 
pcatest2<-testdata%*%pctraindata$rotation[,1:scen2] 

#b)
#LDA PCA scenario 1
lda1.out<-lda(pcatrain1,train.target)
lda1 <- model(lda1.out, pcatrain1, train.target, pcatest1, test.target)

#LDA PCA scenario 2
lda2.out<-lda(pcatrain2,train.target)
lda2 <- model(lda2.out, pcatrain2, train.target, pcatest2, test.target)

#QDA PCA scenario 1
qda1.out<-qda(pcatrain1,train.target)
qda1 <- model(qda1.out, pcatrain1, train.target, pcatest1, test.target)

#QDA PCA scenario 2
qda2.out<-qda(pcatrain2,train.target)
qda2 <- model(qda2.out, pcatrain2, train.target, pcatest2, test.target)

#KNN PCA scenario 1 with tuning
knnmax<-100
knn1<-matrix(rep(0,knnmax*2),nrow=knnmax)
for (j in 1:knnmax){
  predknn.train<- knn(pcatrain1, pcatrain1, train.target, k=j)
  knn1[j,1]<-knn1orrate(train.target,predknn.train)

  predknn.test<- knn(pcatrain1, pcatest1, train.target, k=j)
  knn1[j,2]<-knn1orrate(test.target,predknn.test)
}
min(knn1[,2])
knn1
plot(-10,-10,xlim=c(1,knnmax),ylim=c(0,0.15),col="red",type="b",xlab="K",ylab="knn1or")
lines(c(1:knnmax),knn1[,1],col="red")
lines(c(1:knnmax),knn1[,2],col="blue")
legend("topright",c("training eror", "test knn1or"),col=c("red","blue"),lty=c(1,1))


#KNN PCA scenario 2 with tuning
knnmax<-100
knn2<-matrix(rep(0,knnmax*2),nrow=knnmax)
for (j in 1:knnmax){
  predknn.train<- knn(pcatrain2, pcatrain2, train.target, k=j)
  knn2[j,1]<-knn2orrate(train.target,predknn.train)

  predknn.test<- knn(pcatrain2, pcatest2, train.target, k=j)
  knn2[j,2]<-knn2orrate(test.target,predknn.test)
}
min(knn2[,2])
knn2
plot(-10,-10,xlim=c(1,knnmax),ylim=c(0,0.15),col="red",type="b",xlab="K",ylab="knn2or")
lines(c(1:knnmax),knn2[,1],col="red")
lines(c(1:knnmax),knn2[,2],col="blue")
legend("topright",c("training eror", "test knn2or"),col=c("red","blue"),lty=c(1,1))


# HDDA AKJBKQKD
hdda1.out <- hdda(traindata, train.target, model="AKJBKQKD", d_select = "Cattell", threshold = 0.05)
predhdda1.train <- predict(hdda1.out, traindata, train.target)
hdda1.train<-1-sum(diag(predhdda1.train$confusion))/sum(predhdda1.train$confusion)
predhdda1.test <- predict(hdda1.out, testdata, test.target)
hdda1.test<-1-sum(diag(predhdda1.test$confusion))/sum(predhdda1.test$confusion)

# HDDA AKJBQKD
hdda2.out <- hdda(traindata, train.target, model="AKJBQKD", d_select = "Cattell", threshold = 0.05)
predhdda2.train <- predict(hdda2.out, traindata, train.target)
hdda2.train<-1-sum(diag(predhdda2.train$confusion))/sum(predhdda2.train$confusion)
predhdda2.test <- predict(hdda2.out, testdata, test.target)
hdda2.test<-1-sum(diag(predhdda2.test$confusion))/sum(predhdda2.test$confusion)