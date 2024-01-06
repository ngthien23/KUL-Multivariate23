library(MASS)
library(randomForest)
library(class)
library(rpart)
library(MASS)
library(e1071)
library(HDclassif)

set.seed(8517)

#Calculate error rate from observed and predicted
errorrate <- function(observed, predicted) {
  tab<-table(observed, predicted)
  errorrate<-1-sum(diag(tab))/sum(tab)
  return(errorrate)
}

#Calculate error rate from model ouput, training and test data
model <- function(model.out, train.data, train.target, test.data, test.target) {
  pred.train <- predict(model.out, train.data)
  pred.test <- predict(model.out, test.data)
  train <- errorrate(train.target, pred.train$class)
  test <- errorrate(test.target, pred.test$class)
  return(list(train = train, test = test))
}

#Tune k for KNN
tuneknn <- function(train.data, train.target, test.data, test.target, kmax) {
  knn<-matrix(rep(0,kmax*2),nrow=kmax)
  for (j in 1:kmax){
    predknn.train<- knn(train.data, train.data, train.target, k=j)
    knn[j,1]<-errorrate(train.target,predknn.train)
    
    predknn.test<- knn(train.data, test.data, train.target, k=j)
    knn[j,2]<-errorrate(test.target,predknn.test)
  }
  return(knn)
}

#Plot KNN as a function of k
plotknn <- function(knn, kmax) {
  plot(-10,-10,xlim=c(1,kmax),ylim=c(0,0.15),col="red",type="b",xlab="K",ylab="error")
  lines(c(1:kmax),knn[,1],col="red")
  lines(c(1:kmax),knn[,2],col="blue")
  legend("topright",c("training error", "test error"),col=c("red","blue"),lty=c(1,1))
}

#Find the best k for knn, not taking k=1 into account
knnbest <- function(knn) {
  best <- which(knn1[,2] == sort(unique(knn1[,2]))[2])
  return(best)
}

#Calculate error rate for random forest
rferror <- function(train.data, train.target, test.data, test.target, mtry, ntree) {
  rfdata<-data.frame(train.target=factor(train.target),train.data)
  bag.mod=randomForest(train.target~.,data=rfdata,mtry=mtry,ntree=ntree,importance=TRUE)
  predrf.train<-predict(bag.mod,newdata=rfdata)
  train<-errorrate(rfdata$train.target,predrf.train)
  predrf.test<-predict(bag.mod,newdata=test.data)
  test<-errorrate(test.target,predrf.test)
  return(list(train = train, test = test))
}

#Calculate error rate for HDDA
hddaerror <- function(train.data, train.target, test.data, test.target, model, d_select, threshold) {
  hdda.out <- hdda(train.data, train.target, model=model, d_select = d_select, threshold = threshold)
  predhdda.train <- predict(hdda.out, train.data, train.target)
  train<-errorrate(train.target,predhdda.train$class)
  predhdda.test <- predict(hdda.out, test.data, test.target)
  test<-errorrate(test.target,predhdda.test$class)
  return(list(train = train, test = test))
}


#Print results
results <- function() {
  knn1best <- knnbest(knn1)
  knn2best <- knnbest(knn2)
  
  train_errors <- c(lda1$train, lda2$train, 
                    qda1$train, qda2$train, 
                    knn1[knn1best,1], knn2[knn2best,1], 
                    rf1$train, rf2$train, 
                    hdda1$train, hdda2$train)
  test_errors <- c(lda1$test, lda2$test, 
                  qda1$test, qda2$tes, 
                  knn1[knn1best,2], knn2[knn2best,2], 
                  rf1$test, rf2$test, 
                  hdda1$test, hdda2$test)
  row_names <- c("LDA1", "LDA2", 
                "QDA1", "QDA2", 
                "KNN1", "KNN2", 
                "RF1", "RF2", 
                "HDDA1", "HDDA2")

  results <- data.frame(train = train_errors, test = test_errors)
  rownames(results) <- row_names
  print(results)
  print("The best KNN1 has k = ")
  print(knn1best)
  print("The best KNN2 has k = ")
  print(knn2best)
  print("The best model is: ")
  print(rownames(results.out)[which.min(results.out$test)])
  barplot(t(results), beside = TRUE, col = c("red", "blue"), legend.text = TRUE,
          main = "Model Performance on Training and Test Sets",
          xlab = "Models", ylab = "Error rate", ylim = c(0, 0.15))
  return(results)
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
knn1 <- tuneknn(pcatrain1, train.target, pcatest1, test.target, 20)
plotknn(knn1, 20)

#KNN PCA scenario 2 with tuning
knn2 <- tuneknn(pcatrain2, train.target, pcatest2, test.target, 20)
plotknn(knn2, 20)

#Random Forest PCA scenario 1 (p = 43)
rf1<-rferror(pcatrain1, train.target, pcatest1, test.target, 5, 500)

#Random Forest PCA scenario 2 (p = 86)
rf2<-rferror(pcatrain2, train.target, pcatest2, test.target, 5, 500)

# HDDA AKJBKQKD (raw data)
hdda1<-hddaerror(traindata, train.target, testdata, test.target, model="AKJBKQKD", d_select = "Cattell", threshold = 0.05)

# HDDA AKJBQKD (raw data)
hdda2<-hddaerror(traindata, train.target, testdata, test.target, model="AKJBQKD", d_select = "Cattell", threshold = 0.05)


#c)
results.out<-results()
