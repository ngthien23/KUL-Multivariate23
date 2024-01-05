#create training and test data
set.seed(1578)
train<-sample(2000,200)
data.train<-data[train,]
data.test<-data[-train,]
#training and test error rate as a function of k
knnmax<-100
err<-matrix(rep(0,knnmax*2),nrow=knnmax)
for (j in 1:knnmax){
predknn.train<- knn(data.train[,1:2],data.train[,1:2],data.train$y, k=j)
err[j,1]<-1-hitratknn(data.train$y,predknn.train)
predknn.test<- knn(data.train[,1:2], data.test[,1:2], data.train$y, k=j)
err[j,2]<-1-hitratknn(data.test$y,predknn.test)}




grid<-seq(-3,3,by=0.005)
aantV<-length(grid)
t1<-rep(grid,aantV)
t2<-c(t(grid%o%rep(1,aantV)))
ntest<-data.frame(cbind(t1,t2))
colnames(ntest) <- c('x1', 'x1')

#plot knn classification for different values of k

## K=15

ntest$pred <- knn(data.train[,1:2], ntest[,1:2], data.train$y, k=15)
plot (data[,1:2], type='n',xlab="X1",ylab="X2",main="Decision boundary 15-nn")
points(ntest[ntest[,3]==1,1:2],pch='.',col='blue')
points(ntest[ntest[,3]==0,1:2],pch='.',col='red')

## K=50

ntest$pred <- knn(data.train[,1:2], ntest[,1:2], data.train$y, k=50)
plot (data[,1:2], type='n',xlab="X1",ylab="X2",main="Decision boundary 50-nn")
points(ntest[ntest[,3]==1,1:2],pch='.',col='blue')
points(ntest[ntest[,3]==0,1:2],pch='.',col='red')


## K=1

ntest$pred <- knn(data.train[,1:2], ntest[,1:2], data.train$y, k=1)
plot (data[,1:2], type='n',xlab="X1",ylab="X2",main="Decision boundary 1-nn")
points(ntest[ntest[,3]==1,1:2],pch='.',col='blue')
points(ntest[ntest[,3]==0,1:2],pch='.',col='red')


## K=100

ntest$pred <- knn(data.train[,1:2], ntest[,1:2], data.train$y, k=100)
plot (data[,1:2], type='n',xlab="X1",ylab="X2",main="Decision boundary 100-nn")
points(ntest[ntest[,3]==1,1:2],pch='.',col='blue')
points(ntest[ntest[,3]==0,1:2],pch='.',col='red')



#training and test error rate as a function of k

knnmax<-100

err<-matrix(rep(0,knnmax*2),nrow=knnmax)
for (j in 1:knnmax){
predknn.train<- knn(data.train[,1:2], data.train[,1:2], data.train$y, k=j)
err[j,1]<-1-hitratknn(data.train$y,predknn.train)
predknn.test<- knn(data.train[,1:2], data.test[,1:2], data.train$y, k=j)
err[j,2]<-1-hitratknn(data.test$y,predknn.test)}

plot(-10,-10,xlim=c(1,knnmax),ylim=c(0,0.4),col="red",type="b",xlab="K",ylab="error")
lines(c(1:knnmax),err[,1],col="red")
lines(c(1:knnmax),err[,2],col="blue")
abline(h=.2638,col="brown")
abline(h=.2644,col="green")
legend("topright",c("KNN on X1,X2 training set", "KNN on X1, X2 test set", "logistic on X1,X2 test set", "LDA on X1,X2 test set"),col=c("red","blue","brown","green"),lty=c(1,1,1,1))


#plot(data.train[data.train$y==0,1:2],col="red",xlim=c(min(data[,1]),max(data[,1])),ylim=c(min(data[,2]),max(data[,2])),xlab="X1",ylab="X2",cex=1.2,pch=19)
#points(data.train[data.train$y==1,1:2],col="blue",cex=1.2,pch=19)