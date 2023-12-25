mnist_task1 <- load("/Users/camillecu/Downloads/KUL/multivariate/assignment2/mnist_task1.Rdata")
#head(mnist_task1)
#head(train.data)
#head(test.data)
#head(train.target)

dim(train.data)
dim(test.data)

#summary((train.data))
####################################################a. 
#center the train.data
traindata<-scale(train.data,center=TRUE,scale=FALSE)
head(traindata)
#conduct principle componet on ######train data
pctraindata <- prcomp(traindata)
totvar<- sum(apply(traindata,2,var))
# eigenvalues 
eigenvalues<-pctraindata$sdev^2
propvar<-eigenvalues/totvar 
cumpropvar<-cumsum(propvar)
cumpropvar

#scenario 1: components account for 80% of the variance in the training data
##scenario 2: components account for 90% of the variance in the training data
scenario1_train <- 43
scenario2_train <- 86

#conduct principle componnet on #################test data
#center the test.data
testdata<-scale(test.data,center=TRUE,scale=FALSE)

pctestdata <- prcomp(testdata)
#totvar_test<- sum(apply(testdata,2,var))

# eigenvalues 
#eigenvalues_test<-pctestdata$sdev^2
#propvar_test<-eigenvalues_test/totvar_test
#cumpropvar_test<-cumsum(propvar_test)
#cumpropvar_test
#scenario 1: components account for 80% of the variance in the test data
##scenario 2: components account for 90% of the variance in the test data
#scenario1_test <- 42
#scenario2_test <- 84


###############################################b. 
library(MASS)
performance<-function(tab){ 
  hitrate<-sum(diag(tab))/sum(tab) 
  sensitivity<-tab[2,2]/(tab[2,1]+tab[2,2]) 
  specificity<-tab[1,1]/(tab[1,1]+tab[1,2]) 
  performance<-c(hitrate=hitrate, 
                 sensitivity=sensitivity,specificity=specificity) 
} 

#for scenario 1
#LDA conducted on unstandardized principal components of scenario 1
#compute unstandardized principal components 
comp.train_lda1<-traindata%*%pctraindata$rotation[,1:scenario1_train] 
#conduct LDA test on training data
ldacomp.train1<-lda(comp.train_lda1,train.target) 
#training error 
predlda.train1<-predict(ldacomp.train1,comp.train_lda1) 
tab1_train_lda<-table(train.target,predlda.train1$class) 
ldapca.train1<-performance(tab1_train_lda)
ldapca.train1

lda_trainingerror1<- 1-0.87
lda_trainingerror1

#classify test data using LDA model estimated on training data
comp.test_lda1<-testdata%*%pctestdata$rotation[,1:scenario1_train] 
pred.test_lda1 <- predict(ldacomp.train1, comp.test_lda1)
tab1_test<-table(test.target,pred.test_lda1$class) 
ldapca.test1<-performance(tab1_test)
ldapca.test1

lda_testerror1<- 1-0.009
lda_testerror1

#for scenario 2
#LDA conducted on unstandardized principal components of scenario 2
#compute unstandardized principal components 
comp.train2<-traindata%*%pctraindata$rotation[,1:scenario2_train] 
#conduct LDA test on training data
ldacomp.train2<-lda(comp.train2,train.target) 
#training error 
predlda.train2<-predict(ldacomp.train,comp.train) 
tab2<-table(train.target,predlda.train2$class) 
ldapca.train2<-performance(tab2)
ldapca.train2
lda_trainerror2<- 1-0.87
lda_trainerror2

#classify test data using LDA model estimated on training data for scenario 2
comp.test2<-testdata%*%pctestdata$rotation[,1:scenario2_train] 
pred.test2 <- predict(ldacomp.train2, comp.test2)
tab2_test<-table(test.target,pred.test2$class) 
ldapca.test2<-performance(tab2_test)
ldapca.test2

lda_testerror2<- 1-0.082
lda_testerror2


#QDA conducted on the unstandardized principal components of scenario 1 for train data
qdacomp.train1<-qda(comp.train1,train.target) 
predqda.train1<-predict(qdacomp.train1,comp.train1) 
tab_qda1<-table(train.target,predqda.train1$class)
qdapca.train1<-performance(tab_qda1) 
qdapca.train1
qdatrainerror1 <- 1-0.977
qdatrainerror1

#classify test data using QDA model estimated on training data for scenario 1
comp.test_qda1<-testdata%*%pctestdata$rotation[,1:scenario1_train] 
pred.test_qda1 <- predict(qdacomp.train1, comp.test_qda1)
tab1_test_qda <-table(test.target,pred.test_qda1$class) 
qdapca.test1<-performance(tab1_test_qda)
qdapca.test1

qda_testerror1<- 1-0.0466
qda_testerror1


#QDA conducted on the unstandardized principal components of scenario 2 for train data
qdacomp.train2<-qda(comp.train2,train.target) 
predqda.train2<-predict(qdacomp.train2,comp.train2) 
tab_qda2<-table(train.target,predqda.train2$class)
qdapca.train2<-performance(tab_qda2) 
qdapca.train2
qdatrainerror2 <- 1-0.987
qdatrainerror2
#classify test data using QDA model estimated on training data for scenario 2
comp.test_qda2<-testdata%*%pctestdata$rotation[,1:scenario2_train] 
pred.test_qda2 <- predict(qdacomp.train2, comp.test_qda2)
tab2_test_qda <-table(test.target,pred.test_qda2$class) 
qdapca.test2<-performance(tab2_test_qda)
qdapca.test2

qda_testerror2<- 1-0.0732
qda_testerror2


#KNN 
library(randomForest)
library(class)
knnmax<-100


#KNN conducted on the unstandardized principal components of scenario 1



