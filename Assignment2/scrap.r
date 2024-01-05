#LDA PCA scenario 1
#compute unstandardized principal components 
comp.train_lda1<-traindata%*%pctraindata$rotation[,1:scen1] 
#conduct LDA test on training data
ldacomp.train1<-lda(pcatrain1,train.target) 
#training error 
predlda.train1<-predict(ldacomp.train1,pcatrain1) 
tab1_train_lda<-table(train.target,predlda.train1$class) 
ldapca.train1<-performance(tab1_train_lda)
ldapca.train1

lda_trainingerror1<- 1-0.87
lda_trainingerror1

#classify test data using LDA model estimated on training data
comp.test_lda1<-testdata%*%pctestdata$rotation[,1:scen1] 
pred.test_lda1 <- predict(ldacomp.train1, comp.test_lda1)
tab1_test<-table(test.target,pred.test_lda1$class) 
ldapca.test1<-performance(tab1_test)
ldapca.test1

lda_testerror1<- 1-0.009
lda_testerror1


#LDA PCa scenario 2
#compute unstandardized principal components 
comp.train2<-traindata%*%pctraindata$rotation[,1:scen2] 
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
comp.test2<-testdata%*%pctestdata$rotation[,1:scen2] 
pred.test2 <- predict(ldacomp.train2, comp.test2)
tab2_test<-table(test.target,pred.test2$class) 
ldapca.test2<-performance(tab2_test)
ldapca.test2

lda_testerror2<- 1-0.082
lda_testerror2






#compute lda on components
lda1.out<-lda(pcatrain1,train.target)

#hit rate training set
predlda1.train<-predict(lda1.out,pcatrain1)
tab<-table(train.target,predlda1.train$class)
lda1.train<-sum(diag(tab))/sum(tab)

#hit rate test set
predlda1.test<-predict(lda1.out,pcatest1)
tab<-table(test.target,predlda1.test$class)
lda1.test<-sum(diag(tab))/sum(tab)



#LDA PCA scenario 2
lda2.out<-lda(pcatrain2,train.target)

#hit rate training set
predlda2.train<-predict(lda2.out,pcatrain2)
tab<-table(train.target,predlda2.train$class)
tab
lda2.train<-sum(diag(tab))/sum(tab)
lda2.train

#hit rate test set
predlda2.test<-predict(lda2.out,pcatest2)
tab<-table(test.target,predlda2.test$class)
tab
lda2.test<-sum(diag(tab))/sum(tab)
lda2.test


#QDA PCA scenario 1
qdacomp.train1<-qda(comp.train1,train.target) 
predqda.train1<-predict(qdacomp.train1,comp.train1) 
tab_qda1<-table(train.target,predqda.train1$class)
qdapca.train1<-performance(tab_qda1) 
qdapca.train1
qdatrainerror1 <- 1-0.977
qdatrainerror1

#test QDA PCA scenario 1
comp.test_qda1<-testdata%*%pctestdata$rotation[,1:scen1] 
pred.test_qda1 <- predict(qdacomp.train1, comp.test_qda1)
tab1_test_qda <-table(test.target,pred.test_qda1$class) 
qdapca.test1<-performance(tab1_test_qda)
qdapca.test1

qda_testerror1<- 1-0.0466
qda_testerror1


#QDA PCA scenario 2
qdacomp.train2<-qda(comp.train2,train.target) 
predqda.train2<-predict(qdacomp.train2,comp.train2) 
tab_qda2<-table(train.target,predqda.train2$class)
qdapca.train2<-performance(tab_qda2) 
qdapca.train2
qdatrainerror2 <- 1-0.987
qdatrainerror2

#test QDA PCA scenario 2
comp.test_qda2<-testdata%*%pctestdata$rotation[,1:scen2] 
pred.test_qda2 <- predict(qdacomp.train2, comp.test_qda2)
tab2_test_qda <-table(test.target,pred.test_qda2$class) 
qdapca.test2<-performance(tab2_test_qda)
qdapca.test2

qda_testerror2<- 1-0.0732
qda_testerror2

#QDA PCA scenario 1
qda1.out<-qda(pcatrain1,train.target)

#hit rate training data
predqda1.train<-predict(qda1.out,pcatrain1)
tab<-table(train.target,predqda1.train$class)
tab
qda1.train<-sum(diag(tab))/sum(tab)
qda1.train

#predqda.loocv<-qda(pcatrain1,train.target[index],CV=TRUE)
#tab<-table(train.target[index],predqda.loocv$class)
#sum(diag(tab))/sum(tab)


#hit rate test data
predqda1.test<-predict(qda1.out,pcatest1)
tab<-table(test.target,predqda.test$class)
tab
qdapca1.test<-sum(diag(tab))/sum(tab)
qdapca1.test