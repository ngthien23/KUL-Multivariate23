library(psych)
library(lavaan)
library(candisc)

#function composite reliability
compositerel<-function(x){
A<-(sum(x))^2
B<-sum(1-x^2)
return(A/(A+B))
}


load("cosmetics.Rdata")
names(cosmetics)[1:9]<-c("Att_organic1","Att_organic2","Att_organic3","Att_packaging1","Att_packaging2","Att_packaging3",
"Att_crueltyfree1","Att_crueltyfree2","Att_crueltyfree3")

covmat<-cov(cosmetics)


##################################
#analysis attitude
##################################

##specify model with 3 correlated factors 
cfa1<-'Att_organic =~NA*Att_organic1+Att_organic2+Att_organic3
       Att_packaging =~NA*Att_packaging1+Att_packaging2+Att_packaging3
       Att_crueltyfree  =~NA*Att_crueltyfree1+Att_crueltyfree2+Att_crueltyfree3
       Att_organic ~~1*Att_organic
       Att_packaging~~1*Att_packaging
       Att_crueltyfree  ~~1*Att_crueltyfree'
   

#fit model on covariance matrix
fitcfa1<-cfa(cfa1,sample.cov=covmat,sample.nobs=150)
#summary of results
#summary(fitcfa1,fit.measures=TRUE,std=TRUE)

#print fit measures
fitmeasures(fitcfa1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
#standardized solution
standardizedSolution(fitcfa1)

d<-standardizedSolution(fitcfa1)
d[1:3,4]
factorscore<-c("organic","packaging","crueltyfree")
#composite reliability
reliability<-round(c(compositerel(d[1:3,4]),compositerel(d[4:6,4]),compositerel(d[7:9,4])),3)
#average variance extracted
average_var_extracted<-round(c(mean(d[1:3,4]^2),mean(d[4:6,4]^2),mean(d[7:9,4]^2)),3)
#maximum shared variance
max_shared_var<-round(c(max(d[c(22,23),4]^2),max(d[c(22,24),4]^2),max(d[c(23,24),4]^2)),3)
data.frame(factorscore,reliability,average_var_extracted,max_shared_var)


#assume equal residual covariances for pairs of items that focus on the same aspect
cfa2<-'Att_organic =~NA*Att_organic1+Att_organic2+Att_organic3
       Att_packaging =~NA*Att_packaging1+Att_packaging2+Att_packaging3
       Att_crueltyfree  =~NA*Att_crueltyfree1+Att_crueltyfree2+Att_crueltyfree3
       
       Att_organic ~~1*Att_organic
       Att_packaging~~1*Att_packaging
       Att_crueltyfree ~~1*Att_crueltyfree
        
       Att_organic1~~a*Att_packaging1
       Att_organic1~~a*Att_crueltyfree1
       Att_packaging1~~a*Att_crueltyfree1

       Att_organic2~~b*Att_packaging2
       Att_organic2~~b*Att_crueltyfree2
       Att_packaging2~~b*Att_crueltyfree2

       Att_organic3~~c*Att_packaging3
       Att_organic3~~c*Att_crueltyfree3
       Att_packaging3~~c*Att_crueltyfree3'


#fit model on covariance matrix
fitcfa2<-cfa(cfa2,sample.cov=covmat,sample.nobs=150)
summary(fitcfa2,std=TRUE)

#compare models cfa1 and cfa2
#print fit measures
fitm1<-fitmeasures(fitcfa1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
fitm2<-fitmeasures(fitcfa2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
round(rbind(fitm1,fitm2),3)
anova(fitcfa1,fitcfa2)


#impose further constraints on error variances to impose constraint of
#equal residual correlations for pairs of items that focus on the same aspect
cfa3<-'Att_organic =~NA*Att_organic1+Att_organic2+Att_organic3
       Att_packaging =~NA*Att_packaging1+Att_packaging2+Att_packaging3
       Att_crueltyfree =~NA*Att_crueltyfree1+Att_crueltyfree2+Att_crueltyfree3
       
       Att_organic ~~1*Att_organic
       Att_packaging~~1*Att_packaging
       Att_crueltyfree ~~1*Att_crueltyfree
        
       Att_organic1~~a*Att_packaging1
       Att_organic1~~a*Att_crueltyfree1
       Att_packaging1~~a*Att_crueltyfree1

       Att_organic2~~b*Att_packaging2
       Att_organic2~~b*Att_crueltyfree2
       Att_packaging2~~b*Att_crueltyfree2

       Att_organic3~~c*Att_packaging3
       Att_organic3~~c*Att_crueltyfree3
       Att_packaging3~~c*Att_crueltyfree3
       
       Att_organic1~~d*Att_organic1
       Att_organic2~~e*Att_organic2
       Att_organic3~~f*Att_organic3

       Att_packaging1~~d*Att_packaging1
       Att_packaging2~~e*Att_packaging2
       Att_packaging3~~f*Att_packaging3

       Att_crueltyfree1~~d*Att_crueltyfree1
       Att_crueltyfree2~~e*Att_crueltyfree2
       Att_crueltyfree3~~f*Att_crueltyfree3'


#fit model on covariance matrix
fitcfa3<-cfa(cfa3,sample.cov=covmat,sample.nobs=150)

#print fit measures
fitm1<-fitmeasures(fitcfa1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
fitm2<-fitmeasures(fitcfa2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
fitm3<-fitmeasures(fitcfa3,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
round(rbind(fitm1,fitm2,fitm3),3)
anova(fitcfa2,fitcfa3)

summary(fitcfa3,std=TRUE)


##############################
#analysis behavior intention
#############################

##specify model with 3 correlated factors 

cfa1<-'BI_organic =~NA*BI_organic1+BI_organic2+BI_organic3
       BI_packaging =~NA*BI_packaging1+BI_packaging2+BI_packaging3
       BI_crueltyfree  =~NA*BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3

       BI_organic ~~1*BI_organic
       BI_packaging~~1*BI_packaging
       BI_crueltyfree ~~1*BI_crueltyfree'

#fit model on covariance matrix
fitcfa1<-cfa(cfa1,sample.cov=covmat,sample.nobs=150)
#summary of results
#summary(fitcfa1,fit.measures=TRUE)
#print fit measures
fitmeasures(fitcfa1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
#print standardized solution
standardizedSolution(fitcfa1)


d<-standardizedSolution(fitcfa1)
factorscore<-c("organic","packaging","crueltyfree")
#composite reliability
reliability<-round(c(compositerel(d[1:3,4]),compositerel(d[4:6,4]),compositerel(d[7:9,4])),3)
#average variance extracted
average_var_extracted<-round(c(mean(d[1:3,4]^2),mean(d[4:6,4]^2),mean(d[7:9,4]^2)),3)
#maximum shared variance
max_shared_var<-round(c(max(d[c(22,23),4]^2),max(d[c(22,24),4]^2),max(d[c(23,24),4]^2)),3)
data.frame(factorscore,reliability,average_var_extracted,max_shared_var)

#equal residual covariances for all pairs of items that focus on the same aspect 
cfa2<-'BI_organic =~NA*BI_organic1+BI_organic2+BI_organic3
       BI_packaging =~NA*BI_packaging1+BI_packaging2+BI_packaging3
       BI_crueltyfree  =~NA*BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3

       BI_organic ~~1*BI_organic
       BI_packaging~~1*BI_packaging
       BI_crueltyfree  ~~1*BI_crueltyfree

       BI_organic1~~a*BI_packaging1
       BI_organic1~~a*BI_crueltyfree1
       BI_packaging1~~a*BI_crueltyfree1

       BI_organic2~~b*BI_packaging2
       BI_organic2~~b*BI_crueltyfree2
       BI_packaging2~~b*BI_crueltyfree2

       BI_organic3~~c*BI_packaging3
       BI_organic3~~c*BI_crueltyfree3
       BI_packaging3~~c*BI_crueltyfree3'

#fit model on covariance matrix
fitcfa2<-cfa(cfa2,sample.cov=covmat,sample.nobs=150)
summary(fitcfa2,std=TRUE)

#compare models cfa1 and cfa2
#print fit measures
fitm1<-fitmeasures(fitcfa1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
fitm2<-fitmeasures(fitcfa2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
round(rbind(fitm1,fitm2),3)
anova(fitcfa1,fitcfa2)

#equal residual correlations for all pairs of items that focus on the same aspect 
cfa3<-'BI_organic =~NA*BI_organic1+BI_organic2+BI_organic3
       BI_packaging =~NA*BI_packaging1+BI_packaging2+BI_packaging3
       BI_crueltyfree  =~NA*BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3

       BI_organic ~~1*BI_organic
       BI_packaging~~1*BI_packaging
       BI_crueltyfree  ~~1*BI_crueltyfree

       BI_organic1~~a*BI_packaging1
       BI_organic1~~a*BI_crueltyfree1
       BI_packaging1~~a*BI_crueltyfree1

       BI_organic2~~b*BI_packaging2
       BI_organic2~~b*BI_crueltyfree2
       BI_packaging2~~b*BI_crueltyfree2

       BI_organic3~~c*BI_packaging3
       BI_organic3~~c*BI_crueltyfree3
       BI_packaging3~~c*BI_crueltyfree3

       BI_organic1~~d*BI_organic1
       BI_organic2~~e*BI_organic2
       BI_organic3~~f*BI_organic3

       BI_packaging1~~d*BI_packaging1
       BI_packaging2~~e*BI_packaging2
       BI_packaging3~~f*BI_packaging3

       BI_crueltyfree1~~d*BI_crueltyfree1
       BI_crueltyfree2~~e*BI_crueltyfree2
       BI_crueltyfree3~~f*BI_crueltyfree3

'

#fit model on covariance matrix
fitcfa3<-cfa(cfa3,sample.cov=covmat,sample.nobs=150)


#print fit measures
fitm1<-fitmeasures(fitcfa1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
fitm2<-fitmeasures(fitcfa2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
fitm3<-fitmeasures(fitcfa3,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
round(rbind(fitm1,fitm2,fitm3),3)
anova(fitcfa2,fitcfa3)

summary(fitcfa3,std=TRUE)




############################
#structural equation model
#############################

#equal residual covariances for all pairs of items that focus on the same aspect in measurement models

sem1<-'
       #measurement model attitude items
       Att_organic =~Att_organic1+Att_organic2+Att_organic3
       Att_packaging =~Att_packaging1+Att_packaging2+Att_packaging3
       Att_crueltyfree =~Att_crueltyfree1+Att_crueltyfree2+Att_crueltyfree3
       
       Att_organic ~~Att_organic
       Att_packaging~~Att_packaging
       Att_crueltyfree ~~Att_crueltyfree
        
       Att_organic1~~a1*Att_packaging1
       Att_organic1~~a1*Att_crueltyfree1
       Att_packaging1~~a1*Att_crueltyfree1

       Att_organic2~~b1*Att_packaging2
       Att_organic2~~b1*Att_crueltyfree2
       Att_packaging2~~b1*Att_crueltyfree2

       Att_organic3~~c1*Att_packaging3
       Att_organic3~~c1*Att_crueltyfree3
       Att_packaging3~~c1*Att_crueltyfree3
       

      # measurement model behavior-intention items
       BI_organic =~BI_organic1+BI_organic2+BI_organic3
       BI_packaging =~BI_packaging1+BI_packaging2+BI_packaging3
       BI_crueltyfree  =~BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3

       BI_organic ~~BI_organic
       BI_packaging~~BI_packaging
       BI_crueltyfree  ~~BI_crueltyfree

       BI_organic1~~a2*BI_packaging1
       BI_organic1~~a2*BI_crueltyfree1
       BI_packaging1~~a2*BI_crueltyfree1

       BI_organic2~~b2*BI_packaging2
       BI_organic2~~b2*BI_crueltyfree2
       BI_packaging2~~b2*BI_crueltyfree2

       BI_organic3~~c2*BI_packaging3
       BI_organic3~~c2*BI_crueltyfree3
       BI_packaging3~~c2*BI_crueltyfree3


       #structural model
       
       BI_organic~Att_organic
       BI_packaging~Att_packaging
       BI_crueltyfree~Att_crueltyfree'



fitsem1<-sem(sem1,sample.cov=covmat,sample.nobs=150)
semfit1<-fitmeasures(fitsem1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
round(semfit1,3)
summary(fitsem1,fit.measures=TRUE,std=TRUE)


#equal residual covariances for all pairs of items that focus on the same aspect in measurement models
#impose constraint of equal regression coefficients in structural model

sem2<-'
       #measurement model attitude items
       Att_organic =~Att_organic1+Att_organic2+Att_organic3
       Att_packaging =~Att_packaging1+Att_packaging2+Att_packaging3
       Att_crueltyfree =~Att_crueltyfree1+Att_crueltyfree2+Att_crueltyfree3
       
       Att_organic ~~Att_organic
       Att_packaging~~Att_packaging
       Att_crueltyfree ~~Att_crueltyfree
        
       Att_organic1~~a1*Att_packaging1
       Att_organic1~~a1*Att_crueltyfree1
       Att_packaging1~~a1*Att_crueltyfree1

       Att_organic2~~b1*Att_packaging2
       Att_organic2~~b1*Att_crueltyfree2
       Att_packaging2~~b1*Att_crueltyfree2

       Att_organic3~~c1*Att_packaging3
       Att_organic3~~c1*Att_crueltyfree3
       Att_packaging3~~c1*Att_crueltyfree3
       

      # measurement model behavior-intention items
       BI_organic =~BI_organic1+BI_organic2+BI_organic3
       BI_packaging =~BI_packaging1+BI_packaging2+BI_packaging3
       BI_crueltyfree  =~BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3

       BI_organic ~~BI_organic
       BI_packaging~~BI_packaging
       BI_crueltyfree  ~~BI_crueltyfree

       BI_organic1~~a2*BI_packaging1
       BI_organic1~~a2*BI_crueltyfree1
       BI_packaging1~~a2*BI_crueltyfree1

       BI_organic2~~b2*BI_packaging2
       BI_organic2~~b2*BI_crueltyfree2
       BI_packaging2~~b2*BI_crueltyfree2

       BI_organic3~~c2*BI_packaging3
       BI_organic3~~c2*BI_crueltyfree3
       BI_packaging3~~c2*BI_crueltyfree3

       #structural model
       
       BI_organic~beta*Att_organic
       BI_packaging~beta*Att_packaging
       BI_crueltyfree~beta*Att_crueltyfree'

fitsem2<-sem(sem2,sample.cov=covmat,sample.nobs=150)

semfit1<-fitmeasures(fitsem1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
semfit2<-fitmeasures(fitsem2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
round(rbind(semfit1,semfit2),3)
anova(fitsem1,fitsem2)

#summary of results sem2
summary(fitsem2,fit.measures=TRUE,std=TRUE)


#alternative: use measurement models that include
#equal residual correlations for all pairs of items that focus on the same aspect in measurement models

sem1<-'
       #measurement model attitude items
       Att_organic =~Att_organic1+Att_organic2+Att_organic3
       Att_packaging =~Att_packaging1+Att_packaging2+Att_packaging3
       Att_crueltyfree =~Att_crueltyfree1+Att_crueltyfree2+Att_crueltyfree3
       
       Att_organic ~~Att_organic
       Att_packaging~~Att_packaging
       Att_crueltyfree ~~Att_crueltyfree
        
       Att_organic1~~a1*Att_packaging1
       Att_organic1~~a1*Att_crueltyfree1
       Att_packaging1~~a1*Att_crueltyfree1

       Att_organic2~~b1*Att_packaging2
       Att_organic2~~b1*Att_crueltyfree2
       Att_packaging2~~b1*Att_crueltyfree2

       Att_organic3~~c1*Att_packaging3
       Att_organic3~~c1*Att_crueltyfree3
       Att_packaging3~~c1*Att_crueltyfree3
       
       Att_organic1~~d1*Att_organic1
       Att_organic2~~e1*Att_organic2
       Att_organic3~~f1*Att_organic3

       Att_packaging1~~d1*Att_packaging1
       Att_packaging2~~e1*Att_packaging2
       Att_packaging3~~f1*Att_packaging3

       Att_crueltyfree1~~d1*Att_crueltyfree1
       Att_crueltyfree2~~e1*Att_crueltyfree2
       Att_crueltyfree3~~f1*Att_crueltyfree3

      # measurement model behavior-intention items
       BI_organic =~BI_organic1+BI_organic2+BI_organic3
       BI_packaging =~BI_packaging1+BI_packaging2+BI_packaging3
       BI_crueltyfree  =~BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3

       BI_organic ~~BI_organic
       BI_packaging~~BI_packaging
       BI_crueltyfree  ~~BI_crueltyfree

       BI_organic1~~a2*BI_packaging1
       BI_organic1~~a2*BI_crueltyfree1
       BI_packaging1~~a2*BI_crueltyfree1

       BI_organic2~~b2*BI_packaging2
       BI_organic2~~b2*BI_crueltyfree2
       BI_packaging2~~b2*BI_crueltyfree2

       BI_organic3~~c2*BI_packaging3
       BI_organic3~~c2*BI_crueltyfree3
       BI_packaging3~~c2*BI_crueltyfree3

       BI_organic1~~d2*BI_organic1
       BI_organic2~~e2*BI_organic2
       BI_organic3~~f2*BI_organic3

       BI_packaging1~~d2*BI_packaging1
       BI_packaging2~~e2*BI_packaging2
       BI_packaging3~~f2*BI_packaging3

       BI_crueltyfree1~~d2*BI_crueltyfree1
       BI_crueltyfree2~~e2*BI_crueltyfree2
       BI_crueltyfree3~~f2*BI_crueltyfree3

       #structural model
       
       BI_organic~Att_organic
       BI_packaging~Att_packaging
       BI_crueltyfree~Att_crueltyfree'



fitsem1<-sem(sem1,sample.cov=covmat,sample.nobs=150)
semfit1<-fitmeasures(fitsem1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
round(semfit1,3)
summary(fitsem1,fit.measures=TRUE,std=TRUE)


#equal residual correlations for all pairs of items that focus on the same aspect in measurement models
#impose constraint of equal regression coefficients in structural model

sem2<-'
       #measurement model attitude items
       Att_organic =~Att_organic1+Att_organic2+Att_organic3
       Att_packaging =~Att_packaging1+Att_packaging2+Att_packaging3
       Att_crueltyfree =~Att_crueltyfree1+Att_crueltyfree2+Att_crueltyfree3
       
       Att_organic ~~Att_organic
       Att_packaging~~Att_packaging
       Att_crueltyfree ~~Att_crueltyfree
        
       Att_organic1~~a1*Att_packaging1
       Att_organic1~~a1*Att_crueltyfree1
       Att_packaging1~~a1*Att_crueltyfree1

       Att_organic2~~b1*Att_packaging2
       Att_organic2~~b1*Att_crueltyfree2
       Att_packaging2~~b1*Att_crueltyfree2

       Att_organic3~~c1*Att_packaging3
       Att_organic3~~c1*Att_crueltyfree3
       Att_packaging3~~c1*Att_crueltyfree3
       
       Att_organic1~~d1*Att_organic1
       Att_organic2~~e1*Att_organic2
       Att_organic3~~f1*Att_organic3

       Att_packaging1~~d1*Att_packaging1
       Att_packaging2~~e1*Att_packaging2
       Att_packaging3~~f1*Att_packaging3

       Att_crueltyfree1~~d1*Att_crueltyfree1
       Att_crueltyfree2~~e1*Att_crueltyfree2
       Att_crueltyfree3~~f1*Att_crueltyfree3

      # measurement model behavior-intention items
       BI_organic =~BI_organic1+BI_organic2+BI_organic3
       BI_packaging =~BI_packaging1+BI_packaging2+BI_packaging3
       BI_crueltyfree  =~BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3

       BI_organic ~~BI_organic
       BI_packaging~~BI_packaging
       BI_crueltyfree  ~~BI_crueltyfree

       BI_organic1~~a2*BI_packaging1
       BI_organic1~~a2*BI_crueltyfree1
       BI_packaging1~~a2*BI_crueltyfree1

       BI_organic2~~b2*BI_packaging2
       BI_organic2~~b2*BI_crueltyfree2
       BI_packaging2~~b2*BI_crueltyfree2

       BI_organic3~~c2*BI_packaging3
       BI_organic3~~c2*BI_crueltyfree3
       BI_packaging3~~c2*BI_crueltyfree3

       BI_organic1~~d2*BI_organic1
       BI_organic2~~e2*BI_organic2
       BI_organic3~~f2*BI_organic3

       BI_packaging1~~d2*BI_packaging1
       BI_packaging2~~e2*BI_packaging2
       BI_packaging3~~f2*BI_packaging3

       BI_crueltyfree1~~d2*BI_crueltyfree1
       BI_crueltyfree2~~e2*BI_crueltyfree2
       BI_crueltyfree3~~f2*BI_crueltyfree3

       #structural model
       
       BI_organic~beta*Att_organic
       BI_packaging~beta*Att_packaging
       BI_crueltyfree~beta*Att_crueltyfree'

fitsem2<-sem(sem2,sample.cov=covmat,sample.nobs=150)

semfit1<-fitmeasures(fitsem1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
semfit2<-fitmeasures(fitsem2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
round(rbind(semfit1,semfit2),3)
anova(fitsem1,fitsem2)

#summary of results sem2
summary(fitsem2,fit.measures=TRUE,std=TRUE)


