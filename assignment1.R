library(psych)
library(lavaan)
library(candisc)
library(dplyr)

#function composite reliability
compositerel<-function(x){
A<-(sum(x))^2
B<-sum(1-x^2)
return(A/(A+B))
}

load("ess.Rdata")
names(ess)[2:14]<-c("sotru1","sotru2","sotru3","truin1","truin2","truin3","truin4",
"webe1","webe2","webe3","webe4","webe5","webe6")

ess <- ess %>%
  mutate(across(2:14, ~ . - mean(., na.rm = TRUE)))

covmat<-cov(ess[-1])

##################################
# Question a
##################################

##specify model with 3 correlated factors 
cfa1<-'sotru =~NA*+sotru1+sotru2+sotru3
       truin =~NA*truin1+truin2+truin3+truin4
       webe  =~NA*webe1+webe2+webe3+webe4+webe5+webe6
       sotru ~~1*sotru
       truin ~~1*truin
       webe  ~~1*webe'

#fit model on covariance matrix
fitcfa1<-cfa(cfa1,sample.cov=covmat,sample.nobs=4046)
#summary of results
summary(fitcfa1,fit.measures=TRUE,std=TRUE)

#print fit measures
fitmeasures(fitcfa1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
#standardized solution
d<-standardizedSolution(fitcfa1)
d

factorscore<-c("sotru","truin","webe")
#composite reliability
reliability<-round(c(compositerel(d[1:3,4]),compositerel(d[4:6,4]),compositerel(d[7:9,4])),3)
#average variance extracted
average_var_extracted<-round(c(mean(d[1:3,4]^2),mean(d[4:6,4]^2),mean(d[7:9,4]^2)),3)
#maximum shared variance
max_shared_var<-round(c(max(d[c(22,23),4]^2),max(d[c(22,24),4]^2),max(d[c(23,24),4]^2)),3)
data.frame(factorscore,reliability,average_var_extracted,max_shared_var)

##################################
# Question c
##################################

sem1<-'sotru =~NA*+sotru1+sotru2+sotru3
       truin =~NA*truin1+truin2+truin3+truin4
       webe  =~NA*webe1+webe2+webe3+webe4+webe5+webe6
       sotru ~~1*sotru
       truin ~~1*truin
       webe  ~~1*webe
       webe ~ c* sotru + b* truin'

# Fit the multi-group SEM model, considering country as the grouping variable
fitsem1 <- sem(sem1, data = ess, group = "cntry")
summary(fitcsem1,fit.measures=TRUE,std=TRUE)

# Summarize the results
fitmeasures(fitsem1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))

sem2<-'sotru =~NA*+sotru1+sotru2+sotru3
       truin =~NA*truin1+truin2+truin3+truin4
       webe  =~NA*webe1+webe2+webe3+webe4+webe5+webe6
       sotru ~~1*sotru
       truin ~~1*truin
       webe  ~~1*webe
       webe ~ c* sotru + b* truin
       c == c
       b == b'

# Fit the multi-group SEM model, considering country as the grouping variable
fitsem2 <- sem(sem2, data = ess, group = "cntry")
summary(fitsem2,fit.measures=TRUE,std=TRUE)

# Summarize the results
fitmeasures(fitsem2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))

