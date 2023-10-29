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
centered_ess <- ess %>%
  mutate(across(2:14, ~ . - mean(., na.rm = TRUE)))

covmat<-cov(centered_ess[-1])

##################################
# Question a: fit centered data
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
#summary(fitcfa1,fit.measures=TRUE,std=TRUE)

#fit measures
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
# Question b: modification indices
# (add correlated errors for pairs of items)
##################################

modificationIndices(fitcfa1)

cfa2<-'sotru =~NA*+sotru1+sotru2+sotru3
       truin =~NA*truin1+truin2+truin3+truin4
       webe  =~NA*webe1+webe2+webe3+webe4+webe5+webe6
       sotru ~~1*sotru
       truin ~~1*truin
       webe  ~~1*webe
       truin1 ~~ truin3
       truin1 ~~ truin4
       truin2 ~~ truin3
       truin2 ~~ truin4
       webe1 ~~ webe2
       webe2 ~~ webe3
       webe3 ~~ webe4
       webe3 ~~ webe5
       webe4 ~~ webe5'

#fit model on covariance matrix
fitcfa2<-cfa(cfa2,sample.cov=covmat,sample.nobs=4046)
#summary(fitcfa1,fit.measures=TRUE,std=TRUE)

#fit measures
fitmeasures(fitcfa2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

##################################
# Question c: fit raw data
##################################

sem1<-'sotru =~NA*+sotru1+sotru2+sotru3
       truin =~NA*truin1+truin2+truin3+truin4
       webe  =~NA*webe1+webe2+webe3+webe4+webe5+webe6
       sotru ~~1*sotru
       truin ~~1*truin
       webe  ~~1*webe
       truin1 ~~ truin3
       truin1 ~~ truin4
       truin2 ~~ truin3
       truin2 ~~ truin4
       webe1 ~~ webe2
       webe2 ~~ webe3
       webe3 ~~ webe4
       webe3 ~~ webe5
       webe4 ~~ webe5
       webe ~ sotru + truin'

sem2<-'sotru =~NA*+sotru1+sotru2+sotru3
       truin =~NA*truin1+truin2+truin3+truin4
       webe  =~NA*webe1+webe2+webe3+webe4+webe5+webe6
       sotru ~~1*sotru
       truin ~~1*truin
       webe  ~~1*webe
       truin1 ~~ a*truin3
       truin1 ~~ b*truin4
       truin2 ~~ c*truin3
       truin2 ~~ d*truin4
       webe1 ~~ e*webe2
       webe2 ~~ f*webe3
       webe3 ~~ g*webe4
       webe3 ~~ h*webe5
       webe4 ~~ i*webe5
       webe ~ j*sotru + k*truin'

# Configural measurement invariance model with country-specific regression
config1 <- sem(sem1, data = ess, group = "cntry")
#summary(config1,fit.measures=TRUE,std=TRUE)
#standardizedSolution(config1)

# Configural measurement invariance model with country-specific regression and equality constraints
config2 <- sem(sem2, data = ess, group = "cntry")
#summary(config2,fit.measures=TRUE,std=TRUE)
#standardizedSolution(config2)

# Metric measurement invariance model with country-specific regression
metric1 <- sem(sem1, data = ess, group = "cntry", group.equal="loadings")
#summary(metric1,fit.measures=TRUE,std=TRUE)
#standardizedSolution(metric1)

# Metric measurement invariance model with country-specific regression and equality constraints
metric2 <- sem(sem2, data = ess, group = "cntry", group.equal="loadings")
#summary(metric2,fit.measures=TRUE,std=TRUE)
#standardizedSolution(metric2)

# Fit measures
fitconfig1 <- fitmeasures(config1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
fitconfig2 <- fitmeasures(config2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
fitmetric1 <- fitmeasures(metric1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
fitmetric2 <- fitmeasures(metric2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
fit<-rbind(fitconfig1,fitconfig2,fitmetric1,fitmetric2)
rownames(fit)<-c("config1","config2","metric1","metric2")
round(fit,3)

# Goodness of fit tests
anova(fitconfig1,fitconfig2)
anova(fitconfig1,fitmetric1)
anova(fitconfig1,fitmetric2)
anova(fitconfig2,fitmetric1)
anova(fitconfig2,fitmetric2)
anova(fitmetric1,fitmetric2)

##################################
# Question d: Canonical correlation analysis
##################################

load("ess.Rdata")
zess<- ess
zess[,2:14]<-scale(ess[,2:14],center=TRUE,scale=FALSE)

cancor.out<-cancor(cbind(fltdpr, fltsd, fltanx, wrhpp, enjlf, fltpcfl)
~ppltrst+ pplfair+ pplhlp+ trstprl+ trstlgl+ trstplc+ trstplt, data=zess)
summary(cancor.out)

#redundancies
redu<-redundancy(cancor.out)
round(redu$Ycan,3)

#computation redundancies from output
R2tu<-cancor.out$cancor^2
VAFYbyt<-apply(cancor.out$structure$Y.yscores^2,2,sum)/4
redund<-R2tu*VAFYbyt
round(cbind(R2tu,VAFYbyt,redund,total=cumsum(redund)),3)

##################################
# Question e: split-half approach to assess the validity
##################################

samplesize<-dim(ess)[1]
train<-ess[seq(2,samplesize,by=2),2:14]
valid<-ess[seq(1,samplesize,by=2),2:14]
train<-as.data.frame(scale(train,center=TRUE,scale=TRUE))
valid<-as.data.frame(scale(valid,center=TRUE,scale=TRUE))

#conduct CCA on training data
cancor.train<-cancor(cbind(fltdpr, fltsd, fltanx, wrhpp, enjlf, fltpcfl)
~ppltrst+ pplfair+ pplhlp+ trstprl+ trstlgl+ trstplc+ trstplt, data=train)
#summary(cancor.train)

#round(cancor.train$structure$X.xscores,3)
#round(cancor.train$structure$Y.yscores,3)

#conduct CCA on validation data
cancor.valid<-cancor(cbind(fltdpr, fltsd, fltanx, wrhpp, enjlf, fltpcfl)
~ppltrst+ pplfair+ pplhlp+ trstprl+ trstlgl+ trstplc+ trstplt, data=valid)

#summary(cancor.valid)
#round(cancor.valid$structure$X.xscores,3)
#round(cancor.valid$structure$Y.yscores,3)

# canonical variates calibration set
train.X1<-cancor.train$score$X
train.Y1<-cancor.train$score$Y

# compute canonical variates using data of calibration set and coefficients estimated on validation set
train.X2<-as.matrix(train[,1:7])%*%cancor.valid$coef$X
train.Y2<-as.matrix(train[,8:13])%*%cancor.valid$coef$Y


#R(T,T*) and R(U,U*)
round(cor(train.Y1,train.Y2)[1:3,1:3],3)
round(cor(train.X1,train.X2)[1:3,1:3],3)

#R(U,T) and R(U*,T*)
round(cor(train.X1,train.Y1)[1:3,1:3],3)
round(cor(train.X2,train.Y2)[1:3,1:3],3)

#R(T*,T*) and R(U*,U*)
round(cor(train.Y2,train.Y2)[1:3,1:3],3)
round(cor(train.X2,train.X2)[1:3,1:3],3)

##################################
# Question f: Which pairs of canonical variates are both important and reliable?
##################################

# canonical loadings first two pairs
as.matrix(round(cancor.out$structure$X.xscores[,1],3))
as.matrix(round(cancor.out$structure$Y.yscores[,1],3))
