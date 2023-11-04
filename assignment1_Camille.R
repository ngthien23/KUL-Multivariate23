install.packages("lavaan")
library(lavaan)
#library(psych) #fa()
#library(paran) #procedure Horn


load("/Users/camillecu/Downloads/KUL/multivariate/ess.Rdata")
head(ess)
summary(ess)

#compute centered data
cess<-ess
cess[,2:14]<-scale(ess[,2:14],center=TRUE,scale=FALSE)

##question a
cfa1<- ' 
social_trust =~NA*ppltrst+pplfair+pplhlp
trust_institutions =~NA*trstprl+trstlgl+trstplc+trstplt
well_being =~NA*fltdpr+fltsd+fltanx+wrhpp+enjlf+fltpcfl
social_trust~~1*social_trust
trust_institutions~~1*trust_institutions
well_being~~1*well_being
social_trust~~trust_institutions
social_trust~~well_being
trust_institutions~~well_being
'

#compute convariance matrix
covmat <- cov(cess[,2:14])
fitcfa1 <- cfa(cfa1, sample.cov = covmat, sample.nobs=4046)
#summary of results
summary(fitcfa1, fit.measures=TRUE)
#ask for standardized Solution
standardizedSolution(fitcfa1)

## question b: Use modification indices to see how you can obtain a model that meets the criteria of good fit in
modificationindices(fitcfa1)



#question c
cfa_nocontrain <- ' 
#measurement model
social_trust =~ppltrst+pplfair+pplhlp
trust_institutions =~trstprl+trstlgl+trstplc+trstplt
well_being =~fltdpr+fltsd+fltanx+wrhpp+enjlf+fltpcfl

# Structural Model
well_being ~social_trust + trust_institutions
'


cfa_constrained <- ' 
#measurement model
social_trust =~ppltrst+pplfair+pplhlp
trust_institutions =~trstprl+trstlgl+trstplc+trstplt
well_being =~fltdpr+fltsd+fltanx+wrhpp+enjlf+fltpcfl

# Structural Model with Constrained Coefficients
well_being ~ b1*social_trust + b2*trust_institutions

# Constraints: Constrain b1 and b2 to be equal across countries
[well_being] (1);  # Constrain the coefficient for well_being
[social_trust, trust_institutions] (2);  # Constrain the factor loadings for social_trust and trust_institutions
'

## question c
#1)a configural measurement invariance model with country-specific regression
#coefficients in the regression equation of the structural model
config_nocontrain<-sem(cfa_nocontrain,data=cess,group="cntry") 
summary(config_nocontrain, fit.measures=TRUE)
standardizedsolution(config_nocontrain)

#2) a configural measurement invariance model with regression coefficients that are
#constrained to be equal across countries
config_contrained<-sem(cfa_constrained,data=cess,group="cntry") 
summary(config_contrained, fit.measures=TRUE)
standardizedsolution(config_contrained)

#3)a metric measurement invariance model with country-specific regression
#coefficients in the regression equation of the structural model
metric_nocontrained<-cfa(cfa_nocontrain,data=cess,group="cntry", 
             group.equal="loadings") 
summary(metric_nocontrained, fit.measures=TRUE)
standardizedsolution(metric_nocontrained)

#4) a metric measurement invariance model with regression coefficients that are
#constrained to be equal across countries
metric_constrained<-cfa(cfa_constrained,data=cess,group="cntry", 
             group.equal="loadings") 
summary(metric_constrained, fit.measures=TRUE)
standardizedsolution(metric_constrained)


#fit measures
fitmeasures(config_nocontrain,c("chisq","df","cfi","tli","rmsea","srmr"))
fitmeasures(config_contrained,c("chisq","df","cfi","tli","rmsea","srmr"))
fitmeasures(metric_nocontrained,c("chisq","df","cfi","tli","rmsea","srmr"))
fitmeasures(metric_constrained,c("chisq","df","cfi","tli","rmsea","srmr"))


## d. Conduct a canonical correlation analysis on standardized variables to investigate the relations 
#between the following two sets of variables:

install.packages("candisc")
library(candisc)

#standardize variables
sess<-ess
sess[,2:14]<-scale(ess[,2:14],center=TRUE,scale=TRUE)
head(sess)

#conduct canonical correlation analysis
cancor.sess <- cancor(cbind(fltdpr, fltsd, fltanx, wrhpp, enjlf, fltpcfl)
                     ~ppltrst+pplfair+pplhlp+trstprl+trstlgl+trstplc+trstplt, 
                     data=sess)

#print summary result
summary(cancor.sess)

#print canonical loadings
cancor.sess$structure$X.xscores
cancor.sess$structure$Y.yscores

#print redundancies (this tells us how much variance in Y is explained by X)
redu <- redundancy(cancor.sess)
round(redu$Xcan.redun, 3)
round(redu$Ycan.redun, 3)


#e. Use the split-half approach to assess the validity of the solution. 
# Assign even- numbered observations to the calibration set and assign odd-numbered observations to the validation set 
#when conducting this analysis. Discuss what you can conclude about the validity of the solution.
#validation analysis

#split data and standardize data 
train<-sess[seq(2,4046,by=2),]
valid<-sess[seq(1,4046,by=2),]
train[,2:14]<-scale(train[,2:14],center=TRUE,scale=TRUE)
valid[,2:14]<-scale(valid[,2:14],center=TRUE,scale=TRUE)


#conduct canonical correlation analysis on train data
cancor.train <- cancor(cbind(fltdpr, fltsd, fltanx, wrhpp, enjlf, fltpcfl)
                      ~ppltrst+pplfair+pplhlp+trstprl+trstlgl+trstplc+trstplt, 
                      data=train)
#print summary result
summary(cancor.train)
#print canonical loadings of train 
cancor.train$structure$X.xscores
cancor.train$structure$Y.yscores


#conduct canonical correlation analysis on calibration data
cancor.valid <- cancor(cbind(fltdpr, fltsd, fltanx, wrhpp, enjlf, fltpcfl)
                       ~ppltrst+pplfair+pplhlp+trstprl+trstlgl+trstplc+trstplt, 
                       data=valid)

#print summary result
summary(cancor.valid)

#print canonical loadings of train 
cancor.valid$structure$X.xscores
cancor.valid$structure$Y.yscores


#to obtain U and T from train data
trainU <- cancor.train$scores$X
trainT <- cancor.train$scores$Y


# to compute U* and T*
# U* = Xb*
# T*= Ya*
U_star <-as.matrix(train[,2:8])%*%cancor.valid$coef$X
T_star <-as.matrix(train[,9:14])%*%cancor.valid$coef$Y

# we  need to compute R(T,T*), R(U,U*)
round(cor(TrainT,T_star),3)
round(cor(TrainU,U_star),3)
# we  need to compute R(U*,T*), R(U,T)
round(cor(U_star,T_star),3)
round(cor(trainU,trainT),3)
# we  need to compute R(U*,U*), R(T*,T*)
round(cor(U_star,U_star),3)
round(cor(T_star,T_star),3)



