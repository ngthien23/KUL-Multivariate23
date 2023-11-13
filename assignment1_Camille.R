install.packages("lavaan")
library(lavaan)
#library(psych) #fa()
#library(paran) #procedure Horn


load("ess.Rdata")
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
'

#compute convariance matrix
covmat <- cov(cess[,2:14])
fitcfa1 <- cfa(cfa1, sample.cov = covmat, sample.nobs=4046)
#summary of results
summary(fitcfa1, fit.measures=TRUE)
#ask for standardized Solution
standardizedSolution(fitcfa1)

# Standardized factor loadings for the latent variables
loadings_social_trust <- c(0.684, 0.648, 0.626)
loadings_trust_institutions <- c(0.789, 0.718, 0.581, 0.802)
loadings_well_being <- c(0.661, 0.670, 0.589, 0.718, 0.677, 0.595)

# Calculate the reliability of each indicator (square of loadings)
reliability_social_trust <- loadings_social_trust^2
reliability_trust_institutions <- loadings_trust_institutions^2
reliability_well_being <- loadings_well_being^2

# Display the reliability of each indicator
reliability_social_trust
reliability_trust_institutions
reliability_well_being

#residual variance
RV_social_trust <- c(0.533, 0.580, 0.608)
RV_trust_institutions <- c(0.377, 0.485, 0.662,0.357 )
RV_well_being <- c(0.562, 0.551, 0.654, 0.484, 0.542, 0.646)

# Calculate composite reliability for each latent construct
composite_reliability_social_trust <- sum(loadings_social_trust^2) / (sum(loadings_social_trust^2) + sum(RV_social_trust))
composite_reliability_trust_institutions <- sum(loadings_trust_institutions^2) / (sum(loadings_trust_institutions^2) + sum(RV_trust_institutions))
composite_reliability_well_being <- sum(loadings_well_being^2) / (sum(loadings_well_being^2) + sum(RV_well_being))

# Display the composite reliability for each latent construct
composite_reliability_social_trust
composite_reliability_trust_institutions
composite_reliability_well_being

#print fitmeasures
fitmeasures(fitcfa1,c("cfi","tli","rmsea","srmr"))

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


#print redundancies (this tells us how much variance in Y is explained by X)
redu <- redundancy(cancor.sess)
round(redu$Xcan.redun, 3)
round(redu$Ycan.redun, 3)

#print canonical loadings
cancor.sess$structure$X.xscores
cancor.sess$structure$Y.yscores

#plot 
plot(-5,-5,xlim=c(-2,4),ylim=c(-2,4),xlab="u1",ylab="t1")
points(cancor.sess$scores$X[sess$cntry=="FR",1],cancor.sess$scores$Y[sess$cntry=="FR",1],col="red")
#cancor.sess$scores$X[sess$cntry=="FR",1]---b times X variables when country is FR, which is u1
points(cancor.sess$scores$X[sess$cntry=="GB",1],cancor.sess$scores$Y[sess$cntry=="GB",1],col="blue")
legend("topleft",c("FR","GB"),col=c("red","blue"),pch=c(1,1))

cancor.sess$scores$X
 
sess$cntry[2000]
sess$cntry[3046]

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

# we  need to compute R(T,T*), R(U,U*) for t1,t2,u1,u2
round(cor(trainT,T_star)[1:2,1:2],3)
round(cor(trainU,U_star)[1:2,1:2],3)
# we  need to compute R(U*,T*), R(U,T)
round(cor(U_star,T_star)[1:2,1:2],3)
round(cor(trainU,trainT)[1:2,1:2],3)
# we  need to compute R(U*,U*), R(T*,T*)
round(cor(U_star,U_star)[1:2,1:2],3)
round(cor(T_star,T_star)[1:2,1:2],3)


###############################
#####on regular data
#conduct canonical correlation analysis
cancor.ess <- cancor(cbind(fltdpr, fltsd, fltanx, wrhpp, enjlf, fltpcfl)
                      ~ppltrst+pplfair+pplhlp+trstprl+trstlgl+trstplc+trstplt, 
                      data=ess)

#print summary result
summary(cancor.ess)

#print canonical loadings
cancor.ess$structure$X.xscores
cancor.ess$structure$Y.yscores

#print redundancies (this tells us how much variance in Y is explained by X)
redu <- redundancy(cancor.ess)
round(redu$Xcan.redun, 3)
round(redu$Ycan.redun, 3)


#e. Use the split-half approach to assess the validity of the solution. 
# Assign even- numbered observations to the calibration set and assign odd-numbered observations to the validation set 
#when conducting this analysis. Discuss what you can conclude about the validity of the solution.
#validation analysis

#split data and standardize data 
train<-ess[seq(2,4046,by=2),]
valid<-ess[seq(1,4046,by=2),]
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


