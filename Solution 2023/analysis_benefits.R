library(candisc)

load("Solution 2023/benefits.Rdata")
zbenefits<-benefits
zbenefits[,2:14]<-scale(benefits[,2:14],center=TRUE,scale=FALSE)

cancor.out<-cancor(cbind(SL_pensioners, SL_unemployed, SL_old_gvntresp, SL_unemp_gvntresp)
~SB_strain_economy+SB_prevent_poverty+SB_equal_society+SB_taxes_business+SB_make_lazy+SB_caring_others+unemployed_notmotivated+SB_often_lessthanentitled+SB_often_notentitled, data=zbenefits)
summary(cancor.out)

#redundancies
redu<-redundancy(cancor.out)
round(redu$Ycan,3)

#computation redundancies from output
R2tu<-cancor.out$cancor^2
VAFYbyt<-apply(cancor.out$structure$Y.yscores^2,2,sum)/4
redund<-R2tu*VAFYbyt
round(cbind(R2tu,VAFYbyt,redund,total=cumsum(redund)),3)


#split data in two parts and standardize data
samplesize<-dim(benefits)[1]
train<-benefits[seq(2,samplesize,by=2),2:14]
valid<-benefits[seq(1,samplesize,by=2),2:14]
train<-as.data.frame(scale(train,center=TRUE,scale=TRUE))
valid<-as.data.frame(scale(valid,center=TRUE,scale=TRUE))


#conduct CCA on training data
cancor.train<-cancor(cbind(SL_pensioners, SL_unemployed, SL_old_gvntresp, SL_unemp_gvntresp)~SB_strain_economy+SB_prevent_poverty+SB_equal_society+SB_taxes_business+SB_make_lazy+SB_caring_others+unemployed_notmotivated+SB_often_lessthanentitled+SB_often_notentitled, data=train)
#summary(cancor.train)

#round(cancor.train$structure$X.xscores,3)
#round(cancor.train$structure$Y.yscores,3)

#conduct CCA on validation data
cancor.valid<-cancor(cbind(SL_pensioners, SL_unemployed, SL_old_gvntresp, SL_unemp_gvntresp)~SB_strain_economy+SB_prevent_poverty+SB_equal_society+SB_taxes_business+SB_make_lazy+SB_caring_others+unemployed_notmotivated+SB_often_lessthanentitled+SB_often_notentitled, data=valid)

#summary(cancor.valid)
#round(cancor.valid$structure$X.xscores,3)
#round(cancor.valid$structure$Y.yscores,3)

# canonical variates calibration set
train.X1<-cancor.train$score$X
train.Y1<-cancor.train$score$Y

# compute canonical variates using data of calibration set and coefficients estimated on validation set
train.X2<-as.matrix(train[,5:13])%*%cancor.valid$coef$X
train.Y2<-as.matrix(train[,1:4])%*%cancor.valid$coef$Y


#R(T,T*) and R(U,U*)
round(cor(train.Y1,train.Y2)[1:3,1:3],3)
round(cor(train.X1,train.X2)[1:3,1:3],3)

#R(U,T) and R(U*,T*)
round(cor(train.X1,train.Y1)[1:3,1:3],3)
round(cor(train.X2,train.Y2)[1:3,1:3],3)

#R(T*,T*) and R(U*,U*)
round(cor(train.Y2,train.Y2)[1:3,1:3],3)
round(cor(train.X2,train.X2)[1:3,1:3],3)


# canonical loadings first two pairs
as.matrix(round(cancor.out$structure$X.xscores[,1],3))
as.matrix(round(cancor.out$structure$Y.yscores[,1],3))