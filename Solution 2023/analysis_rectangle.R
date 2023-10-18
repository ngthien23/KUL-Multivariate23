library(smacof)

#analysis rectangles

#ratio 
m1<-smacofSym(delta=rectangles, ndim=2, type="ratio", init="torgerson")
#interval
m2<-smacofSym(delta=rectangles, ndim=2, type="interval", init="torgerson")
#ordinal
m3<-smacofSym(delta=rectangles, ndim=2, type="ordinal", init="torgerson")
#mspline
m4<-smacofSym(delta=rectangles, ndim=2, type="mspline",spline.degree =4 , spline.intKnots = 4, init="torgerson")

#stress-1 values
round(c(m1$stress,m2$stress,m3$stress,m4$stress),3)

plot(m3)
plot(m3,plot.type="resplot")
plot(m3,plot.type="Shepard")



#stress norm
set.seed(1)
rstress<-randomstress(n=16,ndim=2,nrep=500,type="ordinal")
#distribution of stress for random data
mean(rstress)-2*sd(rstress)

#permutation test
set.seed(1)
perm.rectangle<-permtest(m3,nrep=500)
mean(perm.rectangle$stressvec)-2*sd(perm.rectangle$stressvec)


#plot distribution stress
par(mfrow=c(1,2))
hist(rstress,main="stress random data")
hist(perm.rectangle$stressvec,main="stress permuted data")


#stability of solution using jackknife
jack.car<-jackmds(m3)
plot(jack.car,xlim=c(-1.2,1.2),ylim=c(-1,1))


birect <- biplotmds(m3, rect_constr)   
summary(birect)

plot(birect, main = "Biplot Vector Representation", vecscale = 0.6, 
  xlim = c(-2, 2), ylim=c(-2,2), vec.conf = list(col = "brown"), pch = 20, cex = 0.5)


