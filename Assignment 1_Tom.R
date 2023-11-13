library(psych)
library(lavaan)
library(candisc)
library(dplyr)

# Load the data from the 'ess.Rdata' file
load('ess.Rdata')

#social trust = ppltrst, pplfair, pplhlp
#trust institutions = trstprl, trstlgl, trstplc, trstplt
#well-being = fltdpr, fltsd, fltanx, wrhpp, enjlf, fltpcfl 

centered_data <- ess %>%
  mutate(across(2:14, ~ . - mean(., na.rm = TRUE)))

covmat<-cov(centered_data[-1])

compositerel<-function(x){
  A<-(sum(x))^2
  B<-sum(1-x^2)
  return(A/(A+B))
}
##################################
#question A 
##################################

# CFA Model
cfa1 <-' social_trust =~ NA*ppltrst + pplfair + pplhlp
         trust_institutions =~ NA*trstprl + trstlgl + trstplc + trstplt
         well_being =~ NA*fltdpr + fltsd + fltanx
         
         social_trust ~~ 1*social_trust
         trust_institutions ~~ 1*trust_institutions
         well_being ~~ 1*well_being
      '

fitcfa1 <-cfa(cfa1,sample.cov=covmat,sample.nobs=4046)

#Summary of results
summary(fitcfa1,fit.measures=TRUE)




# Validity
standard_fitcfa1 <- standardizedSolution(fitcfa1)
factorscore <- c("social trust", "trust institutions", "well being")
standard_fitcfa1
# Convergent Validity
# Average Variance Extracted (AVE)
ave <- c(mean(standard_fitcfa1[1:3, 4]^2), mean(standard_fitcfa1[4:7, 4]^2), mean(standard_fitcfa1[8:10, 4]^2))

# Maximum Shared Variance (MSV)
msv <- c(max(standard_fitcfa1[c(1:3), 4]^2), max(standard_fitcfa1[c(4:7), 4]^2), max(standard_fitcfa1[c(8:10), 4]^2))

# Composite Reliability
reliability <- c(compositerel(standard_fitcfa1[1:3, 4]), compositerel(standard_fitcfa1[4:7, 4]), compositerel(standard_fitcfa1[8:10, 4]))

convergent_validity <- data.frame(factorscore, AVE = ave, MSV = msv, CR = reliability)
convergent_validity

#Fit measures
fitmeasures(fitcfa1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
convergent_validity


##################################
#question B
##################################

modificationIndices(fitcfa1)

cfa2 <-' social_trust =~ NA*ppltrst + pplfair + pplhlp
         trust_institutions =~ NA*trstprl + trstlgl + trstplc + trstplt
         well_being =~ NA*fltdpr + fltsd + fltanx
         
         social_trust ~~ 1*social_trust
         trust_institutions ~~ 1*trust_institutions
         well_being ~~ 1*well_being
         
         trstprl ~~ trstplc
         trstlgl ~~	trstplc
         trstlgl ~~	trstplt
         trstplc ~~	trstplt
         trstprl ~~ trstplt
         trstprl ~~ trstlgl
      '

fitcfa2 <-cfa(cfa2,sample.cov=covmat,sample.nobs=4046)
fitmeasures(fitcfa2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

##################################
#question C
##################################

# Define configural model
sem1 <- '
  #latent variable
  social_trust =~ 1*ppltrst + pplfair + pplhlp
  trust_institutions =~ 1*trstprl + trstlgl + trstplc + trstplt
  well_being =~ 1*fltdpr + fltsd + fltanx

  # Regression
  well_being ~ social_trust + trust_institutions
'

# Fit configural model
config_1 <- sem(sem1, data = centered_data, group = "cntry")

# Fit model 2 equal regression coefficients 
config_2 <- sem(sem1, data = centered_data, group = "cntry", group.equal = c("regressions"))

# Summarize fit measures
fitconfig_1 <- fitmeasures(config_1, c("chisq", "df", "cfi", "tli", "rmsea", "srmr"))
fitconfig_2 <- fitmeasures(config_2, c("chisq", "df", "cfi", "tli", "rmsea", "srmr"))
config_1_2 <- rbind(fitconfig_1, fitconfig_2)
rownames(config_1_2) <- c("unequal regressions", "equal regressions")
round(config_1_2, 3)

summary(config_1, fit.measures=TRUE)
standardizedsolution(config_1)
modificationindices(config_1)

# Compare models using LR test
anova(config_1, config_2)

#


sem2 <-' 
    social_trust =~ ppltrst + pplfair + pplhlp
    trust_institutions =~ trstprl + trstlgl + trstplc + trstplt
    well_being =~ fltdpr + fltsd + fltanx


    #Regression with country-specific coefficients
    well_being ~ social_trust + trust_institutions         
'

#Fit metric model (loadings are equal across groups)
metric_1 <- sem(sem2, data = centered_data, group = "cntry",group.equal = c("loadings"))
# Fit the metric measurement invariance model with constrained regression coefficients
metric_2 <- sem(sem2, data = centered_data, group = "cntry", group.equal = c("regressions","loadings"))

# Summarize fit measures
fitmetric_1 <- fitmeasures(metric_1, c("chisq", "df", "cfi", "tli", "rmsea", "srmr"))
fitmetric_2 <- fitmeasures(metric_2, c("chisq", "df", "cfi", "tli", "rmsea", "srmr"))
metric_1_2 <- rbind(fitmetric_1, fitmetric_2)
rownames(metric_1_2) <- c("unequal regressions", "equal regressions")
round(metric_1_2, 3)

modificationindices(metric_1)


# Compare models using LR test
anova(metric_1, metric_2)

