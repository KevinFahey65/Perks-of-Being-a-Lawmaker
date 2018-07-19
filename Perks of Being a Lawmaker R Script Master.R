#########################################
## The Perks of Being a Lawmaker       ##
## Kevin Fahey - Dissertation May 2017 ##
## Prepared 2018-04-07                 ##
#########################################

######################
## Clear Everything ##
######################

rm(list=ls())


###########################
## Set Working Directory ##
###########################

# Example: setwd("/File/Subfile")


######################
## Load in Packages ##
######################

library(foreign)
library(sandwich)
library(xtable)
library(lme4)
library(effects)
library(Matching)
library(rgenoud)
library(car)
library(cem)
library(arm)
library(lattice)
library(plm)
library(stargazer)
library(aod)
library(ggplot2)
library(compactr)
library(MASS)
library(stats)
library(dplyr)
library(ecm)
options(scipen=7)
options(digits=3)

#########################
## ClusterMod function ##
#########################

clusterMod<-function(model, cluster)
{
  require(multiwayvcov)
  require(lmtest)
  vcovCL<-cluster.vcov(model, cluster)
  
  coef<-coeftest(model, vcovCL)
  #w<-waldtest(model, vcov = vcovCL, test = "F")
  get_confint<-function(model, vcovCL){
    t<-qt(.975, model$df.residual)
    ct<-coeftest(model, vcovCL)
    cse<-sqrt(diag(vcovCL))
    est<-cbind(ct[,1], cse,ct[,1]-t*ct[,2], ct[,1]+t*ct[,2])
    colnames(est)<-c("Estimate", "Clustered SE","LowerCI","UpperCI")
    return(est)
  }
  ci<-round(get_confint(model, vcovCL),4)
  return(list(coef, ci))
}

#####################
## Read in Dataset ##
#####################


dat <- read.dta("~/2017-1-22 perks of being a lawmaker.dta")

################
## OLS Models ##
################

## Main Model, no fixed effects ##
ols.main<-lm(loginc ~ votemarg + leadership +
               majpty + chair + rules + fintax +
               approp + agriculture + education +
               health + judiciary + highpost + lowpost +
               age + tenure + postgrad + legal + bizman +
               female + white + gdp01, data = dat)
summary(ols.main)

se.cluster.main<-as.matrix(clusterMod(ols.main, dat$year)[[1]])[,2]

se.robust.main<-sqrt(diag(vcovHC(ols.main)))


## Main model, fixed effects ##
ols.fe.main<-lm(loginc ~ votemarg + leadership +
                  majpty + chair + rules + fintax +
                  approp + agriculture + education +
                  health + judiciary +
                  as.factor(year) +
                  as.factor(memberid), data=dat)
summary(ols.fe.main)

se.cluster.fe.main<-as.matrix(clusterMod(ols.fe.main, dat$year)[[1]])[,2]

se.robust.fe.main<-sqrt(diag(vcovHC(ols.fe.main)))



## Electoral Safety Only model, no fixed effects ##
ols.ev<-lm(loginc ~ votemarg + agriculture + education + 
              health + judiciary + highpost + lowpost +
              age + tenure + postgrad + legal +
              bizman + bizman +female + white + 
              gdp01, data = dat)

se.cluster.ev<-as.matrix(clusterMod(ols.ev, dat$year)[[1]])[,2]

se.robust.ev<-sqrt(diag(vcovHC(ols.ev)))


## Electoral Safety Only model, fixed effects ##
ols.ev.fe<-lm(loginc ~ votemarg + agriculture + education + 
             health + judiciary + as.factor(year) +
             as.factor(memberid), data = dat)

se.cluster.ev.fe<-as.matrix(clusterMod(ols.ev.fe, dat$year)[[1]])[,2]

se.robust.ev.fe<-sqrt(diag(vcovHC(ols.ev.fe)))



##Access hypothesis only model, no fixed effects ##
ols.ac<-lm(loginc ~ leadership + majpty + chair +
             rules + fintax + approp + agriculture +
             education + health + judiciary + 
             highpost + lowpost +
             age + tenure + postgrad + legal +
             bizman + bizman +female + white + 
             gdp01, data = dat)

se.cluster.ac<-as.matrix(clusterMod(ols.ac, dat$year)[[1]])[,2]

se.robust.ac<-sqrt(diag(vcovHC(ols.ac)))


##Access hypothesis only model, fixed effects ##
ols.ac.fe<-lm(loginc ~ leadership + majpty + chair + 
                rules + fintax + approp + agriculture +
                education + health+ judiciary +
                as.factor(year) + as.factor(memberid),
              data = dat)

se.cluster.ac.fe<-as.matrix(clusterMod(ols.ac.fe, dat$year)[[1]])[,2]

se.robust.ac.fe<-sqrt(diag(vcovHC(ols.ac.fe)))


## Combine in stargazer, regular standard errors ##

stargazer(ols.main, ols.fe.main, ols.ev, 
          ols.ev.fe, ols.ac, ols.ac.fe,
          se = list(se.cluster.main,
                    se.cluster.fe.main,
                    se.cluster.ev,
                    se.cluster.ev.fe,
                    se.cluster.ac,
                    se.cluster.ac.fe),
          no.space=T,
          keep.stat = c("n", "adj.rsq", "f"),
          omit = c("memberid", "year"),
          dep.var.labels = "Income (2001 $USD)",
          covariate.labels = c("Vote Share",
                               "Party Leaders",
                               "Majority Party",
                               "Committee Chairs",
                               "Rules Committee",
                               "Finance & Tax Committee",
                               "Appropriations Committee",
                               "Agriculture Committee",
                               "Education Committee",
                               "Health Committee",
                               "Judiciary Committee",
                               "Ran For Higher Office",
                               "Ran For Lower Office",
                               "Age",
                               "Tenure",
                               "Post-Graduate Degree",
                               "Legal Career",
                               "Business Career",
                               "Female",
                               "White",
                               "GDP (2001 $USD)",
                               "Intercept"))


#################################
## Error Correction Model      ##
## See Stata Do File           ##
#################################


###########################################################
## Simple propensity score match for Election Increases ##
###########################################################


###############################
## Create Treatment Variable ##
## And Matching Dataset      ##
###############################

Y = dat$difinc
Tr = ifelse(dat$electdif>=0, 1, 0)
X = cbind(dat[,c("ruleslag", "approplag", "fintaxlag", "agrilag", "judiclag", "edulag", "healthlag", "leaderlag", "agelag", "tenurelag", "majpty", "postgrad", "female", "white", "black", "hispanic", "year")])

t<-cbind(Y, Tr, X)
t<-na.omit(t)


############################################
## Estimate Balance on Treatment Variable ##
## With Other Covariates as Predictors    ##
############################################

prop.vote <- glm(Tr ~ ruleslag + approplag + fintaxlag + agrilag + judiclag + edulag + healthlag + leaderlag + agelag + tenurelag + majpty + postgrad + female + white + black + hispanic + year, family = binomial(link = "logit"), data = t)
summary(prop.vote)


########################
## Matching Algorithm ##
########################

Z = cbind(t[,c("agelag", "year")])


#####################################################
## Create a loop to go through all caliper lengths ##
#####################################################

match.ev<-matrix("NA", nrow = 20, ncol = 5)

for(i in 1:20){
  
match1<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=i/10)

match.ev[i,1]<-match1$est[1] # estimate for each caliper #
match.ev[i,2]<-match1$se # standard error for each caliper
match.ev[i,3]<-match1$est[1]/match1$se # z-score for each caliper #
match.ev[i,4]<-match1$caliper[1] # caliper length #
match.ev[i,5]<-match1$wnobs # observations at each caliper #
  
}


#################################################
## return to 0.5 caliper and check for balance ##
#################################################

match1<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper= 0.5)
summary(match1)

bal.vote<-as.data.frame(rbind(t[match1$index.treated,], t[match1$index.control,]))

mvote <- glm(Tr ~ ruleslag + approplag + fintaxlag +
             agrilag + judiclag + edulag + healthlag +
             leaderlag + agelag + tenurelag + majpty +
             postgrad + female + white + black + hispanic + 
             year, data=bal.vote, family=binomial(link="logit"))
voter<-summary(mvote)
se.voter<-sqrt(diag(vcovHC(mvote)))


## t-tests ##
t.test.vote<-matrix(NA, nrow=17, ncol=3)

for(i in 1:17){
  t.test.vote[i,3]<-t.test(bal.vote[i+2][bal.vote$Tr==1,], bal.vote[i+2][bal.vote$Tr==0,])$statistic
  t.test.vote[i,1]<-t.test(bal.vote[i+2][bal.vote$Tr==1,], bal.vote[i+2][bal.vote$Tr==0,])$estimate[1]
  t.test.vote[i,2]<-t.test(bal.vote[i+2][bal.vote$Tr==1,], bal.vote[i+2][bal.vote$Tr==0,])$estimate[2]
}

## qq-plots ##
qqnorm(t.test.vote[,3], pch=16, main="Normal Q-Q Plot, Vote Margin Difference (Any Increase)", xlim=c(-3,3), ylim=c(-1,1))
lines(x=c(-3,3), y=c(-1,1), lwd=1)

## report pre- and post- balance means and variances ##
mb1<-matrix(NA, ncol = 10, nrow = 17)
colnames(mb1) <- c("Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-Value", "Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-value")


## iterate through each covariate to test pre- and post- balance ##
for(i in 1:17){
  mb.ediff<-MatchBalance(Tr~t[,i+2], data=t, match.out = match1, ks = T, nboots = 1000)
  
  # mb1[i,1] <- colnames(t[i+2])
  mb1[i,1] <- mb.ediff$BeforeMatching[[1]]$mean.Tr
  mb1[i,2] <- mb.ediff$BeforeMatching[[1]]$mean.Co
  mb1[i,3] <- mb.ediff$BeforeMatching[[1]]$var.Tr
  mb1[i,4] <- mb.ediff$BeforeMatching[[1]]$var.Co
  mb1[i,5] <- mb.ediff$BeforeMatching[[1]]$p.value
  mb1[i,6] <- mb.ediff$AfterMatching[[1]]$mean.Tr
  mb1[i,7] <- mb.ediff$AfterMatching[[1]]$mean.Co
  mb1[i,8] <- mb.ediff$AfterMatching[[1]]$var.Tr
  mb1[i,9] <- mb.ediff$AfterMatching[[1]]$var.Co
  mb1[i,10]<- mb.ediff$AfterMatching[[1]]$p.value
  
}

## print balance statistics ##
xtable(mb1, digits = 3)


####################################################################################
## Simple propensity score match for Election Increases, 5% increase versus less ###
####################################################################################

Y = dat$difinc
Tr = ifelse(dat$electdif >= 0.05, 1, 0)
X = cbind(dat[,c("ruleslag", "approplag", "fintaxlag", "agrilag", "judiclag", "edulag", "healthlag", "leaderlag", "agelag", "tenurelag", "majpty", "postgrad", "female", "white", "black", "hispanic", "year")])

t<-cbind(Y, Tr, X)
t<-na.omit(t)

############################################
## Estimate Balance on Treatment Variable ##
## With Other Covariates as Predictors    ##
############################################

prop.vote <- glm(Tr ~ ruleslag + approplag + fintaxlag + agrilag + judiclag + edulag + healthlag + leaderlag + agelag + tenurelag + majpty + postgrad + female + white + black + hispanic + year, family = binomial(link = "logit"), data = t)
summary(prop.vote)

########################
## Matching Algorithm ##
########################

Z = cbind(t[,c("judiclag", "agelag", "tenurelag", "black", "hispanic")])

#####################################################
## Create a loop to go through all caliper lengths ##
#####################################################

match.ev<-matrix("NA", nrow = 20, ncol = 5)

for(i in 1:20){
  
  match2<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=i/10)
  
  match.ev[i,1]<-match2$est[1] # estimate for each caliper #
  match.ev[i,2]<-match2$se # standard error for each caliper
  match.ev[i,3]<-match2$est[1]/match2$se # z-score for each caliper #
  match.ev[i,4]<-match2$caliper[1] # caliper length #
  match.ev[i,5]<-match2$wnobs # treated observations at each caliper #
  
}

#################################################
## return to 0.5 caliper and check for balance ##
#################################################

match2<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=0.5)
summary(match2)

## check for balance ##
bal.vote<-as.data.frame(rbind(t[match2$index.treated,], t[match2$index.control,]))

mvote<-glm(Tr ~ ruleslag + approplag + fintaxlag +
             agrilag + judiclag + edulag + healthlag +
             leaderlag + agelag + tenurelag + majpty +
             postgrad + female + white + black + hispanic + 
             year, data=bal.vote, family=binomial(link="logit"))
voter<-summary(mvote)
se.voter<-sqrt(diag(vcovHC(mvote)))


## t-tests ##
t.test.vote<-matrix(NA, nrow=17, ncol=3)

for(i in 1:17){
  t.test.vote[i,3]<-t.test(bal.vote[i+2][bal.vote$Tr==1,], bal.vote[i+2][bal.vote$Tr==0,])$statistic
  t.test.vote[i,1]<-t.test(bal.vote[i+2][bal.vote$Tr==1,], bal.vote[i+2][bal.vote$Tr==0,])$estimate[1]
  t.test.vote[i,2]<-t.test(bal.vote[i+2][bal.vote$Tr==1,], bal.vote[i+2][bal.vote$Tr==0,])$estimate[2]
}


## qq-plots ##
qqnorm(t.test.vote[,3], pch=16, main="Normal Q-Q Plot, Vote Margin Difference (5%)", xlim=c(-3,3), ylim=c(-1,1))
lines(x=c(-3,3), y=c(-1,1), lwd=1)

## report pre- and post- balance means and variances ##
mb1<-matrix(NA, ncol = 10, nrow = 17)
colnames(mb1) <- c("Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-Value", "Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-value")


## iterate through each covariate to test pre- and post- balance ##
for(i in 1:17){
  mb.ediff<-MatchBalance(Tr~t[,i+2], data=t, match.out = match2, ks = T, nboots = 1000)
  
  # mb1[i,1] <- colnames(t[i+2])
  mb1[i,1] <- mb.ediff$BeforeMatching[[1]]$mean.Tr
  mb1[i,2] <- mb.ediff$BeforeMatching[[1]]$mean.Co
  mb1[i,3] <- mb.ediff$BeforeMatching[[1]]$var.Tr
  mb1[i,4] <- mb.ediff$BeforeMatching[[1]]$var.Co
  mb1[i,5] <- mb.ediff$BeforeMatching[[1]]$p.value
  mb1[i,6] <- mb.ediff$AfterMatching[[1]]$mean.Tr
  mb1[i,7] <- mb.ediff$AfterMatching[[1]]$mean.Co
  mb1[i,8] <- mb.ediff$AfterMatching[[1]]$var.Tr
  mb1[i,9] <- mb.ediff$AfterMatching[[1]]$var.Co
  mb1[i,10]<- mb.ediff$AfterMatching[[1]]$p.value
  
}

xtable(mb1, digits = 3)




#####################################################################################
## Simple propensity score match for Election Increases, 10% increase versus less ###
#####################################################################################


###############################
## Create Treatment Variable ##
## And Matching Dataset      ##
###############################

Y = dat$difinc
Tr = ifelse(dat$electdif>=.10, 1, 0)
X = cbind(dat[,c("ruleslag", "approplag", "fintaxlag", "agrilag", "judiclag", "edulag", "healthlag", "leaderlag", "agelag", "tenurelag", "majpty", "postgrad", "female", "white", "black", "hispanic", "year")])

t<-cbind(Y, Tr, X)
t<-na.omit(t)


############################################
## Estimate Balance on Treatment Variable ##
## With Other Covariates as Predictors    ##
############################################

prop.vote <- glm(Tr ~ ruleslag + approplag + fintaxlag + agrilag + judiclag + edulag + healthlag + leaderlag + agelag + tenurelag + majpty + postgrad + female + white + black + hispanic + year, family = binomial(link = "logit"), data = t)
summary(prop.vote)


########################
## Matching Algorithm ##
########################

Z = cbind(t[,c("majpty")])


#####################################################
## Create a loop to go through all caliper lengths ##
#####################################################

match.ev.10<-matrix("NA", nrow = 20, ncol = 5)

for(i in 1:20){
  
match3<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=i/10)
  
  match.ev.10[i,1]<-match3$est[1] # estimate for each caliper #
  match.ev.10[i,2]<-match3$se # standard error for each caliper
  match.ev.10[i,3]<-match3$est[1]/match3$se # z-score for each caliper #
  match.ev.10[i,4]<-match3$caliper[1] # caliper length #
  match.ev.10[i,5]<-match3$wnobs # observations at each caliper #
  
}


#################################################
## return to 0.5 caliper and check for balance ##
#################################################

match3<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=0.5)
summary(match3)


## check for balance ##
bal.vote<-as.data.frame(rbind(t[match3$index.treated,], t[match3$index.control,]))

mvote<-glm(Tr ~ ruleslag + approplag + fintaxlag +
             agrilag + judiclag + edulag + healthlag +
             leaderlag + agelag + tenurelag + majpty +
             postgrad + female + white + black + hispanic + 
             year, data=bal.vote, family=binomial(link="logit"))
voter<-summary(mvote)
se.voter<-sqrt(diag(vcovHC(mvote)))

t.test.vote<-matrix(NA, nrow=17, ncol=3)


## t-tests ##
for(i in 1:17){
  t.test.vote[i,3]<-t.test(bal.vote[i+2][bal.vote$Tr==1,], bal.vote[i+2][bal.vote$Tr==0,])$statistic
  t.test.vote[i,1]<-t.test(bal.vote[i+2][bal.vote$Tr==1,], bal.vote[i+2][bal.vote$Tr==0,])$estimate[1]
  t.test.vote[i,2]<-t.test(bal.vote[i+2][bal.vote$Tr==1,], bal.vote[i+2][bal.vote$Tr==0,])$estimate[2]
}


## qq-plots ##
qqnorm(t.test.vote[,3], pch=16, main="Normal Q-Q Plot, Vote Margin Difference (5%)", xlim=c(-3,3), ylim=c(-1,1))
lines(x=c(-3,3), y=c(-1,1), lwd=1)


## report pre- and post- balance means and variances ##
mb1<-matrix(NA, ncol = 10, nrow = 17)
colnames(mb1) <- c("Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-Value", "Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-value")


## iterate through each covariate to test pre- and post- balance ##
for(i in 1:17){
  mb.ediff<-MatchBalance(Tr~t[,i+2], data=t, match.out = match3, ks = T, nboots = 1000)
  
  # mb1[i,1] <- colnames(t[i+2])
  mb1[i,1] <- mb.ediff$BeforeMatching[[1]]$mean.Tr
  mb1[i,2] <- mb.ediff$BeforeMatching[[1]]$mean.Co
  mb1[i,3] <- mb.ediff$BeforeMatching[[1]]$var.Tr
  mb1[i,4] <- mb.ediff$BeforeMatching[[1]]$var.Co
  mb1[i,5] <- mb.ediff$BeforeMatching[[1]]$p.value
  mb1[i,6] <- mb.ediff$AfterMatching[[1]]$mean.Tr
  mb1[i,7] <- mb.ediff$AfterMatching[[1]]$mean.Co
  mb1[i,8] <- mb.ediff$AfterMatching[[1]]$var.Tr
  mb1[i,9] <- mb.ediff$AfterMatching[[1]]$var.Co
  mb1[i,10]<- mb.ediff$AfterMatching[[1]]$p.value
  
}

xtable(mb1, digits = 3)


#######################################################
## Simple propensity score match for Rules committee ##
#######################################################


###############################
## Create Treatment Variable ##
## And Matching Dataset      ##
###############################

Y = dat$difinc[dat$rulesdif>=0]
Tr = dat$rulesdif[dat$rulesdif>=0]
X = cbind(dat[,c("votelag", "approplag", "fintaxlag", "agrilag", "judiclag", "edulag", "healthlag", "leaderlag", "agelag", "tenurelag", "majpty", "postgrad", "female", "white", "black", "hispanic", "year", "rulesdif")])
X<-X[X$rulesdif>=0,]
X<-X[,c(1:17)]

t<-cbind(Y, Tr, X)
t<-na.omit(t)


############################################
## Estimate Balance on Treatment Variable ##
## With Other Covariates as Predictors    ##
############################################

prop.vote <- glm(Tr ~ votelag + approplag + fintaxlag + agrilag + judiclag + edulag + healthlag + leaderlag + agelag + tenurelag + majpty + postgrad + female + white + black + hispanic + year, family = binomial(link = "logit"), data = t)
summary(prop.vote)


########################
## Matching Algorithm ##
########################

Z = cbind(t[,c("votelag", "agelag")])


#####################################################
## Create a loop to go through all caliper lengths ##
#####################################################

match.rules<-matrix("NA", nrow = 20, ncol = 5)

for(i in 1:20){
  
match4<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=i/10)
  
match.rules[i,1]<-match4$est[1] # estimate for each caliper #
match.rules[i,2]<-match4$se # standard error for each caliper
match.rules[i,3]<-match4$est[1]/match4$se # z-score for each caliper #
match.rules[i,4]<-match4$caliper[1] # caliper length #
match.rules[i,5]<-match4$wnobs # observations at each caliper #
  
}


#################################################
## return to 0.5 caliper and check for balance ##
#################################################

match4<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=0.5)
summary(match4)

bal.rules<-as.data.frame(rbind(t[match4$index.treated,], t[match4$index.control,]))

mrules<-glm(Tr ~ votelag + approplag + fintaxlag +
             agrilag + judiclag + edulag + healthlag +
             leaderlag + agelag + tenurelag + majpty +
             postgrad + female + white + black + hispanic + 
             year, data=bal.rules, family=binomial(link="logit"))
rulesc<-summary(mrules)
se.rules<-sqrt(diag(vcovHC(mrules)))


## t-tests ##
t.test.rules<-matrix(NA, nrow=17, ncol=3)

for(i in 1:17){
  t.test.rules[i,3]<-t.test(bal.rules[i+2][bal.rules$Tr==1,], bal.rules[i+2][bal.rules$Tr==0,])$statistic
  t.test.rules[i,1]<-t.test(bal.rules[i+2][bal.rules$Tr==1,], bal.rules[i+2][bal.rules$Tr==0,])$estimate[1]
  t.test.rules[i,2]<-t.test(bal.rules[i+2][bal.rules$Tr==1,], bal.rules[i+2][bal.rules$Tr==0,])$estimate[2]
}


## qq-plots ##
qqnorm(t.test.rules[,3], pch=16, main="Normal Q-Q Plot, Rules Committee", xlim=c(-3,3), ylim=c(-1,1))
lines(x=c(-3,3), y=c(-1,1), lwd=1)


## report pre- and post- balance means and variances ##
mb1<-matrix(NA, ncol = 10, nrow = 17)
colnames(mb1) <- c("Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-Value", "Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-value")

for(i in 1:17){
  mb.ediff<-MatchBalance(Tr~t[,i+2], data=t, match.out = match4, ks = T, nboots = 1000)
  
  # mb1[i,1] <- colnames(t[i+2])
  mb1[i,1] <- mb.ediff$BeforeMatching[[1]]$mean.Tr
  mb1[i,2] <- mb.ediff$BeforeMatching[[1]]$mean.Co
  mb1[i,3] <- mb.ediff$BeforeMatching[[1]]$var.Tr
  mb1[i,4] <- mb.ediff$BeforeMatching[[1]]$var.Co
  mb1[i,5] <- mb.ediff$BeforeMatching[[1]]$p.value
  mb1[i,6] <- mb.ediff$AfterMatching[[1]]$mean.Tr
  mb1[i,7] <- mb.ediff$AfterMatching[[1]]$mean.Co
  mb1[i,8] <- mb.ediff$AfterMatching[[1]]$var.Tr
  mb1[i,9] <- mb.ediff$AfterMatching[[1]]$var.Co
  mb1[i,10]<- mb.ediff$AfterMatching[[1]]$p.value
  
}

xtable(mb1, digits = 3)


################################################################
## Simple propensity score match for Appropriations committee ##
################################################################


###############################
## Create Treatment Variable ##
## And Matching Dataset      ##
###############################

Y = dat$difinc[dat$apropdif>=0]
Tr = dat$apropdif[dat$apropdif>=0]
X = cbind(dat[,c("votelag", "ruleslag", "fintaxlag", "agrilag", "judiclag", "edulag", "healthlag", "leaderlag", "agelag", "tenurelag", "majpty", "postgrad", "female", "white", "black", "hispanic", "year", "apropdif")])
X<-X[X$apropdif>=0,]
X<-X[,c(1:17)]

t<-cbind(Y, Tr, X)
t<-na.omit(t)


############################################
## Estimate Balance on Treatment Variable ##
## With Other Covariates as Predictors    ##
############################################

prop.vote <- glm(Tr ~ votelag + ruleslag + 
                                fintaxlag + agrilag + judiclag + 
                                edulag + healthlag + leaderlag + 
                                agelag + tenurelag + majpty + 
                                postgrad + female + white + 
                                black + hispanic + year, 
                              family = binomial(link = "logit"), data = t)
summary(prop.vote)


########################
## Matching Algorithm ##
########################

Z = cbind(t[,c("fintaxlag")])


#####################################################
## Create a loop to go through all caliper lengths ##
#####################################################

match.approp<-matrix("NA", nrow = 20, ncol = 5)

for(i in 1:20){
  
  match5<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=i/10)
  
  match.approp[i,1]<-match5$est[1] # estimate for each caliper #
  match.approp[i,2]<-match5$se # standard error for each caliper
  match.approp[i,3]<-match5$est[1]/match5$se # z-score for each caliper #
  match.approp[i,4]<-match5$caliper[1] # caliper length #
  match.approp[i,5]<-match5$wnobs # observations at each caliper #
  
}


#################################################
## return to 0.5 caliper and check for balance ##
#################################################

match5<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=0.5)
summary(match5)

bal.approp<-as.data.frame(rbind(t[match5$index.treated,], t[match5$index.control,]))

mapprop<-glm(Tr ~ votelag + ruleslag + fintaxlag +
              agrilag + judiclag + edulag + healthlag +
              leaderlag + agelag + tenurelag + majpty +
              postgrad + female + white + black + hispanic + 
              year, data=bal.approp, family=binomial(link="logit"))
appropc<-summary(mapprop)
se.approp<-sqrt(diag(vcovHC(mapprop)))


## t-tests ##
t.test.approp<-matrix(NA, nrow=17, ncol=3)

for(i in 1:17){
  t.test.approp[i,3]<-t.test(bal.approp[i+2][bal.approp$Tr==1,], 
                             bal.approp[i+2][bal.approp$Tr==0,])$statistic
  t.test.approp[i,1]<-t.test(bal.approp[i+2][bal.approp$Tr==1,], 
                             bal.approp[i+2][bal.approp$Tr==0,])$estimate[1]
  t.test.approp[i,2]<-t.test(bal.approp[i+2][bal.approp$Tr==1,], 
                             bal.approp[i+2][bal.approp$Tr==0,])$estimate[2]
}


## qq-plots ##
qqnorm(t.test.approp[,3], pch=16, main="Normal Q-Q Plot, Appropriations Committee", 
       xlim=c(-3,3), ylim=c(-1,1))
lines(x=c(-3,3), y=c(-1,1), lwd=1)


## report pre- and post- balance means and variances ##
mb1<-matrix(NA, ncol = 10, nrow = 17)
colnames(mb1) <- c("Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-Value", "Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-value")

for(i in 1:17){
  mb.ediff<-MatchBalance(Tr~t[,i+2], data=t, match.out = match5, ks = T, nboots = 1000)
  
  # mb1[i,1] <- colnames(t[i+2])
  mb1[i,1] <- mb.ediff$BeforeMatching[[1]]$mean.Tr
  mb1[i,2] <- mb.ediff$BeforeMatching[[1]]$mean.Co
  mb1[i,3] <- mb.ediff$BeforeMatching[[1]]$var.Tr
  mb1[i,4] <- mb.ediff$BeforeMatching[[1]]$var.Co
  mb1[i,5] <- mb.ediff$BeforeMatching[[1]]$p.value
  mb1[i,6] <- mb.ediff$AfterMatching[[1]]$mean.Tr
  mb1[i,7] <- mb.ediff$AfterMatching[[1]]$mean.Co
  mb1[i,8] <- mb.ediff$AfterMatching[[1]]$var.Tr
  mb1[i,9] <- mb.ediff$AfterMatching[[1]]$var.Co
  mb1[i,10]<- mb.ediff$AfterMatching[[1]]$p.value
  
}

xtable(mb1, digits = 3)


################################################################
## Simple propensity score match for Finance & Tax committee ##
################################################################


###############################
## Create Treatment Variable ##
## And Matching Dataset      ##
###############################

Y = dat$difinc[dat$fintaxdif>=0]
Tr = dat$fintaxdif[dat$fintaxdif>=0]
X = cbind(dat[,c("votelag", "ruleslag", "approplag", "agrilag", "judiclag", "edulag", "healthlag", "leaderlag", "agelag", "tenurelag", "majpty", "postgrad", "female", "white", "black", "hispanic", "year", "fintaxdif")])
X<-X[X$fintaxdif>=0,]
X<-X[,c(1:17)]

t<-cbind(Y, Tr, X)
t<-na.omit(t)



############################################
## Estimate Balance on Treatment Variable ##
## With Other Covariates as Predictors    ##
############################################

prop.vote <- glm(Tr ~ votelag + ruleslag + 
                   approplag + agrilag + judiclag + 
                   edulag + healthlag + leaderlag + 
                   agelag + tenurelag + majpty + 
                   postgrad + female + white + 
                   black + hispanic + year, 
                 family = binomial(link = "logit"), data = t)
summary(prop.vote)


########################
## Matching Algorithm ##
########################

Z = cbind(t[,c("tenurelag")]) # don't really need to do but can always optimize balance #


#####################################################
## Create a loop to go through all caliper lengths ##
#####################################################

match.fintax<-matrix("NA", nrow = 20, ncol = 5)

for(i in 1:20){
  
  match6<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=i/10)
  
  match.fintax[i,1]<-match6$est[1] # estimate for each caliper #
  match.fintax[i,2]<-match6$se # standard error for each caliper
  match.fintax[i,3]<-match6$est[1]/match6$se # z-score for each caliper #
  match.fintax[i,4]<-match6$caliper[1] # caliper length #
  match.fintax[i,5]<-match6$wnobs # observations at each caliper #
  
}


#################################################
## return to 0.5 caliper and check for balance ##
#################################################

match6<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=0.5)
summary(match6)

bal.fintax<-as.data.frame(rbind(t[match6$index.treated,], t[match6$index.control,]))

mfintax<-glm(Tr ~ votelag + ruleslag + approplag +
               agrilag + judiclag + edulag + healthlag +
               leaderlag + agelag + tenurelag + majpty +
               postgrad + female + white + black + hispanic + 
               year, data=bal.fintax, family=binomial(link="logit"))
fintaxc <- summary(mfintax)
se.fintax<-sqrt(diag(vcovHC(mfintax)))


## t-tests ##
t.test.fintax<-matrix(NA, nrow=17, ncol=3)

for(i in 1:17){
  t.test.fintax[i,3]<-t.test(bal.fintax[i+2][bal.fintax$Tr==1,], 
                             bal.fintax[i+2][bal.fintax$Tr==0,])$statistic
  t.test.fintax[i,1]<-t.test(bal.fintax[i+2][bal.fintax$Tr==1,], 
                             bal.fintax[i+2][bal.fintax$Tr==0,])$estimate[1]
  t.test.fintax[i,2]<-t.test(bal.fintax[i+2][bal.fintax$Tr==1,], 
                             bal.fintax[i+2][bal.fintax$Tr==0,])$estimate[2]
}


## qq-plots ##
qqnorm(t.test.fintax[,3], pch=16, main="Normal Q-Q Plot, Finance and Tax Committee", 
       xlim=c(-3,3), ylim=c(-1,1))
lines(x=c(-3,3), y=c(-1,1), lwd=1)


## report pre- and post- balance means and variances ##
mb1<-matrix(NA, ncol = 10, nrow = 17)
colnames(mb1) <- c("Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-Value", "Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-value")


for(i in 1:17){
  mb.ediff<-MatchBalance(Tr~t[,i+2], data=t, match.out = match6, ks = T, nboots = 1000)
  
  # mb1[i,1] <- colnames(t[i+2])
  mb1[i,1] <- mb.ediff$BeforeMatching[[1]]$mean.Tr
  mb1[i,2] <- mb.ediff$BeforeMatching[[1]]$mean.Co
  mb1[i,3] <- mb.ediff$BeforeMatching[[1]]$var.Tr
  mb1[i,4] <- mb.ediff$BeforeMatching[[1]]$var.Co
  mb1[i,5] <- mb.ediff$BeforeMatching[[1]]$p.value
  mb1[i,6] <- mb.ediff$AfterMatching[[1]]$mean.Tr
  mb1[i,7] <- mb.ediff$AfterMatching[[1]]$mean.Co
  mb1[i,8] <- mb.ediff$AfterMatching[[1]]$var.Tr
  mb1[i,9] <- mb.ediff$AfterMatching[[1]]$var.Co
  mb1[i,10]<- mb.ediff$AfterMatching[[1]]$p.value
  
}


xtable(mb1, digits = 3)


################################################################
## Simple propensity score match for Leadership Position      ##
################################################################


###############################
## Create Treatment Variable ##
## And Matching Dataset      ##
###############################

Y = dat$difinc[dat$leaddif>=0]
Tr = dat$leaddif[dat$leaddif>=0]
X = cbind(dat[,c("votelag", "ruleslag", "approplag", "agrilag", "judiclag", "edulag", "healthlag", "fintaxlag", "agelag", "tenurelag", "majpty", "postgrad", "female", "white", "black", "hispanic", "year", "leaddif")])
X<-X[X$leaddif>=0,]
X<-X[,c(1:17)]

t<-cbind(Y, Tr, X)
t<-na.omit(t)


############################################
## Estimate Balance on Treatment Variable ##
## With Other Covariates as Predictors    ##
############################################

prop.vote <- glm(Tr ~ votelag + ruleslag + 
                   fintaxlag + agrilag + judiclag + 
                   edulag + healthlag + approplag + 
                   agelag + tenurelag + majpty + 
                   postgrad + female + white + 
                   black + hispanic + year, 
                 family = binomial(link = "logit"), data = t)
summary(prop.vote)


########################
## Matching Algorithm ##
########################

Z = cbind(t[,c("ruleslag")])


#####################################################
## For Leaders, to 2 caliper and check for balance ##
#####################################################

match7<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=2)
summary(match7)

bal.lead<-as.data.frame(rbind(t[match7$index.treated,], t[match7$index.control,]))

mlead <- glm(Tr ~ votelag + ruleslag + fintaxlag +
               agrilag + judiclag + edulag + healthlag +
               approplag + agelag + tenurelag + majpty +
               postgrad + female + white + black + hispanic + 
               year, data=bal.lead, family=binomial(link="logit"))
leadc<-summary(mlead)
se.lead<-sqrt(diag(vcovHC(mlead)))


## t-tests ##
t.test.lead<-matrix(NA, nrow=17, ncol=3)

for(i in 1:17){
  t.test.lead[i,3]<-t.test(bal.lead[i+2][bal.lead$Tr==1,], 
                           bal.lead[i+2][bal.lead$Tr==0,])$statistic
  t.test.lead[i,1]<-t.test(bal.lead[i+2][bal.lead$Tr==1,], 
                           bal.lead[i+2][bal.lead$Tr==0,])$estimate[1]
  t.test.lead[i,2]<-t.test(bal.lead[i+2][bal.lead$Tr==1,], 
                           bal.lead[i+2][bal.lead$Tr==0,])$estimate[2]
}


## qq-plots ##
qqnorm(t.test.lead[,3], pch=16, main="Normal Q-Q Plot, Leadership Position", 
       xlim=c(-3,3), ylim=c(-1,1))
lines(x=c(-3,3), y=c(-1,1), lwd=1)


## report pre- and post- balance means and variances ##
mb1<-matrix(NA, ncol = 10, nrow = 17)
colnames(mb1) <- c("Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-Value", "Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-value")

for(i in 1:17){
  mb.ediff<-MatchBalance(Tr~t[,i+2], data=t, match.out = match7, ks = T, nboots = 1000)
  
  # mb1[i,1] <- colnames(t[i+2])
  mb1[i,1] <- mb.ediff$BeforeMatching[[1]]$mean.Tr
  mb1[i,2] <- mb.ediff$BeforeMatching[[1]]$mean.Co
  mb1[i,3] <- mb.ediff$BeforeMatching[[1]]$var.Tr
  mb1[i,4] <- mb.ediff$BeforeMatching[[1]]$var.Co
  mb1[i,5] <- mb.ediff$BeforeMatching[[1]]$p.value
  mb1[i,6] <- mb.ediff$AfterMatching[[1]]$mean.Tr
  mb1[i,7] <- mb.ediff$AfterMatching[[1]]$mean.Co
  mb1[i,8] <- mb.ediff$AfterMatching[[1]]$var.Tr
  mb1[i,9] <- mb.ediff$AfterMatching[[1]]$var.Co
  mb1[i,10]<- mb.ediff$AfterMatching[[1]]$p.value
  
}

xtable(mb1, digits = 3)


################################################################
## Simple propensity score match for Committee Chair          ##
################################################################


###############################
## Create Treatment Variable ##
## And Matching Dataset      ##
###############################

Y = dat$difinc[dat$chairdif>=0]
Tr = dat$chairdif[dat$chairdif>=0]
X = cbind(dat[,c("votelag", "ruleslag", "approplag", "agrilag", "judiclag", "edulag", "healthlag", "fintaxlag", "agelag", "tenurelag", "majpty", "postgrad", "female", "white", "black", "hispanic", "year", "chairdif")])
X<-X[X$chairdif>=0,]
X<-X[,c(1:17)]

t<-cbind(Y, Tr, X)
t<-na.omit(t)


############################################
## Estimate Balance on Treatment Variable ##
## With Other Covariates as Predictors    ##
############################################

prop.vote <- glm(Tr ~ votelag + ruleslag + 
                   fintaxlag + agrilag + judiclag + 
                   edulag + healthlag + approplag + 
                   agelag + tenurelag + majpty + 
                   postgrad + female + white + 
                   black + hispanic + year, 
                 family = binomial(link = "logit"), data = t)
summary(prop.vote)


########################
## Matching Algorithm ##
########################

Z = cbind(t[,c("votelag", "ruleslag", "agelag", "hispanic")])


######################################################
## For Cmte Chairs, 2 caliper and check for balance ##
######################################################

match8<-Match(Y=t$Y, Tr=t$Tr, X=t[,c(3:17)], Z=Z, BiasAdjust=T, estimand="ATT", M=1, replace=T, caliper=2)
summary(match8)


bal.chair<-as.data.frame(rbind(t[match8$index.treated,], t[match8$index.control,]))

mchair <- glm(Tr ~ votelag + ruleslag + fintaxlag +
               agrilag + judiclag + edulag + healthlag +
               approplag + agelag + tenurelag + majpty +
               postgrad + female + white + black + hispanic + 
               year, data=bal.chair, family=binomial(link="logit"))
chairc<-summary(mchair)
se.chair<-sqrt(diag(vcovHC(mchair)))


## t-tests ##
t.test.chair<-matrix(NA, nrow=17, ncol=3)

for(i in 1:17){
  t.test.chair[i,3]<-t.test(bal.chair[i+2][bal.chair$Tr==1,], 
                            bal.chair[i+2][bal.chair$Tr==0,])$statistic
  t.test.chair[i,1]<-t.test(bal.chair[i+2][bal.chair$Tr==1,], 
                            bal.chair[i+2][bal.chair$Tr==0,])$estimate[1]
  t.test.chair[i,2]<-t.test(bal.chair[i+2][bal.chair$Tr==1,], 
                            bal.chair[i+2][bal.chair$Tr==0,])$estimate[2]
}


## qq-plots ##
qqnorm(t.test.lead[,3], pch=16, main="Normal Q-Q Plot, Committee Chairs", 
       xlim=c(-3,3), ylim=c(-1,1))
lines(x=c(-3,3), y=c(-1,1), lwd=1)


## report pre- and post- balance means and variances ##
mb1<-matrix(NA, ncol = 10, nrow = 17)
colnames(mb1) <- c("Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-Value", "Mean, Tr", "Mean, Co", "Var, Tr", "Var, Co", "P-value")

for(i in 1:17){
  mb.ediff<-MatchBalance(Tr~t[,i+2], data=t, match.out = match8, ks = T, nboots = 1000)
  
  # mb1[i,1] <- colnames(t[i+2])
  mb1[i,1] <- mb.ediff$BeforeMatching[[1]]$mean.Tr
  mb1[i,2] <- mb.ediff$BeforeMatching[[1]]$mean.Co
  mb1[i,3] <- mb.ediff$BeforeMatching[[1]]$var.Tr
  mb1[i,4] <- mb.ediff$BeforeMatching[[1]]$var.Co
  mb1[i,5] <- mb.ediff$BeforeMatching[[1]]$p.value
  mb1[i,6] <- mb.ediff$AfterMatching[[1]]$mean.Tr
  mb1[i,7] <- mb.ediff$AfterMatching[[1]]$mean.Co
  mb1[i,8] <- mb.ediff$AfterMatching[[1]]$var.Tr
  mb1[i,9] <- mb.ediff$AfterMatching[[1]]$var.Co
  mb1[i,10]<- mb.ediff$AfterMatching[[1]]$p.value
  
}

xtable(mb1, digits = 3)


##############################################
## Difference-in-Differences Matching Table ##
##############################################

est <- c(match1$est, match2$est, match3$est,
         match7$est, match8$est,
         match4$est, match6$est, match5$est)
se <- c(match1$se, match2$se, match3$se,
        match7$se, match8$se,
        match4$se, match6$se, match5$se)
n <- c(sum(length(match1$index.treated), length(match1$index.control)), 
       sum(length(match2$index.treated), length(match2$index.control)), 
       sum(length(match3$index.treated), length(match3$index.control)),
       sum(length(match7$index.treated), length(match7$index.control)),
       sum(length(match8$index.treated), length(match8$index.control)),
       sum(length(match4$index.treated), length(match4$index.control)),
       sum(length(match6$index.treated), length(match6$index.control)),
       sum(length(match5$index.treated), length(match5$index.control))
       )

table.match <- cbind(est, se, n)
rownames(table.match) <- c("Vote Share", "Vote Share 0.05",
                           "Vote Share 0.10", 
                           "Leadership", "Cmte Chair", "Rules Cmte",
                           "Finance & Tax Cmte", "Appropriations Cmte")


#########################
## Rules Committee t+1 ##
#########################

rules.dat <- dat[dat$ruleslag>= 1,]


##################################
## Re-Do OLS Main Model, And FE ##
##################################

## Main Model, no fixed effects ##
ols.main.rules <- lm(loginc ~ votemarg + leadership +
               majpty + chair + rules + fintax +
               approp + agriculture + education +
               health + judiciary + highpost + lowpost +
               age + tenure + postgrad + legal + bizman +
               female + white + gdp01, data = rules.dat)
summary(ols.main.rules)

se.cluster.main.rules <- as.matrix(clusterMod(ols.main.rules, rules.dat$year)[[1]])[,2]

se.robust.main.rules <- sqrt(diag(vcovHC(ols.main.rules)))


## Main model, fixed effects ##
ols.fe.main.rules <- lm(loginc ~ votemarg + leadership +
                  majpty + chair + rules + fintax +
                  approp + agriculture + education +
                  health + judiciary +
                  as.factor(year) +
                  as.factor(memberid), data=rules.dat)
summary(ols.fe.main.rules)

se.cluster.fe.main.rules <- as.matrix(clusterMod(ols.fe.main.rules, rules.dat$year)[[1]])[,2]

se.robust.fe.main.rules <- sqrt(diag(vcovHC(ols.fe.main.rules)))


################################
## ECM Model in Stata do-file ##
## Stargaze OLS Models here   ##
################################

stargazer(ols.main.rules, ols.fe.main.rules,
          se = list(se.cluster.main.rules,
                    se.cluster.fe.main.rules),
          no.space=T,
          keep.stat = c("n", "adj.rsq", "f"),
          omit = c("memberid", "year"),
          dep.var.labels = "Income (2001 $USD)",
          covariate.labels = c("Vote Share",
                               "Party Leaders",
                               "Majority Party",
                               "Committee Chairs",
                               "Rules Committee",
                               "Finance & Tax Committee",
                               "Appropriations Committee",
                               "Agriculture Committee",
                               "Education Committee",
                               "Health Committee",
                               "Judiciary Committee",
                               "Ran For Higher Office",
                               "Ran For Lower Office",
                               "Age",
                               "Tenure",
                               "Post-Graduate Degree",
                               "Legal Career",
                               "Business Career",
                               "Female",
                               "White",
                               "GDP (2001 $USD)",
                               "Intercept"))



