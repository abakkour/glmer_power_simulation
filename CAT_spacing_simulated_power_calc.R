rm(list=ls())
args=commandArgs(T)
# Enter number of subjects to be simulated
numsub=args[1]

# load data from first sample to be replicated
load("boost_space3_probe.RData")
library(lme4)
library(dplyr)

boost_space3_probe$PairType=as.factor(boost_space3_probe$PairType)

# run mixed-effects logistic on data from first sample
mod=glmer(Outcome ~ PairType + session + (PairType|subjid) + (session|subjid) + (1|subjid), data=boost_space3_probe,family='binomial')

# extract fixed effects
beta=fixef(mod)
theta=getME(mod,"theta")

# Number of simulated datasets
nsim=100

power=c()
# prepare labels for 4 pairtypes and 3 sessions, each with 18 observations (4*3*18=216) per subject
expdat <- expand.grid(subjid = factor(1:numsub), PairType=factor(1:4),session=factor(1:3),obs=factor(1:18))
expdat$session=as.numeric(expdat$session)
set.seed(numsub)
#simulate 1/0 responses
ss <- simulate(~ PairType + session + (PairType|subjid) + (session|subjid) + (1|subjid), nsim = nsim, family = binomial, newdata = expdat, newparams = list(theta = theta,beta = beta))

# append first set of simulated responses to dataframe
expdat$resp <- ss[, 1]

# Fit same model to first iteration of simulated responses
fit <- glmer(resp ~ PairType + session + (PairType|subjid) + (session|subjid) + (1|subjid), family = binomial, data = expdat) #weights = rep(25,nrow(expdat))

# function to output coefficients from refitting the model to each iteration of simulated responses
fitsim <- function(i) {
  coef(summary(refit(fit, ss[[i]])))
}

# run all iterations
t <- system.time(fitAll <- sapply(seq(nsim), function(i) fitsim(i)))

print(t) 
# make a dataframe out of the outputs from the refits to all iterations of simulated data
fitAll <- as.data.frame(t(fitAll))
# Name the header for intuitive referencing
names(fitAll)=paste(c(rep("est_",5),rep("stderr_",5) ,rep("zval_",5),rep("pval_",5)),rep(c("int","pt2","pt3","pt4","sess"),4),sep="")

# save the dataframe that includes all coefficients from all iterations for the inputed number of simulated subjects
save(fitAll,file=paste("fitAll_numsub_",numsub,".RData",sep=""))