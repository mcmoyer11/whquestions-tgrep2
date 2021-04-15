# ---
# title: "Analysis of EMBEDDED QUESTIONS"
# author: "mcmoyer"
# date: "March 30, 2021"
# ---

## Step 1: select stimuli for experiment
library(lme4)
library(lmerTest)
library(multcomp) # not available for this version of R
library(tidyverse)
theme_set(theme_bw())

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source("../../helpers.R")

cbPalette <- c("#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73")

########################################################################
# Read the data into R.
d = read.csv("../data/normed.csv")

d$ModalPresent = as.factor(d$ModalPresent)
d$Wh = as.factor(d$Wh)
d$paraphrase = as.factor(d$paraphrase)

# Need to first figure out which verbs to look at
d$VerbLemma = as.factor(d$VerbLemma)

# Set the reference levels
contrasts(d$ModalPresent) # once this is centered, the reference level won't matter anymore
contrasts(d$paraphrase)
contrasts(d$Wh)

contrasts(d$Wh) = cbind("how.vs.when"=c(0,1,0,0,0,0),"what.vs.when"=c(1,0,0,0,0,0),
                "where.vs.when"=c(0,0,0,1,0,0),"who.vs.when"=c(0,0,0,0,1,0),
                "why.vs.when"=c(0,0,0,0,0,1))

contrasts(d$paraphrase) = cbind("a.vs.every"=c(1,0,0),"the.vs.every"=c(0,0,1))

########################################################################
# mean center modalpresent (2-level variables only)


# d$ModalPresent[d$ModalPresent == "no"] = "0"
# d$ModalPresent[d$ModalPresent == "yes"] = "1"

str(d)
centered = cbind(d,myCenter(d["ModalPresent"]))

head(centered)
summary(centered)
str(centered)


# model with random slopes no matrix verb predictor
m. = lmer(rating ~ cModalPresent*Wh*VerbLemma*paraphrase + (1+paraphrase+Wh+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=centered,REML=FALSE) 
summary(m.full)

# Error message(s) when including VerbLemma as random slope:

# Error: number of observations (=64911) <= number of random effects (=90440) 
# for term (1 + paraphrase + Wh + cModalPresent + VerbLemma | workerid); the 
# random-effects parameters and the residual variance (or scale parameter) 
# are probably unidentifiable


# fixed-effect model matrix is rank deficient so dropping 2508 columns / coefficients



saveRDS(m.full, "EQ-model-full.rds")
my_model <- readRDS("EQ-model-full.rds")

table(centered$ModalPresent,centered$Wh,centered$paraphrase)
table(centered$workerid,centered$paraphrase)
table(centered$workerid,centered$Wh)
table(centered$workerid,centered$ModalPresent)


# model with random slopes no matrix verb predictor
m.mid = lmer(rating ~ cModalPresent*Wh*paraphrase + (1+paraphrase+Wh+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=centered,REML=FALSE) 
summary(m.mid)
# Model takes FOREVER to run (like, 30 mins)
saveRDS(m.mid, "EQ-model-noMV.rds")
my_model <- readRDS("EQ-model-noMV.rds")

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: rating ~ cModalPresent * Wh * paraphrase + (1 + paraphrase +  
#     Wh + cModalPresent | workerid) + (1 + paraphrase | tgrep_id)
#    Data: centered
# 
#      AIC      BIC   logLik deviance df.resid 
#  40698.4  41499.5 -20261.2  40522.4    66278 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.4108 -0.6191 -0.1715  0.5454  3.4331 
# 
# Random effects:
#  Groups   Name                   Variance  Std.Dev.  Corr                                           
#  workerid (Intercept)            1.226e-02 0.1107306                                                
#           paraphrasea.vs.every   3.233e-02 0.1797972 -0.81                                          
#           paraphrasethe.vs.every 5.262e-02 0.2293982 -0.83  0.35                                    
#           Whhow.vs.when          5.513e-07 0.0007425 -0.02  0.60 -0.53                              
#           Whwhat.vs.when         4.282e-07 0.0006544  0.04  0.55 -0.58  1.00                        
#           Whwhere.vs.when        3.067e-06 0.0017514 -0.27  0.78 -0.30  0.96  0.95                  
#           Whwho.vs.when          2.077e-05 0.0045571  0.14  0.47 -0.66  0.98  0.99  0.92            
#           Whwhy.vs.when          7.417e-06 0.0027233  0.04  0.55 -0.58  1.00  1.00  0.95  0.99      
#           cModalPresent          3.641e-06 0.0019082 -0.18 -0.43  0.69 -0.98 -0.99 -0.90 -1.00 -0.99
#  tgrep_id (Intercept)            2.558e-02 0.1599292                                                
#           paraphrasea.vs.every   3.665e-02 0.1914377 -0.88                                          
#           paraphrasethe.vs.every 1.080e-01 0.3286608 -0.96  0.71                                    
#  Residual                        9.699e-02 0.3114372                                                
# Number of obs: 66366, groups:  workerid, 976; tgrep_id, 853
# 
# Fixed effects:
#                                                        Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                           1.705e-01  4.043e-02  8.698e+02   4.217 2.74e-05 ***
# cModalPresent                                        -3.392e-02  9.740e-02  8.707e+02  -0.348 0.727711    
# Whhow.vs.when                                         1.113e-01  4.119e-02  8.558e+02   2.701 0.007055 ** 
# Whwhat.vs.when                                       -7.691e-04  4.178e-02  8.556e+02  -0.018 0.985317    
# Whwhere.vs.when                                       3.285e-02  4.494e-02  8.530e+02   0.731 0.464898    
# Whwho.vs.when                                         9.545e-02  4.816e-02  8.486e+02   1.982 0.047793 *  
# Whwhy.vs.when                                        -2.400e-02  4.495e-02  8.536e+02  -0.534 0.593544    
# paraphrasea.vs.every                                  1.333e-01  4.987e-02  8.716e+02   2.674 0.007647 ** 
# paraphrasethe.vs.every                                2.850e-01  8.018e-02  8.582e+02   3.555 0.000399 ***
# cModalPresent:Whhow.vs.when                           6.838e-02  1.009e-01  8.716e+02   0.678 0.498166    
# cModalPresent:Whwhat.vs.when                          7.718e-02  1.033e-01  8.717e+02   0.747 0.455184    
# cModalPresent:Whwhere.vs.when                        -4.297e-02  1.125e-01  8.665e+02  -0.382 0.702538    
# cModalPresent:Whwho.vs.when                           1.688e-01  1.574e-01  8.509e+02   1.072 0.283848    
# cModalPresent:Whwhy.vs.when                           5.399e-02  1.092e-01  8.639e+02   0.494 0.621175    
# cModalPresent:paraphrasea.vs.every                    1.950e-01  1.200e-01  8.667e+02   1.625 0.104581    
# cModalPresent:paraphrasethe.vs.every                 -8.836e-02  1.929e-01  8.552e+02  -0.458 0.647107    
# Whhow.vs.when:paraphrasea.vs.every                   -2.331e-01  5.066e-02  8.474e+02  -4.601 4.86e-06 ***
# Whwhat.vs.when:paraphrasea.vs.every                  -7.891e-02  5.138e-02  8.471e+02  -1.536 0.124953    
# Whwhere.vs.when:paraphrasea.vs.every                 -1.287e-01  5.524e-02  8.439e+02  -2.330 0.020038 *  
# Whwho.vs.when:paraphrasea.vs.every                   -1.943e-01  5.917e-02  8.379e+02  -3.283 0.001068 ** 
# Whwhy.vs.when:paraphrasea.vs.every                   -8.012e-02  5.528e-02  8.443e+02  -1.449 0.147590    
# Whhow.vs.when:paraphrasethe.vs.every                 -9.080e-02  8.167e-02  8.434e+02  -1.112 0.266505    
# Whwhat.vs.when:paraphrasethe.vs.every                 9.410e-02  8.283e-02  8.431e+02   1.136 0.256249    
# Whwhere.vs.when:paraphrasethe.vs.every                4.269e-02  8.909e-02  8.407e+02   0.479 0.631901    
# Whwho.vs.when:paraphrasethe.vs.every                 -7.351e-02  9.554e-02  8.385e+02  -0.769 0.441869    
# Whwhy.vs.when:paraphrasethe.vs.every                  1.928e-01  8.916e-02  8.427e+02   2.162 0.030867 *  
# cModalPresent:Whhow.vs.when:paraphrasea.vs.every     -1.073e-01  1.244e-01  8.677e+02  -0.863 0.388512    
# cModalPresent:Whwhat.vs.when:paraphrasea.vs.every    -1.392e-01  1.273e-01  8.678e+02  -1.093 0.274656    
# cModalPresent:Whwhere.vs.when:paraphrasea.vs.every   -1.961e-02  1.386e-01  8.612e+02  -0.142 0.887496    
# cModalPresent:Whwho.vs.when:paraphrasea.vs.every     -3.343e-01  1.935e-01  8.405e+02  -1.728 0.084360 .  
# cModalPresent:Whwhy.vs.when:paraphrasea.vs.every     -1.063e-01  1.345e-01  8.574e+02  -0.791 0.429382    
# cModalPresent:Whhow.vs.when:paraphrasethe.vs.every   -1.008e-01  1.999e-01  8.568e+02  -0.504 0.614250    
# cModalPresent:Whwhat.vs.when:paraphrasethe.vs.every  -1.155e-01  2.047e-01  8.566e+02  -0.564 0.572640    
# cModalPresent:Whwhere.vs.when:paraphrasethe.vs.every  1.177e-01  2.229e-01  8.524e+02   0.528 0.597481    
# cModalPresent:Whwho.vs.when:paraphrasethe.vs.every   -2.216e-01  3.124e-01  8.426e+02  -0.709 0.478317    
# cModalPresent:Whwhy.vs.when:paraphrasethe.vs.every   -6.111e-02  2.165e-01  8.511e+02  -0.282 0.777798    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation matrix not shown by default, as p = 36 > 12.
# Use print(x, correlation=TRUE)  or
#     vcov(x)        if you need it
# 
# convergence code: 0
# boundary (singular) fit: see ?isSingular

########################################################################
# breaking up by wh-word so that 3-way interaction is more interpretable
########################################################################
# some of them cannot have the full random effects, so random effects
# have to be what is justified by the data
########################################################################


########################################################################
# "what" model
d_what = d %>% 
  filter(Wh=="what") %>% 
  mutate(cModalPresent = as.numeric(ModalPresent)-mean(as.numeric(ModalPresent))) %>% 
  droplevels()
contrasts(d_what$paraphrase) = cbind("a.vs.every"=c(1,0,0),"the.vs.every"=c(0,0,1))

m.what = lmer(rating ~ cModalPresent*paraphrase + (1+paraphrase+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=d_what) 
summary(m.what)

m.what.simple = lmer(rating ~ paraphrase*cModalPresent-cModalPresent + (1+paraphrase+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=d_what) 
summary(m.what.simple)

table(d_what$workerid,d_what$ModalPresent)

########################################################################
# "how" model
d_how = d %>% 
  filter(Wh=="how") %>% 
  mutate(cModalPresent = as.numeric(ModalPresent)-mean(as.numeric(ModalPresent))) %>% 
  droplevels()
contrasts(d_how$paraphrase) = cbind("a.vs.every"=c(1,0,0),"the.vs.every"=c(0,0,1))

m.how = lmer(rating ~ cModalPresent*paraphrase + (1+paraphrase+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=d_how) 
summary(m.how)

m.how.simple = lmer(rating ~ paraphrase*cModalPresent-cModalPresent + (1+paraphrase|tgrep_id), data=d_how) 
summary(m.how.simple)

########################################################################
# "where" model
d_where = d %>% 
  filter(Wh=="where") %>% 
  mutate(cModalPresent = as.numeric(ModalPresent)-mean(as.numeric(ModalPresent))) %>% 
  droplevels()
contrasts(d_where$paraphrase) = cbind("a.vs.every"=c(1,0,0),"the.vs.every"=c(0,0,1))

m.where = lmer(rating ~ cModalPresent*paraphrase + (1+paraphrase+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=d_where) 
summary(m.where)

m.where.simple = lmer(rating ~ paraphrase*cModalPresent-cModalPresent + (1+paraphrase+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=d_where) 
summary(m.where.simple)

########################################################################
# "why" model
d_why = d %>% 
  filter(Wh=="why") %>% 
  mutate(cModalPresent = as.numeric(ModalPresent)-mean(as.numeric(ModalPresent))) %>% 
  droplevels()
contrasts(d_why$paraphrase) = cbind("a.vs.every"=c(1,0,0),"the.vs.every"=c(0,0,1))

m.why = lmer(rating ~ cModalPresent*paraphrase + (1+paraphrase+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=d_why) 
summary(m.why)

m.why.simple = lmer(rating ~ paraphrase*cModalPresent-cModalPresent + (1+paraphrase+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=d_why) 
summary(m.why.simple)

########################################################################
# "who" model
d_who = d %>% 
  filter(Wh=="who") %>% 
  mutate(cModalPresent = as.numeric(ModalPresent)-mean(as.numeric(ModalPresent))) %>% 
  droplevels()
contrasts(d_who$paraphrase) = cbind("a.vs.every"=c(1,0,0),"the.vs.every"=c(0,0,1))

m.who = lmer(rating ~ cModalPresent*paraphrase + (1+paraphrase|workerid) + (1+paraphrase|tgrep_id), data=d_who) 
summary(m.who)

m.who.simple = lmer(rating ~ paraphrase*cModalPresent-cModalPresent + (1+paraphrase|workerid) + (1+paraphrase|tgrep_id), data=d_who) 
summary(m.who.simple)

########################################################################
# "when" model
# have to use less random effects structure
d_when = d %>% 
  filter(Wh=="when") %>% 
  mutate(cModalPresent = as.numeric(ModalPresent)-mean(as.numeric(ModalPresent))) %>% 
  droplevels()
contrasts(d_when$paraphrase) = cbind("a.vs.every"=c(1,0,0),"the.vs.every"=c(0,0,1))

m.when = lmer(rating ~ cModalPresent*paraphrase + (1+paraphrase|tgrep_id), data=d_when) 
summary(m.when)

# simple effects
m.when.simple = lmer(rating ~ paraphrase*cModalPresent-cModalPresent + (1+paraphrase|tgrep_id), data=d_when) 
summary(m.when.simple)



table(d_when$workerid)
table(d_when$workerid,d_when$paraphrase)
table(d_when$tgrep_id,d_when$paraphrase)
table(d_when$workerid,d_when$ModalPresent)
