# ---
# title: "Analysis of questions"
# author: "mcmoyer"
# date: "January 22, 2021"
# ---

## Step 1: select stimuli for experiment
library(tidyverse)
library(lme4)
library(lmerTest)
library(multcomp) # not available for this version of R
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


# full model with random slopes
m.full = lmer(rating ~ cModalPresent*Wh*paraphrase + (1+paraphrase+Wh+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=centered,REML=FALSE) 
summary(m.full)
# Model takes FOREVER to run (like, 30 mins)
table(centered$ModalPresent,centered$Wh,centered$paraphrase)
table(centered$workerid,centered$paraphrase)
table(centered$workerid,centered$Wh)
table(centered$workerid,centered$ModalPresent)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
# lmerModLmerTest]
# Formula: rating ~ cModalPresent * Wh * paraphrase + (1 + paraphrase +  
#     Wh + cModalPresent | workerid) + (1 + paraphrase | tgrep_id)
#    Data: centered
# 
#      AIC      BIC   logLik deviance df.resid 
#  21876.0  22639.7 -10850.0  21700.0    43304 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.7228 -0.6044 -0.1279  0.4950  4.2025 
# 
# Random effects:
#  Groups   Name                   Variance  Std.Dev. Corr                                     
#  tgrep_id (Intercept)            2.684e-02 0.163840                                          
#           paraphrasea.vs.every   2.540e-02 0.159377 -0.78                                    
#           paraphrasethe.vs.every 1.474e-01 0.383954 -0.96  0.58                              
#  workerid (Intercept)            0.000e+00 0.000000                                          
#           paraphrasea.vs.every   3.837e-02 0.195879   NaN                                    
#           paraphrasethe.vs.every 4.604e-02 0.214577   NaN  0.39                              
#           Whhow.vs.when          1.292e-02 0.113662   NaN -0.83 -0.84                        
#           Whwhat.vs.when         1.267e-02 0.112545   NaN -0.83 -0.84  1.00                  
#           Whwhere.vs.when        1.279e-02 0.113101   NaN -0.83 -0.84  1.00  1.00            
#           Whwho.vs.when          1.342e-02 0.115841   NaN -0.82 -0.84  1.00  1.00  1.00      
#           Whwhy.vs.when          1.241e-02 0.111421   NaN -0.82 -0.84  1.00  1.00  1.00  1.00
#           cModalPresent          8.694e-06 0.002949   NaN  0.96  0.64 -0.95 -0.96 -0.96 -0.95
#  Residual                        8.464e-02 0.290937                                          
#       
#       
#  -0.95
#       
# Number of obs: 43392, groups:  tgrep_id, 842; workerid, 621
# 
# Fixed effects:
#                                                       Estimate Std. Error        df t value
# (Intercept)                                            0.04889    0.04708 848.44943   1.039
# cModalPresent                                          0.01203    0.18551 856.29852   0.065
# Whhow.vs.when                                          0.20904    0.04795 863.89472   4.359
# Whwhat.vs.when                                         0.11314    0.04954 862.68055   2.284
# Whwhere.vs.when                                        0.07188    0.05158 858.80868   1.394
# Whwho.vs.when                                          0.12775    0.05885 865.87052   2.171
# Whwhy.vs.when                                          0.12889    0.05398 858.19211   2.388
# paraphrasea.vs.every                                   0.16204    0.05069 889.62878   3.197
# paraphrasethe.vs.every                                 0.65419    0.10526 849.16360   6.215
# cModalPresent:Whhow.vs.when                            0.03694    0.18718 856.05778   0.197
# cModalPresent:Whwhat.vs.when                          -0.01158    0.19091 855.92150  -0.061
# cModalPresent:Whwhere.vs.when                         -0.04695    0.21324 854.52074  -0.220
# cModalPresent:Whwho.vs.when                           -0.01785    0.20313 856.85475  -0.088
# cModalPresent:Whwhy.vs.when                           -0.03496    0.19543 857.12459  -0.179
# cModalPresent:paraphrasea.vs.every                     0.52643    0.19851 869.57773   2.652
# cModalPresent:paraphrasethe.vs.every                  -0.62531    0.41374 845.86477  -1.511
# Whhow.vs.when:paraphrasea.vs.every                    -0.20200    0.05081 849.88801  -3.975
# Whwhat.vs.when:paraphrasea.vs.every                   -0.10755    0.05241 846.77656  -2.052
# Whwhere.vs.when:paraphrasea.vs.every                  -0.16454    0.05452 841.17102  -3.018
# Whwho.vs.when:paraphrasea.vs.every                    -0.15648    0.06229 853.72510  -2.512
# Whwhy.vs.when:paraphrasea.vs.every                    -0.11503    0.05707 841.90980  -2.016
# Whhow.vs.when:paraphrasethe.vs.every                  -0.42768    0.10641 838.61286  -4.019
# Whwhat.vs.when:paraphrasethe.vs.every                 -0.24206    0.10986 836.10372  -2.203
# Whwhere.vs.when:paraphrasethe.vs.every                -0.03489    0.11444 834.39752  -0.305
# Whwho.vs.when:paraphrasethe.vs.every                  -0.23035    0.13036 837.82317  -1.767
# Whwhy.vs.when:paraphrasethe.vs.every                  -0.27087    0.11978 834.60806  -2.261
# cModalPresent:Whhow.vs.when:paraphrasea.vs.every      -0.44499    0.20031 869.14041  -2.222
# cModalPresent:Whwhat.vs.when:paraphrasea.vs.every     -0.36585    0.20387 865.56751  -1.794
# cModalPresent:Whwhere.vs.when:paraphrasea.vs.every    -0.50700    0.22779 862.04355  -2.226
# cModalPresent:Whwho.vs.when:paraphrasea.vs.every      -0.31776    0.21722 867.82899  -1.463
# cModalPresent:Whwhy.vs.when:paraphrasea.vs.every      -0.46610    0.20926 870.39991  -2.227
# cModalPresent:Whhow.vs.when:paraphrasethe.vs.every     0.40452    0.41750 845.98119   0.969
# cModalPresent:Whwhat.vs.when:paraphrasethe.vs.every    0.45761    0.42540 843.54757   1.076
# cModalPresent:Whwhere.vs.when:paraphrasethe.vs.every   0.68928    0.47551 844.73826   1.450
# cModalPresent:Whwho.vs.when:paraphrasethe.vs.every     0.44681    0.45286 845.91455   0.987
# cModalPresent:Whwhy.vs.when:paraphrasethe.vs.every     0.62798    0.43595 847.48941   1.440
#                                                      Pr(>|t|)    
# (Intercept)                                           0.29928    
# cModalPresent                                         0.94830    
# Whhow.vs.when                                        1.46e-05 ***
# Whwhat.vs.when                                        0.02264 *  
# Whwhere.vs.when                                       0.16382    
# Whwho.vs.when                                         0.03021 *  
# Whwhy.vs.when                                         0.01717 *  
# paraphrasea.vs.every                                  0.00144 ** 
# paraphrasethe.vs.every                               8.03e-10 ***
# cModalPresent:Whhow.vs.when                           0.84360    
# cModalPresent:Whwhat.vs.when                          0.95166    
# cModalPresent:Whwhere.vs.when                         0.82580    
# cModalPresent:Whwho.vs.when                           0.93001    
# cModalPresent:Whwhy.vs.when                           0.85807    
# cModalPresent:paraphrasea.vs.every                    0.00815 ** 
# cModalPresent:paraphrasethe.vs.every                  0.13107    
# Whhow.vs.when:paraphrasea.vs.every                   7.62e-05 ***
# Whwhat.vs.when:paraphrasea.vs.every                   0.04046 *  
# Whwhere.vs.when:paraphrasea.vs.every                  0.00262 ** 
# Whwho.vs.when:paraphrasea.vs.every                    0.01218 *  
# Whwhy.vs.when:paraphrasea.vs.every                    0.04417 *  
# Whhow.vs.when:paraphrasethe.vs.every                 6.37e-05 ***
# Whwhat.vs.when:paraphrasethe.vs.every                 0.02784 *  
# Whwhere.vs.when:paraphrasethe.vs.every                0.76055    
# Whwho.vs.when:paraphrasethe.vs.every                  0.07759 .  
# Whwhy.vs.when:paraphrasethe.vs.every                  0.02399 *  
# cModalPresent:Whhow.vs.when:paraphrasea.vs.every      0.02657 *  
# cModalPresent:Whwhat.vs.when:paraphrasea.vs.every     0.07309 .  
# cModalPresent:Whwhere.vs.when:paraphrasea.vs.every    0.02629 *  
# cModalPresent:Whwho.vs.when:paraphrasea.vs.every      0.14387    
# cModalPresent:Whwhy.vs.when:paraphrasea.vs.every      0.02618 *  
# cModalPresent:Whhow.vs.when:paraphrasethe.vs.every    0.33287    
# cModalPresent:Whwhat.vs.when:paraphrasethe.vs.every   0.28236    
# cModalPresent:Whwhere.vs.when:paraphrasethe.vs.every  0.14755    
# cModalPresent:Whwho.vs.when:paraphrasethe.vs.every    0.32410    
# cModalPresent:Whwhy.vs.when:paraphrasethe.vs.every    0.15010    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


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
