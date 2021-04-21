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
d$Wh = as.factor(d $Wh)
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

str(d)
centered = cbind(d,myCenter(d["ModalPresent"]))
table(centered$ModalPresent,centered$Wh,centered$paraphrase)
table(centered$workerid,centered$paraphrase)
table(centered$workerid,centered$Wh)
table(centered$workerid,centered$ModalPresent)

########################################################################
# model with random slopes no matrix verb predictor
m.full = lmer(rating ~ cModalPresent*Wh*paraphrase + (1+paraphrase+Wh+cModalPresent|workerid) + (1+paraphrase|tgrep_id), data=centered,REML=FALSE) 
summary(m.full)

saveRDS(m., "EQ-model-full-nomv.rds")
my_model <- readRDS("EQ-model-full-nomv.rds")


# THIS IS THE MODEL WITHOUT EMBEDDED SQ REMOVED
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
my_model_noMV_noSQ <- readRDS("EQ-model-full-nomv.rds")
summary(my_model_noMV_noSQ)

# THIS IS THE MODEL WITH THE EMBEDDED SQ QUESTIONS REMOVED

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: rating ~ cModalPresent * Wh * paraphrase + (1 + paraphrase +  
#     Wh + cModalPresent | workerid) + (1 + paraphrase | tgrep_id)
#    Data: centered
# 
#      AIC      BIC   logLik deviance df.resid 
#  36993.5  37786.1 -18408.7  36817.5    60191 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.3193 -0.6175 -0.1690  0.5439  3.4396 
# 
# Random effects:
#  Groups   Name                   Variance  Std.Dev.  Corr                                           
#  workerid (Intercept)            1.243e-02 0.1114986                                                
#           paraphrasea.vs.every   3.289e-02 0.1813433 -0.80                                          
#           paraphrasethe.vs.every 5.346e-02 0.2312127 -0.84  0.34                                    
#           Whhow.vs.when          6.953e-08 0.0002637 -0.65  0.07  0.96                              
#           Whwhat.vs.when         4.482e-07 0.0006695 -0.62  0.02  0.95  1.00                        
#           Whwhere.vs.when        4.047e-06 0.0020118 -0.90  0.98  0.52  0.26  0.21                  
#           Whwho.vs.when          1.638e-05 0.0040476  0.08  0.53 -0.61 -0.81 -0.83  0.36            
#           Whwhy.vs.when          2.220e-06 0.0014898  0.15  0.47 -0.67 -0.85 -0.87  0.29  1.00      
#           cModalPresent          2.474e-06 0.0015727 -0.63  0.04  0.95  1.00  1.00  0.23 -0.82 -0.86
#  tgrep_id (Intercept)            2.593e-02 0.1610212                                                
#           paraphrasea.vs.every   3.652e-02 0.1911091 -0.88                                          
#           paraphrasethe.vs.every 1.095e-01 0.3308864 -0.96  0.71                                    
#  Residual                        9.667e-02 0.3109212                                                
# Number of obs: 60279, groups:  workerid, 952; tgrep_id, 797
# 
# Fixed effects:
#                                                        Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                            0.179976   0.040813 815.338434   4.410 1.17e-05 ***
# cModalPresent                                         -0.041913   0.098079 810.311212  -0.427 0.669248    
# Whhow.vs.when                                          0.104872   0.041629 802.422880   2.519 0.011955 *  
# Whwhat.vs.when                                        -0.011089   0.042223 802.672874  -0.263 0.792905    
# Whwhere.vs.when                                        0.006005   0.045843 799.974880   0.131 0.895820    
# Whwho.vs.when                                          0.091257   0.048739 794.144127   1.872 0.061524 .  
# Whwhy.vs.when                                         -0.039297   0.046254 800.192449  -0.850 0.395798    
# paraphrasea.vs.every                                   0.132067   0.050036 819.665719   2.639 0.008462 ** 
# paraphrasethe.vs.every                                 0.253765   0.080858 803.005595   3.138 0.001761 ** 
# cModalPresent:Whhow.vs.when                            0.052039   0.101919 811.756272   0.511 0.609776    
# cModalPresent:Whwhat.vs.when                           0.088260   0.104063 811.532710   0.848 0.396613    
# cModalPresent:Whwhere.vs.when                         -0.016012   0.113586 808.265981  -0.141 0.887929    
# cModalPresent:Whwho.vs.when                            0.174368   0.158543 792.476488   1.100 0.271749    
# cModalPresent:Whwhy.vs.when                            0.066939   0.112609 802.862210   0.594 0.552386    
# cModalPresent:paraphrasea.vs.every                     0.198709   0.120058 806.459733   1.655 0.098293 .  
# cModalPresent:paraphrasethe.vs.every                  -0.064529   0.194276 796.780811  -0.332 0.739864    
# Whhow.vs.when:paraphrasea.vs.every                    -0.239076   0.050873 795.376116  -4.699 3.07e-06 ***
# Whwhat.vs.when:paraphrasea.vs.every                   -0.076253   0.051601 795.675845  -1.478 0.139873    
# Whwhere.vs.when:paraphrasea.vs.every                  -0.103947   0.056002 792.182971  -1.856 0.063806 .  
# Whwho.vs.when:paraphrasea.vs.every                    -0.199583   0.059496 784.271319  -3.355 0.000833 ***
# Whwhy.vs.when:paraphrasea.vs.every                    -0.074937   0.056516 792.152669  -1.326 0.185246    
# Whhow.vs.when:paraphrasethe.vs.every                  -0.060759   0.082450 788.196876  -0.737 0.461395    
# Whwhat.vs.when:paraphrasethe.vs.every                  0.126746   0.083629 788.532805   1.516 0.130027    
# Whwhere.vs.when:paraphrasethe.vs.every                 0.102840   0.090806 786.070486   1.133 0.257759    
# Whwho.vs.when:paraphrasethe.vs.every                  -0.049553   0.096620 782.828079  -0.513 0.608187    
# Whwhy.vs.when:paraphrasethe.vs.every                   0.239143   0.091673 788.009529   2.609 0.009262 ** 
# cModalPresent:Whhow.vs.when:paraphrasea.vs.every      -0.092150   0.124801 808.263646  -0.738 0.460502    
# cModalPresent:Whwhat.vs.when:paraphrasea.vs.every     -0.144709   0.127414 807.870441  -1.136 0.256404    
# cModalPresent:Whwhere.vs.when:paraphrasea.vs.every    -0.049181   0.139001 803.335835  -0.354 0.723569    
# cModalPresent:Whwho.vs.when:paraphrasea.vs.every      -0.325885   0.193580 781.637317  -1.683 0.092684 .  
# cModalPresent:Whwhy.vs.when:paraphrasea.vs.every      -0.099021   0.137693 795.671167  -0.719 0.472263    
# cModalPresent:Whhow.vs.when:paraphrasethe.vs.every    -0.071946   0.201931 798.811871  -0.356 0.721718    
# cModalPresent:Whwhat.vs.when:paraphrasethe.vs.every   -0.144948   0.206160 798.274137  -0.703 0.482207    
# cModalPresent:Whwhere.vs.when:paraphrasethe.vs.every   0.059791   0.225063 795.315344   0.266 0.790568    
# cModalPresent:Whwho.vs.when:paraphrasethe.vs.every    -0.255438   0.314696 784.959228  -0.812 0.417211    
# cModalPresent:Whwhy.vs.when:paraphrasethe.vs.every    -0.110268   0.223248 791.720289  -0.494 0.621494  


########################################################################

m.full = lmer(rating ~ cModalPresent*Wh*paraphrase + (1+paraphrase+Wh+cModalPresent|workerid) + (1+paraphrase|VerbLemma) + (1+paraphrase|tgrep_id), data=centered,REML=FALSE) 
summary(m.full)
saveRDS(m.full, "EQ-model-full-mvRE.rds")
my_model_mvRE <- readRDS("EQ-model-full-mvRE.rds")
summary(my_model_mvRE)
# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: rating ~ cModalPresent * Wh * paraphrase + (1 + paraphrase +  
#     Wh + cModalPresent | workerid) + (1 + paraphrase | VerbLemma) +      (1 + paraphrase | tgrep_id)
#    Data: centered
# 
#      AIC      BIC   logLik deviance df.resid 
#  36980.5  37827.2 -18396.3  36792.5    60185 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.3082 -0.6174 -0.1690  0.5445  3.4443 
# 
# Random effects:
#  Groups    Name                   Variance  Std.Dev.  Corr                                           
#  workerid  (Intercept)            1.248e-02 0.1116974                                                
#            paraphrasea.vs.every   3.292e-02 0.1814313 -0.80                                          
#            paraphrasethe.vs.every 5.339e-02 0.2310708 -0.84  0.34                                    
#            Whhow.vs.when          1.980e-07 0.0004449 -0.83  0.33  0.99                              
#            Whwhat.vs.when         7.177e-07 0.0008472 -0.74  0.19  0.99  0.99                        
#            Whwhere.vs.when        4.821e-06 0.0021958 -0.93  0.97  0.57  0.56  0.43                  
#            Whwho.vs.when          1.605e-05 0.0040061  0.03  0.57 -0.57 -0.58 -0.70  0.34            
#            Whwhy.vs.when          1.967e-06 0.0014024  0.00  0.60 -0.54 -0.55 -0.67  0.38  1.00      
#            cModalPresent          2.693e-06 0.0016411 -0.63  0.04  0.95  0.95  0.99  0.30 -0.79 -0.77
#  tgrep_id  (Intercept)            2.429e-02 0.1558445                                                
#            paraphrasea.vs.every   3.504e-02 0.1871773 -0.88                                          
#            paraphrasethe.vs.every 1.008e-01 0.3174589 -0.96  0.72                                    
#  VerbLemma (Intercept)            3.201e-03 0.0565740                                                
#            paraphrasea.vs.every   2.663e-03 0.0516029 -0.84                                          
#            paraphrasethe.vs.every 1.774e-02 0.1331752 -0.98  0.71                                    
#  Residual                         9.667e-02 0.3109218                                                
# Number of obs: 60279, groups:  workerid, 952; tgrep_id, 797; VerbLemma, 86
# 
# Fixed effects:
#                                                        Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                            0.197573   0.041330 744.784166   4.780 2.11e-06 ***
# cModalPresent                                         -0.036717   0.096714 808.000586  -0.380 0.704309    
# Whhow.vs.when                                          0.099899   0.041148 803.657231   2.428 0.015408 *  
# Whwhat.vs.when                                        -0.016053   0.041872 805.578929  -0.383 0.701543    
# Whwhere.vs.when                                       -0.001575   0.045547 802.159718  -0.035 0.972420    
# Whwho.vs.when                                          0.087438   0.048172 796.838332   1.815 0.069883 .  
# Whwhy.vs.when                                         -0.054707   0.046479 789.225570  -1.177 0.239541    
# paraphrasea.vs.every                                   0.121972   0.050632 739.405821   2.409 0.016241 *  
# paraphrasethe.vs.every                                 0.209751   0.082086 731.529575   2.555 0.010812 *  
# cModalPresent:Whhow.vs.when                            0.041040   0.100437 807.980644   0.409 0.682930    
# cModalPresent:Whwhat.vs.when                           0.073492   0.102606 808.692888   0.716 0.474045    
# cModalPresent:Whwhere.vs.when                         -0.034503   0.111978 805.122126  -0.308 0.758070    
# cModalPresent:Whwho.vs.when                            0.160365   0.157394 786.425667   1.019 0.308575    
# cModalPresent:Whwhy.vs.when                            0.051846   0.111156 799.815297   0.466 0.641039    
# cModalPresent:paraphrasea.vs.every                     0.192058   0.119073 794.330947   1.613 0.107153    
# cModalPresent:paraphrasethe.vs.every                  -0.073804   0.190589 795.685857  -0.387 0.698683    
# Whhow.vs.when:paraphrasea.vs.every                    -0.235743   0.050579 793.633223  -4.661 3.69e-06 ***
# Whwhat.vs.when:paraphrasea.vs.every                   -0.074101   0.051433 796.136362  -1.441 0.150058    
# Whwhere.vs.when:paraphrasea.vs.every                  -0.103687   0.055896 792.090209  -1.855 0.063967 .  
# Whwho.vs.when:paraphrasea.vs.every                    -0.202337   0.059119 783.460546  -3.423 0.000653 ***
# Whwhy.vs.when:paraphrasea.vs.every                    -0.059138   0.057024 758.482884  -1.037 0.300038    
# Whhow.vs.when:paraphrasethe.vs.every                  -0.048848   0.081119 788.420673  -0.602 0.547230    
# Whwhat.vs.when:paraphrasethe.vs.every                  0.139865   0.082625 790.398311   1.693 0.090894 .  
# Whwhere.vs.when:paraphrasethe.vs.every                 0.125981   0.089931 787.125307   1.401 0.161650    
# Whwho.vs.when:paraphrasethe.vs.every                  -0.034885   0.095088 784.549608  -0.367 0.713815    
# Whwhy.vs.when:paraphrasethe.vs.every                   0.270452   0.091980 787.073690   2.940 0.003375 ** 
# cModalPresent:Whhow.vs.when:paraphrasea.vs.every      -0.083466   0.123732 793.885473  -0.675 0.500146    
# cModalPresent:Whwhat.vs.when:paraphrasea.vs.every     -0.131488   0.126388 796.231899  -1.040 0.298494    
# cModalPresent:Whwhere.vs.when:paraphrasea.vs.every    -0.039004   0.137877 791.576809  -0.283 0.777335    
# cModalPresent:Whwho.vs.when:paraphrasea.vs.every      -0.309433   0.192645 780.012784  -1.606 0.108627    
# cModalPresent:Whwhy.vs.when:paraphrasea.vs.every      -0.084277   0.136773 786.430509  -0.616 0.537951    
# cModalPresent:Whhow.vs.when:paraphrasethe.vs.every    -0.046874   0.197928 796.775640  -0.237 0.812856    
# cModalPresent:Whwhat.vs.when:paraphrasethe.vs.every   -0.112843   0.202206 796.560455  -0.558 0.576960    
# cModalPresent:Whwhere.vs.when:paraphrasethe.vs.every   0.106763   0.220701 793.276125   0.484 0.628702    
# cModalPresent:Whwho.vs.when:paraphrasethe.vs.every    -0.228955   0.311814 769.579988  -0.734 0.463009    
# cModalPresent:Whwhy.vs.when:paraphrasethe.vs.every    -0.078666   0.219214 788.942019  -0.359 0.719801    
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
