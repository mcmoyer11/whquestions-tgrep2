# ---
# title: "Analysis of questions"
# author: "mcmoyer"
# date: "November 20, 2020"
# ---

## Step 1: select stimuli for experiment
setwd("/Users/momo/Dropbox/Stanford/whquestions-tgrep2/analysis/rscripts/")
source("/Users/momo/Dropbox/Stanford/whquestions-tgrep2/analysis/helpers.R")
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(multcomp) # not available for this version of R
theme_set(theme_bw())
cbPalette <- c("#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73")

########################################################################
# Read the data into R.
d = read.csv("/Users/momo/Dropbox/Stanford/whquestions-tgrep2/analysis/data/normed.csv")

# Full Model with random slopes
m = lmerTest::lmer(rating ~ ModalPresent*Wh*paraphrase + (1+ModalPresent|workerid) + (1+Wh|workerid) + (1+paraphrase|workerid) + (1|tgrep_id), data=d,REML=FALSE) 
# message?: boundary (singular) fit: see ?isSingular
summary(m)

# Fixed Effects, all still significant
# paraphrase
m1 = lmerTest::lmer(rating ~ ModalPresent*Wh + (1+ModalPresent|workerid) + (1+Wh|workerid) + (1+paraphrase|workerid) + (1|tgrep_id), data=d,REML=FALSE) 
anova(m,m1) #***

# ModalPresent
m2 = lmerTest::lmer(rating ~ Wh*paraphrase +  (1+ModalPresent|workerid) + (1+Wh|workerid) + (1+paraphrase|workerid) + (1|tgrep_id), data=d,REML=FALSE) 
anova(m,m2) #***

# Wh
m3 = lmerTest::lmer(rating ~ ModalPresent*paraphrase + (1+ModalPresent|workerid) + (1+Wh|workerid) + (1+paraphrase|workerid) + (1|tgrep_id), data=normed,REML=FALSE) 
anova(m,m3) #***



# Full Model without random slopes
m = lmerTest::lmer(rating ~ ModalPresent*Wh*paraphrase + (1+ModalPresent|workerid) + (1+Wh|workerid) + (1+paraphrase|workerid) + (1|tgrep_id), data=d,REML=FALSE) 
# message?: boundary (singular) fit: see ?isSingular
summary(m)

# Fixed Effects, all still significant
# paraphrase
m1 = lmerTest::lmer(rating ~ ModalPresent*Wh +  (1|workerid) + (1|tgrep_id), data=d,REML=FALSE) 
anova(m,m1) #***

# ModalPresent
m2 = lmerTest::lmer(rating ~ Wh*paraphrase +  (1|workerid) + (1|tgrep_id), data=d,REML=FALSE) 
anova(m,m2) #***

# Wh
m3 = lmerTest::lmer(rating ~ ModalPresent*paraphrase + (1|workerid) + (1|tgrep_id), data=normed,REML=FALSE) 
anova(m,m3) #***




# Pairwise comparisons
K1 <- glht(m,mcp(Wh="Tukey"))$linfct
K2 <- glht(m,mcp(paraphrase="Tukey"))$linfct
K3 <- glht(m,mcp(ModalPresent="Tukey"))$linfct
summary(glht(m, linfct = rbind(K1,K2,K3)))

# Interaction terms
# TO ANSWER: is this really a legitimate way of doing interactions?
d$WP = interaction(d$Wh,d$paraphrase)
m.i = lmerTest::lmer(rating ~ WP*ModalPresent + (1|workerid) + (1|tgrep_id), data=d,REML=FALSE) 
summary(glht(m.i,mcp(WP="Tukey")))

d$MP = interaction(d$ModalPresent,d$paraphrase)
m.i = lmerTest::lmer(rating ~ MP*Wh + (1|workerid) + (1|tgrep_id), data=d,REML=FALSE) 
summary(glht(m.i,mcp(MP="Tukey")))

########################################################################
# mean center modalpresent (2-level variables only)


d$ModalPresent[d$ModalPresent == "no"] = "0"
d$ModalPresent[d$ModalPresent == "yes"] = "1"

d$ModalPresent = as.factor(d$ModalPresent)

str(d)
centered = cbind(d,myCenter(d["ModalPresent"]))

head(centered)
summary(centered)
str(centered)
m = lmerTest::lmer(rating ~ cModalPresent*Wh*paraphrase + (1+cModalPresent|workerid) + (1+Wh|workerid) + (1+paraphrase|workerid) + (1|tgrep_id), data=centered,REML=FALSE) 
