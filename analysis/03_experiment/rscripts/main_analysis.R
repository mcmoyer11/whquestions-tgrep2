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

contrasts(d$ModalPresent)
contrasts(d$paraphrase)
#         every the
# a         0   0
# every     1   0
# the       0   1

contrasts(d$Wh)
contrasts(d$Wh) = cbind("how.vs.when"=c(0,1,0,0,0,0),"what.vs.when"=c(1,0,0,0,0,0),
                "where.vs.when"=c(0,0,0,1,0,0),"who.vs.when"=c(0,0,0,0,1,0),
                "why.vs.when"=c(0,0,0,0,0,1))

contrasts(d$paraphrase) = cbind("a.vs.every"=c(1,0,0),"the.vs.every"=c(0,0,1))


# Full Model with random slopes
m = lmerTest::lmer(rating ~ ModalPresent*Wh*paraphrase + (1+paraphrase|workerid) + (1+paraphrase|tgrep_id), data=d,REML=FALSE) 
# message?: boundary (singular) fit: see ?isSingular
summary(m)

m.twowayinteraction = lmer(rating ~ ModalPresent+Wh+paraphrase + ModalPresent:paraphrase + Wh:paraphrase + (1+paraphrase|workerid) + (1+paraphrase|tgrep_id), data=d,REML=FALSE) 
summary(m.twowayinteraction)


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
summary(m.twowayinteraction.centered)
save(m.full, "m.full.RData")

table(centered$ModalPresent,centered$Wh,centered$paraphrase)
table(centered$workerid,centered$paraphrase)
table(centered$workerid,centered$Wh)
table(centered$workerid,centered$ModalPresent)

# "when" model
d_when = d %>% 
  filter(Wh=="when") %>% 
  mutate(cModalPresent = as.numeric(ModalPresent)-mean(as.numeric(ModalPresent))) %>% 
  droplevels()
contrasts(d_when$paraphrase) = cbind("a.vs.every"=c(1,0,0),"the.vs.every"=c(0,0,1))

m.when = lmer(rating ~ cModalPresent*paraphrase + (1+paraphrase|tgrep_id), data=d_when) 
summary(m.when)

m.when.simple = lmer(rating ~ paraphrase*cModalPresent-cModalPresent + (1+paraphrase|tgrep_id), data=d_when) 
summary(m.when.simple)

# "what" model
d_what = d %>% 
  filter(Wh=="what") %>% 
  mutate(cModalPresent = as.numeric(ModalPresent)-mean(as.numeric(ModalPresent))) %>% 
  droplevels()
contrasts(d_what$paraphrase) = cbind("a.vs.every"=c(1,0,0),"the.vs.every"=c(0,0,1))

m.what = lmer(rating ~ cModalPresent*paraphrase + (1+paraphrase|tgrep_id), data=d_what) 
summary(m.what)

m.what.simple = lmer(rating ~ paraphrase*cModalPresent-cModalPresent + (1+paraphrase|tgrep_id), data=d_what) 
summary(m.what.simple)



table(d_when$workerid)
table(d_when$workerid,d_when$paraphrase)
table(d_when$tgrep_id,d_when$paraphrase)
table(d_when$workerid,d_when$ModalPresent)

