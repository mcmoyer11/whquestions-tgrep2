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
# Model takes FOREVER to run, so save and load later to save time
save(m.full, file="m.full.RData")
load("m.full.RData")

table(centered$ModalPresent,centered$Wh,centered$paraphrase)
table(centered$workerid,centered$paraphrase)
table(centered$workerid,centered$Wh)
table(centered$workerid,centered$ModalPresent)

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
