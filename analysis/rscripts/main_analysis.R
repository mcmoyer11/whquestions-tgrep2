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
normed$ModalPresent[normed$ModalPresent == "no"] = "0"
normed$ModalPresent[normed$ModalPresent == "yes"] = "1"

normed$ModalPresent = as.factor(normed$ModalPresent)

str(normed)
centered = cbind(normed,myCenter(normed["ModalPresent"]))

head(centered)
summary(centered)
str(centered)
m = lmerTest::lmer(rating ~ cModalPresent*Wh*paraphrase + (1+cModalPresent|workerid) + (1+Wh|workerid) + (1+paraphrase|workerid) + (1|tgrep_id), data=centered,REML=FALSE) 


########################################################################
########################################################################
########################################################################
# Normalizing A and Every and The 



  
View(agr_normed)

agr = agr_normed %>%
  # mutate(normed_rating = rating*2) %>%
  group_by(paraphrase) %>%
  summarize(mean_normed_rating = mean(normed_rating), CILow = ci.low(normed_rating), CIHigh = ci.high(normed_rating)) %>%
  mutate(YMin = mean_normed_rating - CILow, YMax = mean_normed_rating + CIHigh)
View(agr)
ggplot(agr, aes(x=paraphrase,y=mean_normed_rating)) +
  geom_bar(stat="identity",position = "dodge") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=.25,position=position_dodge(0.9))
  # facet_wrap(~ModalPresent)


ggsave("../graphs/.pdf")



########################################################################
########################################################################
########################################################################
########################################################################
# First break up data into each paraphrase,
# Run regression on each

the = subset(test_norm, test_norm$paraphrase == "the")
a = subset(test_norm, test_norm$paraphrase == "a")
all = subset(test_norm, test_norm$paraphrase == "all")
other = subset(test_norm, test_norm$paraphrase == "other")

########################################################################
# start with subjects only intercept

# "The" Paraphrase
m0 = lmer(rating ~ (1|workerid), data=the)
# summary(m0)
the$FittedOnlySubjectVar_the = fitted(m0)

means = the %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedOnlySubjectVar_the),MeanEmpirical = mean(rating))
View(means)
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(0,1) +
  # ylim(0,1) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_0_the.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)


# "A" Paraphrase
m0 = lmer(rating ~ (1|workerid), data=a)
# summary(m0)
a$FittedOnlySubjectVar_a = fitted(m0)

means = a %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedOnlySubjectVar_a),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(0,1) +
  # ylim(0,1) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_0_a.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)


# "All" Paraphrase
m0 = lmer(rating ~ (1|workerid), data=all)
all$FittedOnlySubjectVar_all = fitted(m0)

means = all %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedOnlySubjectVar_all),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(0,1) +
  # ylim(0,1) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_0_all.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)

head(the)
########################################################################
# add fixed effects of interest with random subjects
m.noitems = lmer(rating ~ ModalPresent + Wh + (1|workerid), data=the) 
summary(m.noitems)
the$FittedNoCluster = fitted(m.noitems)

means = the %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedNoCluster),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(1,7) +
  # ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_fixed_the.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)

# don't work with current version of R
r.squaredGLMM(m.noitems) 
vif.mer(m.noitems) 


m.noitems = lmer(rating ~ ModalPresent + Wh + (1|workerid), data=a) 
summary(m.noitems)
a$FittedNoCluster = fitted(m.noitems)

means = a %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedNoCluster),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(1,7) +
  # ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_fixed_a.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)

# don't work with current version of R
r.squaredGLMM(m.noitems) 
vif.mer(m.noitems) 


m.noitems = lmer(rating ~ ModalPresent + Wh + (1|workerid), data=all) 
summary(m.noitems)
all$FittedNoCluster = fitted(m.noitems)

means = all %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedNoCluster),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(1,7) +
  # ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_fixed_all.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)


########################################################################
# add fixed effects of interest no random effects

# add fixed effects of interest
m.noitemsnosubjects = lm(rating ~  ModalPresent + Wh, data=the)
summary(m.noitemsnosubjects)
the$FittedNoItemsNoSubject = fitted(m.noitemsnosubjects)

means = the %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedNoItemsNoSubject),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(1,7) +
  # ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_fixed_norandom_the.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)

m.noitemsnosubjects = lm(rating ~  ModalPresent + Wh, data=a)
summary(m.noitemsnosubjects)
a$FittedNoItemsNoSubject = fitted(m.noitemsnosubjects)

means = a %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedNoItemsNoSubject),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(1,7) +
  # ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_fixed_norandom_a.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)

m.noitemsnosubjects = lm(rating ~  ModalPresent + Wh, data=all)
summary(m.noitemsnosubjects)
all$FittedNoItemsNoSubject = fitted(m.noitemsnosubjects)

means = all %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedNoItemsNoSubject),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(1,7) +
  # ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_fixed_norandom_all.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)


########################################################################
# add fixed effects of interest with random subjects and items intercepts
m.noitems = lmer(rating ~ ModalPresent + Wh + (1|workerid) + (1|tgrep_id), data=the) 
summary(m.noitems)
the$FittedNoClusterItem = fitted(m.noitems)

means = the %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedNoClusterItem),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(1,7) +
  # ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_full_the.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)

# plot of raw histogram
ggplot(the, aes(x=rating)) +
  geom_histogram()
ggsave("../graphs/histogram_raw_the.pdf",width=4,height=3)

# plot of means histogram
ggplot(means, aes(x=MeanEmpirical)) +
  geom_histogram()
ggsave("../graphs/histogram_means_the.pdf",width=4,height=3)


m.noitems.3 = lmerTest::lmer(rating ~ ModalPresent*Wh + (1|workerid) + (1|tgrep_id), data=a, REML=FALSE) 
m.noitems.3a = lmerTest::lmer(rating ~ ModalPresent + Wh + (1|workerid) + (1|tgrep_id), data=a, REML=FALSE) 
m.noitems.2 = lmerTest::lmer(rating ~ Wh + (1|workerid) + (1|tgrep_id), data=a, REML=FALSE) 
m.noitems.1 = lmerTest::lmer(rating ~ ModalPresent + (1|workerid) + (1|tgrep_id), data=a, REML=FALSE) 
summary(m.noitems.3a)
anova(m.noitems.3,m.noitems.3a)
# ModalPresent
anova(m.noitems.3,m.noitems.2) # ***
# Wh
anova(m.noitems.3,m.noitems.1) # **

summary(m.noitems)
a$FittedNoClusterItem = fitted(m.noitems)

means = a %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedNoClusterItem),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(1,7) +
  # ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_full_a.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)

# plot of raw histogram
ggplot(a, aes(x=rating)) +
  geom_histogram()
ggsave("../graphs/histogram_raw_a.pdf",width=4,height=3)

# plot of means histogram
ggplot(means, aes(x=MeanEmpirical)) +
  geom_histogram()
ggsave("../graphs/histogram_means_a.pdf",width=4,height=3)


m.noitems.3 = lmer(rating ~ ModalPresent*Wh + (1|workerid) + (1|tgrep_id), data=all, REML=FALSE) 
m.noitems.2 = lmerTest::lmer(rating ~ Wh + (1|workerid) + (1|tgrep_id), data=all, REML=FALSE) 
m.noitems.1 = lmerTest::lmer(rating ~ ModalPresent + (1|workerid) + (1|tgrep_id), data=all,  REML=FALSE) 
summary(m.noitems.3)
# ModalPresent
anova(m.noitems.3,m.noitems.2) # ns
# Wh
anova(m.noitems.3,m.noitems.1) # ns

summary(m.noitems)
all$FittedNoClusterItem = fitted(m.noitems)

means = all %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedNoClusterItem),MeanEmpirical = mean(rating))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  # xlim(1,7) +
  # ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_full_all.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)

# plot of raw histogram
ggplot(all, aes(x=rating)) +
  geom_histogram()
ggsave("../graphs/histogram_raw_all.pdf",width=4,height=3)

# plot of means histogram
ggplot(means, aes(x=MeanEmpirical)) +
  geom_histogram()
ggsave("../graphs/histogram_means_all.pdf",width=4,height=3)



# COMPARING ALL and A
# all vs. a
alla = rbind(all,a)
m.noitems.3 = lmerTest::lmer(rating ~ ModalPresent + Wh +paraphrase + (1|workerid) + (1|tgrep_id), data=alla) 
m.noitems.2 = lmerTest::lmer(rating ~ Wh + ModalPresent + (1|workerid) + (1|tgrep_id), data=alla) 
# m.noitems.1 = lmerTest::lmer(rating ~ ModalPresent + (1|workerid) + (1|tgrep_id), data=alla) 
summary(m.noitems.3)
# ModalPresent
anova(m.noitems.3,m.noitems.2)
# Wh
anova(m.noitems.3,m.noitems.1) # **

m.noitems.3 = lmer(rating ~ ModalPresent + Wh + paraphrase + (1|workerid) + (1|tgrep_id), data=alla) 
m.noitems.2 = lmerTest::lmer(rating ~ Wh + ModalPresent + (1|workerid) + (1|tgrep_id), data=alla) 
# m.noitems.1 = lmerTest::lmer(rating ~ ModalPresent + (1|workerid) + (1|tgrep_id), data=alla) 
summary(m.noitems.3)
# ModalPresent
anova(m.noitems.3,m.noitems.2)

alla_mods = alla %>%
  filter(ModalPresent %in% c("yes"))
m.noitems.3 = lmerTest::lmer(rating ~ Wh + paraphrase + Modal + (1|workerid) + (1|tgrep_id), data=alla_mods, REML=FALSE) 
m.noitems.2 = lmerTest::lmer(rating ~ Wh + Modal + (1|workerid) + (1|tgrep_id), data=alla_mods, REML=FALSE) 
# m.noitems.1 = lmerTest::lmer(rating ~ ModalPresent + (1|workerid) + (1|tgrep_id), data=alla) 
summary(m.noitems.3)
# ModalPresent
