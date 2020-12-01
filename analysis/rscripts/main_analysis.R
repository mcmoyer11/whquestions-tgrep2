# ---
# title: "Analysis of questions: pilot e02"
# author: "mcmoyer"
# date: "November 20, 2020"
# output: html_document
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
theme_set(theme_bw())

# Read the database into R.
corp = read.table("/Users/momo/Dropbox/Stanford/whquestions-tgrep2/corpus/results/swbd.tab",sep="\t",header=T,quote="")
# Read the data into R.
d1 = read.csv("/Users/momo/Dropbox/Stanford/whquestions-tgrep2/analysis/data/main-merged.csv")

str(d1)
# Rename the Item_ID variable in the database to Tgrep_ID
names(corp)[names(corp) == "Item_ID"] <- "tgrep_id"

# filter from the database the tgrep_ids from the data
corp_match = corp %>%
  filter(tgrep_id %in% d1$tgrep_id)

nrow(corp)
nrow(d1)
nrow(corp_match)

# join the two dataframes together 
# merging will remove the values of "tgrep_id" that aren't shared
# dm <- merge(d1, corp_match, by="tgrep_id")
# nrow(dm)
# left-join does not
d <- left_join(d1, corp_match, by="tgrep_id")
d$time_in_minutes = as.numeric(as.character(d$time_in_minutes))
d$rating = as.numeric(d$rating)
# write.csv(df,"df_nested.csv")

head(d)
# until i can find a way to unnest, save this as csv and unnest in python,
# then read the csv back in here
str(d)


# read in the contexts too:
d_contexts = read.csv("../../experiments/clean_corpus/pilot2.txt",sep="\t",header=T,quote="")

# look at comments
unique(d$subject_information.comments)

# fair price
ggplot(d, aes(x=subject_information.fairprice)) +
  geom_histogram(stat="count")
table(d$subject_information.fairprice)

# overall assessment
ggplot(d, aes(x=subject_information.enjoyment)) +
  geom_histogram(stat="count")

# gender
ggplot(d, aes(x=subject_information.gender)) +
  geom_histogram(stat="count")


# language
ggplot(d, aes(x=subject_information.language)) +
  geom_histogram(stat="count")

# education
ggplot(d, aes(x=subject_information.education)) +
  geom_histogram(stat="count")


# time_in_minutes
ggplot(d, aes(x=time_in_minutes)) +
  geom_histogram(stat="count")
mean(d$time_in_minutes)

# Practice trials
practice = d %>%
  filter(tgrep_id %in% c("example1", "example2", "example3", "example4"))
  # write.csv(.,"practice_01_pilot_e01.csv")
View(practice)


# look at just the first practice trial
prac_agr = practice %>%
  group_by(workerid,tgrep_id,paraphrase,rating) %>%
  summarise(count = n()) %>%
  group_by(workerid,tgrep_id) %>%
  mutate(total_per_ex = sum(count))
nrow(prac_agr) # 6102

prac_agr_rem = practice %>%
  group_by(workerid,tgrep_id,paraphrase,rating) %>%
  summarise(count = n()) %>%
  group_by(workerid,tgrep_id) %>%
  mutate(total_per_ex = sum(count)) %>%
  filter(total_per_ex > 4)
  # write.csv(.,"practice_to_edit.csv")
nrow(prac_agr_rem) # 2014

fixed = read.csv("practice_edited.csv",header=TRUE)
nrow(fixed) # 68
head(fixed)
# remove that one column
fixed = fixed[c(2:7)]

head(fixed)
prac_agr_keep = practice %>%
  group_by(workerid,tgrep_id,paraphrase,rating) %>%
  summarise(count = n()) %>%
  group_by(workerid,tgrep_id) %>%
  mutate(total_per_ex = sum(count)) %>%
  filter(total_per_ex <= 4)
nrow(prac_agr_keep) # 4088

practice_first = rbind(fixed,prac_agr_keep)
nrow(practice_first) #320


agr = practice %>%
  group_by(tgrep_id, paraphrase) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()
# View(agr)

labels = c("Who can help spread the word?","Where can I get coffee around here?","Who came to the party?","How do I get to Central Park?")
names(labels) = c("example1","example2","example3", "example4")
# View(labels)
ggplot(agr,aes(x=paraphrase, y=mean_rating, fill=paraphrase)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position="dodge") +
  facet_wrap(~tgrep_id,labeller = labeller(tgrep_id = labels)) +
  ggsave("../graphs/main_practice_total.pdf")
# theme(axis.text.x = element_text(angle = 90))

d$tgrep_id
# controls
controls = d1 %>%
  filter(grepl("control",tgrep_id))
nrow(controls) # 720
cntrls = read.csv("../../experiments/clean_corpus/controls.csv",header=TRUE,quote="")
names(cntrls)[names(cntrls) == "TGrepID"] <- "tgrep_id"

# View(controls)

c <- left_join(controls, cntrls, by="tgrep_id")
View(c)


# View(controls)
agr = c %>%
  group_by(EntireSentence,paraphrase) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()
# View(agr)

ggplot(agr,aes(x=paraphrase, y=mean_rating, fill=paraphrase)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position="dodge") +
  facet_wrap(~EntireSentence, labeller = labeller(Sentence = label_wrap_gen(1))) +
  ggsave("../graphs/main_controls.pdf")


# test
test = d %>%
  filter(!tgrep_id %in% c("example1", "example2", "example3", "example4","bot_check")) %>%
  filter(!grepl("control",tgrep_id))
View(test)

agr = test %>%
  group_by(paraphrase) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()
View(agr)

ggplot(agr,aes(x=paraphrase, y=mean_rating, fill=paraphrase)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position="dodge") +
  ggsave("../graphs/main_test_overall.pdf")
  # facet_wrap(~Wh)

agr = test %>%
  group_by(Wh,ModalPresent,paraphrase) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()
# View(agr)

ggplot(agr,aes(x=paraphrase, y=mean_rating, fill=ModalPresent)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(0.9)) +
  facet_wrap(~Wh) +
  ggsave("../graphs/main_test_ModxWh.pdf")


# rename 'ca' in modals to 'can'
test$Modal[test$Modal == "ca"] = "can"

agr = test %>%
  filter(ModalPresent %in% c("yes")) %>%
  group_by(Wh,Modal,paraphrase) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()
View(agr)

ggplot(agr,aes(x=paraphrase, y=mean_rating, fill=Modal)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=.25,position=position_dodge(0.9)) +
  facet_wrap(~Wh) +
  ggsave("../graphs/main_test_ModalsxWh.pdf")

table(d$ModalPresent,)

head(test)
agr = test %>%
  group_by(paraphrase,Question) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh)

# subject variablility
ggplot(agr, aes(x=Question,y=mean_rating,fill=Question)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=.25,position=position_dodge(0.9)) +
  facet_wrap(~paraphrase) +
  theme(legend.position = "none")
  # ggsave("../graphs/main_byitem.pdf")

ggplot(agr, aes(x=mean_rating)) +
  geom_histogram(stat="count") +
  facet_wrap(~paraphrase)
  # theme(legend.position = "none")


the_high = agr %>%
  filter(paraphrase %in% c("the") &  mean_rating > .5)

the_high["Question"]  

a_high = agr %>%
  filter(paraphrase %in% c("a") &  mean_rating > .5)
a_high["Question"]

all_high = agr %>%
  filter(paraphrase %in% c("all") &  mean_rating > .5)
all_high["Question"]

cbPalette <- c("#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73")
########################################################################
# Regression Models
########################################################################
# First break up data into each paraphrase,
# Run logistic regression in each

the = subset(test, test$paraphrase == "the")
a = subset(test, test$paraphrase == "a")
all = subset(test, test$paraphrase == "all")
other = subset(test, test$paraphrase == "other")

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
m.noitems = lmer(rating ~ ModalPresent + Wh + DeterminerSubjPresent + DeterminerNonSubjPresent  + (1|workerid), data=the) 
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


m.noitems = lmer(rating ~ ModalPresent + Wh + DeterminerSubjPresent + DeterminerNonSubjPresent  + (1|workerid), data=a) 
summary(m.noitems)
a$FittedNoCluster = fitted(m.noitems)

means = a %>%
  group_by(tgrep_id) %>%
  summarize(MeanPredicted = mean(FittedNoCluster),MeanEmpirical = mean(rating))
View(means)
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


m.noitems = lmer(rating ~ ModalPresent + Wh + DeterminerSubjPresent + DeterminerNonSubjPresent  + (1|workerid), data=all) 
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
m.noitemsnosubjects = lm(rating ~  ModalPresent + Wh + DeterminerSubjPresent + DeterminerNonSubjPresent, data=the)
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

m.noitemsnosubjects = lm(rating ~  ModalPresent + Wh + DeterminerSubjPresent + DeterminerNonSubjPresent, data=a)
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

m.noitemsnosubjects = lm(rating ~  ModalPresent + Wh + DeterminerSubjPresent + DeterminerNonSubjPresent, data=all)
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
m.noitems = lmer(rating ~ ModalPresent + Wh + DeterminerSubjPresent + DeterminerNonSubjPresent  + (1|workerid) + (1|tgrep_id), data=the) 
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


m.noitems = lmer(rating ~ ModalPresent + Wh + DeterminerSubjPresent + DeterminerNonSubjPresent  + (1|workerid) + (1|tgrep_id), data=a) 
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


m.noitems = lmer(rating ~ ModalPresent + Wh + DeterminerSubjPresent + DeterminerNonSubjPresent  + (1|workerid) + (1|tgrep_id), data=all) 
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
