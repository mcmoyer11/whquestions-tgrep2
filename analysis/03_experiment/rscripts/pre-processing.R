# ---
# title: "Analysis of questions"
# author: "mcmoyer"
# date: "January 18, 2020"
# ---

## Step 1: select stimuli for experiment
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(multcomp) # not available for this version of R
theme_set(theme_bw())
cbPalette <- c("#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73")


this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source("../../helpers.R")
########################################################################
# Read the database into R.
corp = read.table("../../../corpus/results/swbd.tab",sep="\t",header=T,quote="")
# Read the data into R.
d = read.csv("../data/exp03_main-merged.csv")

########################################################################
# Rename the Item_ID variable in the database to Tgrep_ID
names(corp)[names(corp) == "Item_ID"] <- "tgrep_id"

# filter from the database the tgrep_ids from the data
corp_match = corp %>%
  filter(tgrep_id %in% d$tgrep_id)

nrow(corp)
nrow(d) #112031
nrow(corp_match)
length(unique(d$workerid)) # 656
33*20
table(d$proliferate.condition)

exp28: 2974


# join together
d <- left_join(d, corp_match, by="tgrep_id")

length(unique(d$workerid)) # 656
table(d$proliferate.condition)

nrow(d)
d$time_in_minutes = as.numeric(as.character(d$time_in_minutes))
d$rating = as.numeric(d$rating)
# write.csv(df,"df_nested.csv")

# read in the contexts too:
# d_contexts = read.csv("../../../experiments/clean_corpus/pilot2.txt",sep="\t",header=T,quote="")


########################################################################
########################################################################
# comments and demographic information
########################################################################
########################################################################
length(unique(d$workerid)) #656

# look at comments
unique(d$subject_information.comments)

# fair price
ggplot(d, aes(x=subject_information.fairprice)) +
  geom_histogram(stat="count")

# overall assessment
ggplot(d, aes(x=subject_information.enjoyment)) +
  geom_histogram(stat="count")

# gender
ggplot(d, aes(x=subject_information.gender)) +
  geom_histogram(stat="count")

# education
ggplot(d, aes(x=subject_information.education)) +
  geom_histogram(stat="count")


# time_in_minutes
ggplot(d, aes(x=time_in_minutes)) +
  geom_histogram(stat="count")
mean(d$time_in_minutes)

########################################################################
########################################################################
# Practice trials
# TODO: look at just the first choice items
# Note Date: 12/2
########################################################################
########################################################################
# Practice trials
practice = d %>%
  filter(tgrep_id %in% c("example1", "example2", "example3", "example4"))
  # write.csv(.,"practice_01_pilot_e01.csv")
nrow(practice) #16540

######################################################################
# To DO!!!

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

# fixed = read.csv("practice_edited.csv",header=TRUE)
nrow(fixed) # 68
head(fixed)
# remove that one column
# fixed = fixed[c(2:7)]

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

######################################################################

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
  facet_wrap(~tgrep_id,labeller = labeller(tgrep_id = labels))
ggsave("../graphs/main_practice_total.pdf")
# theme(axis.text.x = element_text(angle = 90))


########################################################################
########################################################################
# Control Items
########################################################################
########################################################################
nrow(d)
controls = d %>%
  filter(grepl("control",tgrep_id))
nrow(controls) # 15744
# read in the file to have access to the items
cntrls = read.csv("../../../experiments/clean_corpus/controls.csv",header=TRUE,quote="")
# rename the item column in order to merge on it
names(cntrls)[names(cntrls) == "TGrepID"] <- "tgrep_id"
# join dfs together
c <- left_join(controls, cntrls, by="tgrep_id")

agr = c %>%
  group_by(EntireSentence,paraphrase) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()

ggplot(agr,aes(x=paraphrase, y=mean_rating, fill=paraphrase)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position="dodge") +
  facet_wrap(~EntireSentence, labeller = labeller(Sentence = label_wrap_gen(1)))
# ggsave("../graphs/main_controls.pdf")

########################################################################
########################################################################
# remove subjects who failed 2/6 controls
########################################################################
########################################################################
length(unique(d$workerid)) #656
unique(c$tgrep_id)

# for each control trial type, create a binary measure of whether the
# trial was passed
t = c %>%
  separate(tgrep_id,into=c("tgrep_id","para","trial"),sep="[_]") %>%
  group_by(workerid,paraphrase,trial) %>%
  filter(trial %in% c("movie", "book"), paraphrase %in% c("the")) %>%
  mutate(control_passed = ifelse(rating > .5,"1","0"))
nrow(t) #1312
a1 = c %>%
  separate(tgrep_id,into=c("tgrep_id","para","trial"),sep="_") %>%
  group_by(workerid,paraphrase,trial) %>%
  filter(trial %in% c("novels", "cookies"), paraphrase %in% c("all")) %>%
  mutate(control_passed = ifelse(rating > .5,"1","0"))
nrow(a1) #1312
a2 = c %>%
  separate(tgrep_id,into=c("tgrep_id","para","trial"),sep="_") %>%
  group_by(workerid,paraphrase,trial) %>% 
  filter(trial %in% c("tissue", "napkin"), paraphrase %in% c("a")) %>%
  mutate(control_passed = ifelse(rating > .5,"1","0"))
nrow(a2) #1312

# combine all those files together
con = rbind(t,a1,a2)
nrow(con) #3936

# filter out participants who failed more than 2 controls by taking the sum of 
# all the passed controls, and filtering out workerids who passed more than 2
failed_controls = con %>%
  filter(control_passed == "1") %>%
  group_by(workerid, control_passed) %>%
  summarise(sum_control_passed = n()) %>%
  filter(sum_control_passed < 4)

length(unique(failed_controls$workerid)) # 35
length(unique(failed_controls$workerid))/length(unique(d$workerid))*100 # 4.9% ---> 5.3%

########################################################################
########################################################################
# Test Items
########################################################################
########################################################################
# N before removing ppl who failed 2 controls 
length(unique(d$workerid)) #656

test = d %>%
  filter(!workerid %in% c(failed_controls$workerid)) %>%
  filter(!tgrep_id %in% c("example1", "example2", "example3", "example4","bot_check")) %>%
  filter(!grepl("control",tgrep_id))
# rename all to every
test$paraphrase[test$paraphrase == "all"] = "every"

# N after removing participants who failed 2/6 controls
length(unique(test$workerid)) #621


test[is.na(test)] <- 0

agr = test %>%
  group_by(paraphrase) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()

ggplot(agr,aes(x=paraphrase, y=mean_rating, fill=paraphrase)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position="dodge", show.legend = FALSE) +
  # ggtitle("Overall mean rating for each paraphrase") +
  xlab("Paraphrase") +
  ylab("Mean rating") +
  theme(legend.position = "none")
# ggsave("../graphs/test_overall.pdf")

agr = test %>%
  group_by(Wh,ModalPresent,paraphrase) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()

ggplot(agr,aes(x=paraphrase, y=mean_rating, fill=ModalPresent)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(0.9)) +
  facet_wrap(~Wh)
# ggsave("../graphs/test_ModxWh.pdf")


########################################################################
########################################################################
# Normalize the data by removing rhetorical questions 
# (questions with "other" as the highest rating)
########################################################################
########################################################################
test_agr = test %>%
  group_by(tgrep_id, paraphrase) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating))

other_ratings = test %>%
  group_by(tgrep_id, paraphrase) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  filter((mean_rating[paraphrase == "other"] > mean_rating[paraphrase=="a"]) & 
           (mean_rating[paraphrase == "other"] > mean_rating[paraphrase=="every"]) & 
           (mean_rating[paraphrase == "other"] > mean_rating[paraphrase=="the"]))

nrow(other_ratings)/nrow(test_agr)*100 # 15%
nrow(test_agr)#1340

or_ids = other_ratings$tgrep_id
test_other = test %>%
  filter(tgrep_id %in% or_ids)

# 15% of the items are rhetorical questions
nrow(test_other)
length(unique(test$Sentence))

# filter out those bad guys
test_norm = test %>%
  filter(!tgrep_id %in% or_ids)
nrow(test_norm)/nrow(test)*100 # 84.8%

table(test_norm$Wh,test_norm$ModalPresent)
  
########################################################################
########################################################################
# NORMALIZING TO MAKE A PROBABILITY DISTRIBUTION
# for just "a", "every", and "the"

########################################################################
critical = test_norm %>%
  filter(paraphrase %in% c("every","a","the"))
unique(critical$paraphrase)

critical$ids = paste(critical$workerid,critical$tgrep_id)

critical$ModalPresent = as.factor(critical$ModalPresent)
critical$Wh = as.factor(critical$Wh)
critical$paraphrase = as.factor(critical$paraphrase)

# First renormalize the probability distribution to these three paraphrases
length(unique(critical$tgrep_id)) #842
nrow(critical) #47625

cr = critical %>%
  group_by(ids) %>%
  summarize(rating_sum = sum(rating))
nrow(cr) #47625
# remove duplicate rows

critical = critical %>%
  left_join(cr, by="ids")

View(test[test$tgrep_id == "135330:4",])
t = test %>%
  filter(tgrep_id == "135330:4") %>%
  group_by(paraphrase) %>%
  summarise(mean = mean(rating))

View(t)
critical$factors = paste(critical$ids,critical$paraphrase)

normed_agr = critical %>%
  group_by(factors) %>%
  summarise(normed_rating = rating/rating_sum) %>%
  drop_na() # this removes ALOT of rows
View(normed_agr)
nrow(normed_agr) # 43392

normed = merge(normed_agr,critical,by='factors')
normed[is.na(normed$ModalPresent)] <- "no"

# FIND OUT:
# are there particular items that show bimodality between "other" and another para?

# save to .csv to load into analysis script
# write.csv(normed,"../data/normed.csv")

nrow(normed)# 47625
View(normed)

agr = normed %>%
  group_by(paraphrase) %>%
  summarize(mean_rating = mean(normed_rating), CILow = ci.low(normed_rating), CIHigh = ci.high(normed_rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()

ggplot(agr,aes(x=paraphrase, y=mean_rating, fill=paraphrase)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position="dodge", show.legend = FALSE) +
  # ggtitle("Mean rating for 'a' vs. 'every'") +
  xlab("Paraphrase") +
  ylab("Mean rating") +
  ylim(0,.6) +
  theme(legend.position = "none")
# ggsave("../graphs/final_normed_overall.pdf")

########################################################################
#  MOD X WH
########################################################################

agr = normed %>%
  group_by(Wh,ModalPresent,paraphrase) %>%
  summarize(mean_rating = mean(normed_rating), CILow = ci.low(normed_rating), CIHigh = ci.high(normed_rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()

ggplot(agr,aes(x=paraphrase, y=mean_rating, fill=ModalPresent)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(0.9)) +
  facet_wrap(~Wh, ncol=2) +
  xlab("Paraphrase") +
  ylab("Mean rating") +
  theme(legend.key.size = unit(0.3, "cm"),
        legend.position = "top", # c(.5,1)
        legend.direction = "horizontal")
ggsave("../graphs/final_norm_ModxWh.pdf")


########################################################################
########################################################################
# WH
########################################################################
########################################################################
agr = normed %>%
  group_by(paraphrase,Wh) %>%
  summarize(mean_rating = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()

ggplot(agr,aes(x=Wh, y=mean_rating, fill=paraphrase)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=.25,position=position_dodge(0.9))  +
  # ggtitle("Mean rating for Wh-Word") +
  xlab("Wh-Word") +
  ylab("Mean rating") +
  theme(legend.title = element_blank()) +
  theme(legend.key.size = unit(0.3, "cm"),
        legend.position = "top", # c(.5,1)
        legend.direction = "horizontal")
ggsave("../graphs/final_normed_wh.pdf")


########################################################################
########################################################################
# Modal
########################################################################
########################################################################
agr = normed %>%
  group_by(paraphrase,ModalPresent) %>%
  summarize(mean_rating = mean(normed_rating), CILow = ci.low(normed_rating), CIHigh = ci.high(normed_rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh) %>%
  drop_na()

ggplot(agr,aes(x=ModalPresent, y=mean_rating, fill=paraphrase)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=.25,position=position_dodge(0.9)) +
  xlab("Modal Present") +
  ylab("Mean rating") +
  theme(legend.title = element_blank()) +
  theme(legend.key.size = unit(0.3, "cm"),
        legend.position = "top", # c(.5,1)
        legend.direction = "horizontal")
# legend.spacing.y = unit(-10, 'cm'))
# guides(fill=guide_legend(title="Paraphrase"))
ggsave("../graphs/final_normed_modalpresent.pdf")

########################################################################
# LOOKING AT INDIVIDUAL MODALS
########################################################################
mod = normed %>%
  filter(ModalPresent %in% c("yes")) %>%
  group_by(Modal,paraphrase) %>%
  summarize(mean_rating = mean(normed_rating), CILow = ci.low(normed_rating), CIHigh = ci.high(normed_rating)) %>%
  mutate(YMin = mean_rating - CILow, YMax = mean_rating + CIHigh)

ggplot(mod, aes(x=paraphrase,y=mean_rating,fill=paraphrase)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=.25,position=position_dodge(0.9)) +
  facet_wrap(~Modal)
ggsave("../graphs/final_normed_modals.pdf")

########################################################################
########################################################################
# LOOKING AT ITEMS
########################################################################
########################################################################

the_high = normed %>%
  filter((paraphrase == "the") & (Wh == "what")) %>%
  group_by(tgrep_id,Question) %>%
  summarize(mean_rating = mean(normed_rating)) %>%
  filter(mean_rating > .5)
View(the_high)

a_high = normed %>%
  filter((paraphrase %in% c("a")) & (Wh == "when")) %>%
  group_by(tgrep_id,Question) %>%
  summarize(mean_rating = mean(normed_rating)) %>%
  filter(mean_rating > .3)
View(a_high)

ex = d %>%
  filter(tgrep_id %in% c("102025:30")) %>%
  group_by(paraphrase) %>%
  summarize(mean_rating = mean(normed_rating), CILow = ci.low(normed_rating), CIHigh = ci.high(normed_rating))
View(ex)  

all_high = normed %>%
  filter((paraphrase %in% c("every")) & (Wh == "when")) %>%
  group_by(tgrep_id,Question) %>%
  summarize(mean_rating = mean(normed_rating)) %>%
  filter(mean_rating > .1)
View(all_high)
