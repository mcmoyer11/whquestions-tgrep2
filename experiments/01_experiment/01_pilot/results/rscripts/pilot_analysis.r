# ---
# title: "Analysis of questions: pilot e01"
# author: "mcmoyer"
# date: "November 11, 2020"
# output: html_document
# ---

## Step 1: select stimuli for experiment
setwd("/Users/momo/Dropbox/Stanford/whquestions-tgrep2/experiments/01_experiment/01_pilot/results/rscripts/")
source("/Users/momo/Dropbox/Stanford/whquestions-tgrep2/analysis/helpers.R")
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(tidyverse)
# theme_set(theme_bw())

# Read the database into R.
corp = read.table("/Users/momo/Dropbox/Stanford/whquestions-tgrep2/corpus/results/swbd.tab",sep="\t",header=T,quote="")
# Read the data into R.
d1 = read.csv("/Users/momo/Dropbox/Stanford/whquestions-tgrep2/experiments/01_experiment/01_pilot/results/pilot_e01-merged.csv")

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
dm <- merge(d1, corp_match, by="tgrep_id")
nrow(dm)
# left-join does not
df <- left_join(d1, corp_match, by="tgrep_id")
df$time_in_minutes = as.numeric(as.character(df$time_in_minutes))
write.csv(df,"df_nested.csv")

head(df)
# until i can find a way to unnest, save this as csv and unnest in python,
# then read the csv back in here

############################################################
# trying to UNNEST
t = d$response[[1]]
View(t)
library(purr)
test = d %>% 
  mutate(strange = unnest(., response))

test = d %>% split(d$response)

str(test)
unnest(d,response)

# this is the way to do it:
separate(response,into=c("response_val","response_goodsentence"),sep="\', ") %>% 
d$response_goodsentence = as.factor(as.character(gsub('[{u\'strance_sentence\': ','',sapply(strsplit(as.character(d$response),", u\'sliderval\': "), "[", 1),fixed=T)))
d$response_val = as.numeric(as.character(gsub('}]','',sapply(strsplit(as.character(d$response),", u\'sliderval\': "), "[", 2),fixed=T)))

View(test)
test = cbind(d[1],t(data.frame(d$strange)))

# ############################################################
# read the csv back in
d = read.csv("../total_unnested.csv", header = TRUE)

# read in the contexts too:
d_contexts = read.csv("../../../../clean_corpus/pilot1.txt",sep="\t",header=T,quote="")
head(d_contexts)
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


# Practice trials
practice = d %>%
  filter(tgrep_id %in% c("example1", "example2", "example3", "example4")) %>%
  mutate(response = as.factor(response), is_strange = as.factor(is_strange))

# what are the subjects doing

# something is wrong with the labels..it looks like example3 didn't get labeled properly
# ideally, we will want to look at the first attempts

# these numbers may be off because we tracked participants' first responses
agr = practice %>%
  group_by(tgrep_id, response) %>%
  summarize(count_response = n()) %>%
  group_by(tgrep_id) %>%
  mutate(prop = count_response/sum(count_response))
View(agr)

ggplot(agr,aes(x=tgrep_id, y=prop, fill=response)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90))


# test
test = d %>%
  filter(!tgrep_id %in% c("example1", "example2", "example3", "example4","bot_check")) %>%
  mutate(response = as.factor(response), is_strange = as.factor(is_strange))

agr = test %>%
  group_by(tgrep_id, response) %>%
  summarize(count_response = n()) %>%
  group_by(tgrep_id) %>%
  mutate(prop = count_response/sum(count_response))

View(agr)
ggplot(agr,aes(x=tgrep_id, y=prop, fill=response)) +
  geom_bar(stat="identity") +
  ggsave("../graphs/pilot_test.pdf")
  # theme(axis.text.x = element_text(angle = 60))

View(d_contexts)

agr_strange = test %>%
  group_by(tgrep_id, is_strange) %>%
  summarize(count_is_strange = n()) %>%
  group_by(tgrep_id) %>%
  mutate(prop = count_is_strange/sum(count_is_strange))
View(agr_strange)
ggplot(agr_strange,aes(x=tgrep_id, y=prop, fill=is_strange)) +
  geom_bar(stat="identity")
# theme(axis.text.x = element_text(angle = 60))

