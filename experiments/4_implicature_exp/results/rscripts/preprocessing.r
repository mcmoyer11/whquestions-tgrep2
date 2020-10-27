theme_set(theme_bw(18))
require(tidyverse)
library(stringr)
# library(lmerTest)
library(brms)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("helpers.R")

# load final.csv to merge sentences into tgrep ids
idmatching = read.table(file="../../../1_butnotboth_insertion/results/data/bestresponses.csv",sep="\t",header=T,quote="")
colnames(idmatching) = c("tgrep.id","sentence_original","sentence_bnb") # need to update to reflect new (better) bnb sentence

# load data
d = read_csv("../../experiments/proliferate/03_main2/main-merged.csv") %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(workerid = as.factor(as.character(workerid))) %>%
  rename(tgrep.id=tgrep_id) %>%
  filter(!tgrep.id %in% c("bot_check","example1","example2")) %>% 
  droplevels()

d$Trial = d$slide_number_in_experiment - 5
nrow(d)
str(d)
summary(d)
names(d)
length(unique(d$tgrep.id))

# look at comments
unique(d$subject_information.comments)
# "After the first few, I mainly just read the last statement in the conversation and the blue sentence as I felt the previous conversation topics did not affect how I rated the definitions of the red and blue sentences"  
# "Most of the sentences were very strange; "but not both", even when it didn't change the meaning, generally felt pretty unnatural to add." 
# mixed enjoyment: 
# - frustration with disfluencies; difficulty in understanding contexts
# - enjoyment/reports of being intrigued

# A GLITCH TO LOOK INTO:
#  i encountered an error once where there was no blue text and once where there were random characters in the blue text portion that made the text unintelligible                                # One of the earlier sentences didn't include a blue sentence at all, I think that was a glitch.  

subjects = unique(d[,c("workerid","subject_information.comments","subject_information.fairprice","subject_information.asses","subject_information.enjoyment","subject_information.age","subject_information.gender","subject_information.education","subject_information.language","time_in_minutes")])

# fair price
ggplot(subjects, aes(x=subject_information.fairprice)) +
  geom_histogram(stat="count")

# overall assessment
ggplot(subjects, aes(x=subject_information.asses)) +
  geom_histogram(stat="count")

# enjoyment
ggplot(subjects, aes(x=subject_information.enjoyment)) +
  geom_histogram(stat="count")

# age
ggplot(subjects, aes(x=subject_information.age)) +
  geom_histogram(stat="count")

# gender
ggplot(subjects, aes(x=subject_information.gender)) +
  geom_histogram(stat="count")

# education
ggplot(subjects, aes(x=subject_information.education)) +
  geom_histogram(stat="count")

# language
ggplot(subjects, aes(x=subject_information.language)) +
  geom_histogram(stat="count") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))

# completion time
ggplot(subjects, aes(x=time_in_minutes)) +
  geom_histogram()
summary(subjects$time_in_minutes)

# fast subjects (less than 5 minutes) -- should exclude:
fastsubjects = as.character(subjects[subjects$workerid %in% as.character(subjects[subjects$time_in_minutes < 5,]$workerid),]$workerid)

## PRACTICE TRIALS
practice = d %>%
  filter(Trial < 0) %>%
  mutate(response = as.numeric(as.character(response)))

# plot the two example answers
# ggplot(practice, aes(x=response,fill=as.factor(Trial))) +
  # geom_density(alpha=.5)
  
## TARGET TRIALS
d = d %>%
  filter(Trial > 0) %>%
  left_join(idmatching,by=c("tgrep.id")) %>% 
  separate(response,into=c("response_val","response_goodsentence"),sep="\', ") %>% 
  mutate(response_val=as.numeric(str_extract(response_val,"[:digit:]")), response_goodsentence=str_extract(response_goodsentence,"[:alpha:]")) 
str(d)

d = d %>%
  mutate(response_goodsentence = fct_recode(response_goodsentence, 
                                            "good bnb sentence" = "F",
                                            "bad bnb sentence" = "T"),
         sentence_length = nchar(as.character(sentence_bnb)))

d$log_sentence_length = log(d$sentence_length)

# are some cases clearly weirder than others?
dd = d %>%  
  count(tgrep.id,response_goodsentence,sort=T)
View(dd)  

# these should be good:
d[d$tgrep.id %in% c("104373:17","112263:70","112347:54","112417:64"),c("sentence_bnb","response_goodsentence")] %>% 
  arrange(sentence_bnb) %>% 
  View() 

# these should be bad:
d[d$tgrep.id %in% c("166599:24","46957:32","23676:18","105458:174","106988:25"),c("sentence_bnb","response_goodsentence")] %>% 
  arrange(sentence_bnb) %>% 
  View() 

# plot overall distribution of responses -- looks like way more "dissimilar" responses, replicating Degen 2015 :)
agr = d %>%
  group_by(tgrep.id) %>%
  summarize(Mean=mean(response_val))
ggplot(agr, aes(x=Mean)) +
  geom_histogram() +
  xlab("Mean rating by item")
ggsave("../graphs/responses.pdf",height=3.5,width=6)

# plot overall distribution of responses by whether sentence was rated as weird sounding --
# -- more "similar" (implicature) responses for good than for weird sentences
# -- even for good sentences, there are more low (no implicature) than high (implicature) values
ggplot(d, aes(x=response_val,fill=response_goodsentence)) +
  geom_density(alpha=.5) 

# plot subject variability -- some show more bimodality than others
p = ggplot(d, aes(x=response_val)) +
  geom_histogram() +
  facet_wrap(~workerid)


# plot subject variability by whether they thought the sentences was weird 
# -- not everyone uses the button, but of those who do, they tend to rate those cases lower
p = ggplot(d, aes(x=response_val,fill=response_goodsentence)) +
  geom_density(alpha=.5) +
  facet_wrap(~workerid,scales="free_y")
ggsave(p,file="../graphs/subject_variability.pdf",height=40,width=40)

# implicature ratings by tgrep.id, include proportion of good sentences  
good = d %>% 
  group_by(tgrep.id,response_goodsentence) %>%
  summarize(counts = n()) %>%
  group_by(tgrep.id) %>%
  mutate(total = sum(counts),proportion_goodsentence=counts/total) %>%
  filter(response_goodsentence == "good bnb sentence") %>%
  select(tgrep.id,proportion_goodsentence) 
agr = d %>%
  group_by(sentence_bnb,tgrep.id) %>%
  summarize(Mean=mean(response_val),CILow=ci.low(response_val),CIHigh=ci.high(response_val)) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh,Sentence_BNB=fct_reorder(sentence_bnb,Mean)) %>%
  arrange(Mean) %>%
  left_join(good,by=c("tgrep.id"))
agr$proportion_goodsentence = replace_na(agr$proportion_goodsentence,0)

d = d %>% 
  rename(gender=subject_information.gender,age=subject_information.age)
  
write.csv(agr[,c("tgrep.id","proportion_goodsentence","Sentence_BNB","Mean","YMin","YMax")],file="../data/sentence_si_means.csv",row.names=F)

write.csv(d[,c("tgrep.id","workerid","sentence_bnb","response_goodsentence","response_val","Trial","gender","age","proliferate.condition","time_in_minutes")],file="../data/data.csv",row.names=F)

ggplot(agr, aes(x=Sentence_BNB,y=Mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  coord_flip() +
  theme(axis.text.y=element_text(size=20))
# ggsave(file="../graphs/pilot_casemeans.pdf",height=40,width=45)  

# implicature ratings by tgrep.id and whether or not sentence was good and include proportion of good/bad sentences
agr = d %>%
  group_by(tgrep.id,sentence_bnb,response_goodsentence) %>%
  summarize(Mean=mean(response_val),CILow=ci.low(response_val),CIHigh=ci.high(response_val)) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)  %>% 
  arrange(Mean,response_goodsentence)
write.csv(agr[,c("tgrep.id","sentence_bnb","response_goodsentence","Mean","YMin","YMax")],file="../data/sentence_si_means_bygoodsentence.csv",row.names=F)

ggplot(agr, aes(x=sentence_bnb,y=Mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  coord_flip() +
  facet_wrap(~response_goodsentence) +
  theme(axis.text.y=element_text(size=20))
# ggsave(file="../graphs/pilot_casemeans_bygoodsentence.pdf",height=40,width=45)  

