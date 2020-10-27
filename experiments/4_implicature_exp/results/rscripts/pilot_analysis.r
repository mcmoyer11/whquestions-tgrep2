theme_set(theme_bw(18))
require(tidyverse)
library(stringr)

source("helpers.R")

# load final.csv to merge sentences into tgrep ids
idmatching = read.table(file="../../../1_butnotboth_insertion/results/data/bestresponses.csv",sep="\t",header=T,quote="")
colnames(idmatching) = c("tgrep.id","sentence_original","sentence_bnb")
# load data
d1 = read.table(file="../data/pilot1.csv",sep=",",header=T)
d2 = read.table(file="../data/pilot2.csv",sep=",",header=T)
d2$workerid = d2$workerid + 5
d = rbind(d1,d2)
d = data.frame(lapply(d, function(x) { gsub('\"','',x,fixed=T) } ))
d$Answer.time_in_minutes = as.numeric(as.character(d$Answer.time_in_minutes))
d$slide_number_in_experiment = as.numeric(as.character(d$slide_number_in_experiment))
d$Trial = d$slide_number_in_experiment - 4
nrow(d)
str(d)
summary(d)
head(d)
length(unique(d$tgrep.id))

# look at comments
unique(d$comments)

# fair price
ggplot(d, aes(x=fairprice)) +
  geom_histogram(stat="count")
table(d$fairprice)

# overall assessment
ggplot(d, aes(x=asses)) +
  geom_histogram(stat="count")

# enjoyment
ggplot(d, aes(x=enjoyment)) +
  geom_histogram(stat="count")

# age
ggplot(d, aes(x=age)) +
  geom_histogram(stat="count")

# gender
ggplot(d, aes(x=gender)) +
  geom_histogram(stat="count")

# education
ggplot(d, aes(x=education)) +
  geom_histogram(stat="count")

# language
ggplot(d, aes(x=language)) +
  geom_histogram(stat="count")

# completion time
ggplot(d, aes(x=Answer.time_in_minutes)) +
  geom_histogram(stat="count")


## PRACTICE TRIALS
practice = d %>%
  filter(Trial < 0) %>%
  mutate(response = as.numeric(as.character(response)))

# plot the two example answers
ggplot(practice, aes(x=response,fill=as.factor(Trial))) +
  geom_density(alpha=.5)
  
## TARGET TRIALS
d = d %>%
  filter(Trial > 0) %>%
  left_join(idmatching,by=c("tgrep.id"))
d$response_goodsentence = as.factor(as.character(gsub('[{u\'strance_sentence\': ','',sapply(strsplit(as.character(d$response),", u\'sliderval\': "), "[", 1),fixed=T)))
d = d %>%
  mutate(response_goodsentence = fct_recode(response_goodsentence, 
                                            "good bnb sentence" = "False",
                                            "bad bnb sentence" = "True"))
d$response_val = as.numeric(as.character(gsub('}]','',sapply(strsplit(as.character(d$response),", u\'sliderval\': "), "[", 2),fixed=T)))

# are some cases clearly weirder than others?
dd = d %>%  
  count(tgrep.id,response_goodsentence,sort=T)
View(dd)  

# these should be good:
unique(d[d$tgrep.id %in% c("104373:17","112263:70","14163:45","81953:37"),]$sentence_bnb)

# these should be bad:
unique(d[d$tgrep.id %in% c("69440:43","46957:32","23676:18","105458:174","106988:25"),]$sentence_bnb)

# plot overall distribution of responses -- looks like way more "dissimilar" responses, replicating Degen 2015 :)
ggplot(d, aes(x=response_val)) +
  geom_histogram() 

# plot overall distribution of responses by whether sentence was rated as weird sounding --
# -- more "similar" (implicature) responses for good than for weird sentences
# -- even for good sentences, there are more low (no implicature) than high (implicature) values
ggplot(d, aes(x=response_val,fill=response_goodsentence)) +
  geom_density(alpha=.5) 

# plot subject variability -- some show more bimodality than others
ggplot(d, aes(x=response_val)) +
  geom_histogram() +
  facet_wrap(~workerid)

# plot subject variability by whether they thought the sentences was weird 
# -- not everyone uses the button, but of those who do, they tend to rate those cases lower
ggplot(d, aes(x=response_val,fill=response_goodsentence)) +
  geom_density(alpha=.5) +
  facet_wrap(~workerid,scales="free_y")
  
# implicature ratings by tgrep.id  
agr = d %>%
  group_by(sentence_bnb) %>%
  summarize(Mean=mean(response_val),CILow=ci.low(response_val),CIHigh=ci.high(response_val)) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh,Sentence_BNB=fct_reorder(sentence_bnb,Mean))

ggplot(agr, aes(x=Sentence_BNB,y=Mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  coord_flip() +
  theme(axis.text.y=element_text(size=20))
ggsave(file="../graphs/pilot_casemeans.pdf",height=40,width=45)  

# implicature ratings by tgrep.id  
agr = d %>%
  group_by(sentence_bnb,response_goodsentence) %>%
  summarize(Mean=mean(response_val),CILow=ci.low(response_val),CIHigh=ci.high(response_val)) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

ggplot(agr, aes(x=sentence_bnb,y=Mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  coord_flip() +
  facet_wrap(~response_goodsentence) +
  theme(axis.text.y=element_text(size=20))
ggsave(file="../graphs/pilot_casemeans_bygoodsentence.pdf",height=40,width=45)  














