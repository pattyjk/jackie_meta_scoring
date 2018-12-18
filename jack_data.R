#################Split meta analysis############
setwd("./jack_data/meta")
data<-read.delim("RDoC_Trimmed.txt", header=T)
str(data)

#split data by study
data_split<-split(data, data$PMID)

#remove columns with NAs
data_split2<-lapply(data_split, function(df) df[, colSums(is.na(df)) == 0])

#write it to csvs
for(i in names(data_split2)){
  write.csv(data_split2[[i]], paste0(i,".csv"), quote=F, row.names=F)
}


###Meta analysis
setwd("./jack_data/meta")
library(readr)
library(ggplot2)
library(metafor)
sleep <- read_csv("FINAL MASTER_sleepsuic.csv")

#calculate log response ratio
LRR<-escalc(measure="ROM", data=sleep, append=F, m1i=sleep$case.m, n1i=sleep$case.n, sd1i=sleep$case.sd, m2i=sleep$ctrl.m, n2i=sleep$ctrl.n, sd2i=sleep$ctrl.sd)
class(LRR)

#merge to original frame
sleep_2<-cbind(sleep, as.data.frame(LRR))
funnel(sleep_2$yi, sleep_2$vi)

#calculated hedges g
hedges<-escalc(measure="ROM", data=sleep, append=F, m1i=sleep$case.m, n1i=sleep$case.n, sd1i=sleep$case.sd, m2i=sleep$ctrl.m, n2i=sleep$ctrl.n, sd2i=sleep$ctrl.sd)
class(hedges)

#bind data to original frame
sleep3<-cbind(sleep, as.data.frame(hedges))

#funnel plot
funnel(sleep3$yi, sleep3$vi)
funnel(sleep_2$yi, sleep_2$vi)

#forest plot
forest(sleep3$yi, sleep3$vi, slab=sleep3$metaid, pch=19, subset=order(sleep3$yi))
forest(sleep_2$yi, sleep_2$vi, slab=sleep_2$metaid, pch=19, subset=order(sleep_2$yi))

#infulsence of one study
inf<-influence(hedges)

#plot raw log response ratio
ggplot(sleep, aes(metaid, LRR))+
  geom_point()+
  theme_bw()+
  xlab("Study ID")+
  ylab("Log Response Ratio")+
  coord_flip()

#plot raw hedges
ggplot(sleep, aes(metaid, sleep$yi))+
  geom_point()+
  theme_bw()+
  xlab("Study ID")+
  ylab("Hedges G")+
  coord_flip()

#plot hedges by sleep con
ggplot(sleep3, aes(sleep3$basicsleepcon, sleep3$yi))+
  geom_boxplot()+
  theme_bw()+
  xlab("Basic Sleep Con")+
  ylab("Hedges G")+s
  coord_flip()

#plot hedges by STB construct
ggplot(sleep3, aes(sleep3$STBconstruct, sleep3$yi))+
  geom_boxplot()+
  theme_bw()+
  xlab("STB Construct")+
  ylab("Hedges G")+
  coord_flip()



#########################################################################
#####################Colour data#########################################
#########################################################################
setwd("/Users/patty/Dropbox/R/jack_data/")
#read in data
#file can't have white space between entries, needs to be commas
#file has to be each participant on one line, each separated by a comma (seach ] and replace with ],)
library(readr)
jack_test <- read_delim("attn_fix.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
#split the column by commas
#install.package("stringr")
library(stringr)
jack_split<-str_split_fixed(jack_test$X1, ",", n=Inf)
#use the first row as column names
colnames(jack_split)<-jack_split[1,]


##########Getting headers for columns
#remove extraa crap from headers
colnames(jack_split)<-gsub("bag|text|blue|practice|single-stim|ball|red|tape|basket|cat|desk|table|radio", '', colnames(jack_split))
#remove all numbers
colnames(jack_split)<-gsub("[[:digit:]]+", '', colnames(jack_split))
colnames(jack_split)<-gsub("[[:punct:]]", '', colnames(jack_split))
colnames(jack_split)<-gsub("[[:space:]]", '', colnames(jack_split))

colnames(jack_split)<-gsub("keypressedm", 'keypressed', colnames(jack_split))
colnames(jack_split)<-gsub("correctresponsecorrect", 'correctresponse', colnames(jack_split))
colnames(jack_split)<-gsub("keypressedc", 'keypressed', colnames(jack_split))
colnames(jack_split)<-gsub("stimulusrejected",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordrejected",	'word', colnames(jack_split))
colnames(jack_split)<-gsub("blocktest",	'	block', colnames(jack_split))
colnames(jack_split)<-gsub("wordtypenegative",	'word', colnames(jack_split))
colnames(jack_split)<-gsub("stimulusfuneral",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordfuneral",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("wordtypesuicide",	'	wordtype', colnames(jack_split))
colnames(jack_split)<-gsub("stimulussuicide",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordsuicide",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("stimulusdead",	'	wordtype', colnames(jack_split))
colnames(jack_split)<-gsub("worddead",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("stimulushappy",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordhappy",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("wordtypepositive",	'	wordtype', colnames(jack_split))
colnames(jack_split)<-gsub("stimulusengine",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordengine",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("wordtypeneutral",	'	wordtype', colnames(jack_split))
colnames(jack_split)<-gsub("stimulussuccess",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordsuccess",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("stimulusalone",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordalone",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("stimulusmuseum",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordmuseum",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("stimulusstupid",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordstupid",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("stimuluspaper",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordpaper",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("stimuluspleasure",	'	stimulus', colnames(jack_split))
colnames(jack_split)<-gsub("wordpleasure",	'	word', colnames(jack_split))
colnames(jack_split)<-gsub("keypresstimeout",	'	keypress', colnames(jack_split))
colnames(jack_split)<-gsub("trialtypeTimed",	'	trialtype', colnames(jack_split))

#remove text and punctuation from data rows
jack_split<-gsub("block_num|color|correct_response|internal_node_id|key_pressed|participant_response|key_press|stimulus_desk|stimulus_radio|stimulus_table|time_elapsed|trial_index|trial_type|word_desk|word_radio|word_table|word_type|block|word|rt|stimulus", '', jack_split)
jack_split<-gsub("[[:punct:]]", '', jack_split)

#remove 'internodeid' columns
jack_split<-as.data.frame(jack_split[,-grep('internalnodeid', colnames(jack_split))])

#write cleaned up data file to a .csv
write.csv(jack_split, 'jack_split_attn.csv')

#re-read in data
jack_attn <- read.delim("C:/Users/patty/Dropbox/R/jack_data/jack_attn.txt")

jack_attn<-jack_split

#remove practice runs
jack_attn_pact<-jack_attn[,c(1:341)]
jack_attn<-jack_attn[,-c(1:341)]

#remove rows
jack_attn<-jack_attn[,-c(1862:1866)]
jack_attn<-jack_attn[,-c(1:5)]

#re-read in data
write.csv(jack_attn, 'jack_attn.csv')
jack_attn <- read.delim("C:/Users/patty/Dropbox/R/jack_data/attn_fix.txt")

#stack table (can change k to alter number of columns used)
names(jack_attn)<-gsub("[[:punct:]]", '', names(jack_attn))
names(jack_attn)<-gsub("[[:digit:]]", '', names(jack_attn))

k <- 29
nr <- nrow(jack_attn)
nc <- ncol(jack_attn)
unames <- names(jack_attn[1:29])

a <- array(as.matrix(jack_attn), c(nr, k, nc/k))
m <- matrix(aperm(a, c(1, 3, 2)),, k, dimnames = list(NULL, unames))
jack_split_attn<-as.data.frame(m, stringsAsFactors = FALSE)

#write to file
write.csv(jack_split_attn, 'jack_split_attn_full.csv')

#add subject names
colour_subject <- read.table("colour_subject.txt", quote="\"", comment.char="", header=T)
jack_split_attn<-cbind(colour_subject, jack_split_attn)

#split file by subject
jack_attn_subject<-split(jack_split_attn, jack_split_attn$SubjectID)


#write each subject ot own file
setwd("./attn_split/")

for(i in names(jack_attn_subject)){
  write.csv(jack_attn_subject[[i]], paste0(i,".csv"))
}

#setw wd back to default
setwd("C:/Users/patty/Dropbox/R/jack_data/")

#calculate the stats
#global mean
jacky_colour_glob_means<-ddply(jack_split_attn, c("SubjectID"), summarise, mean=mean(as.numeric(rt.1)), sd=sd(as.numeric(rt.1)), pos_2sd=2*sd(rt.1)+mean(as.numeric(rt.1)), neg_2sd=mean(as.numeric(rt.1))-2*sd(as.numeric(rt.1)), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))
jacky_colour_type_means<-ddply(jack_split_attn, c("SubjectID", "wordtype"), summarise, mean=mean(as.numeric(rt.1)), sd=sd(as.numeric(rt.1)), pos_2sd=2*sd(rt.1)+mean(as.numeric(rt.1)), neg_2sd=mean(as.numeric(rt.1))-2*sd(as.numeric(rt.1)), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))

#global means of valid, unvalid
valid_split<-split(jack_split_attn, jack_split_attn$validity)
invalid_means<-ddply(valid_split$validityinvalid, c("SubjectID", "wordtype"), summarise, mean=mean(as.numeric(rt.1)), sd=sd(as.numeric(rt.1)), pos_2sd=2*sd(rt.1)+mean(as.numeric(rt.1)), neg_2sd=mean(as.numeric(rt.1))-2*sd(as.numeric(rt.1)), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))
valid_means<-ddply(valid_split$validityvalid, c("SubjectID", "wordtype"), summarise, mean=mean(as.numeric(rt.1)), sd=sd(as.numeric(rt.1)), pos_2sd=2*sd(rt.1)+mean(as.numeric(rt.1)), neg_2sd=mean(as.numeric(rt.1))-2*sd(as.numeric(rt.1)), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))
valid_global_mean<-ddply(valid_split$validityvalid, c("SubjectID"), summarise, mean=mean(as.numeric(rt.1)), sd=sd(as.numeric(rt.1)), pos_2sd=2*sd(rt.1)+mean(as.numeric(rt.1)), neg_2sd=mean(as.numeric(rt.1))-2*sd(as.numeric(rt.1)), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))
invalid_global_mean<-ddply(valid_split$validityinvalid c("SubjectID"), summarise, mean=mean(as.numeric(rt.1)), sd=sd(as.numeric(rt.1)), pos_2sd=2*sd(rt.1)+mean(as.numeric(rt.1)), neg_2sd=mean(as.numeric(rt.1))-2*sd(as.numeric(rt.1)), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))

#########################################################################
#####################Attention data######################################
#########################################################################
setwd("/Users/patty/Dropbox/R/jack_data/")
#install.package("readr")
library(readr)
jack_test2 <- read_delim("col_fix.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
#remove this crap
jack_test2$X1 <- gsub(",oe", 'oe', jack_test2$X1)
#split the column by commas
#install.package("stringr")
library(stringr)
jacky_split<-str_split_fixed(jack_test2$X1, ",", n=Inf)

#append column headers to table
colnames(jacky_split)<-jacky_split[1,]

#make into data frame
#jacky_split<-as.data.frame(jacky_split)

#remove punctuation
jacky_split<-gsub("[[:punct:]]", '', jacky_split)

#remove other stuff
jacky_split<-gsub("rt", '', jacky_split)
jacky_split<-gsub("stimulus", '', jacky_split)
jacky_split<-gsub("keypress", '', jacky_split)
jacky_split<-gsub("word", '', jacky_split)
jacky_split<-gsub("wordposition", '', jacky_split)
jacky_split<-gsub("validity", '', jacky_split)
jacky_split<-gsub("block", '', jacky_split)
jacky_split<-gsub("blocknum", '', jacky_split)
jacky_split<-gsub("wordtype", '', jacky_split)
jacky_split<-gsub("trialtype", '', jacky_split)
jacky_split<-gsub("trialindex", '', jacky_split)
jacky_split<-gsub("timeelapsed", '', jacky_split)
jacky_split<-gsub("internalnodeid", '', jacky_split)
jacky_split<-gsub("rt", '', jacky_split)
jacky_split<-gsub("stimulus", '', jacky_split)
jacky_split<-gsub("keypress", '', jacky_split)
jacky_split<-gsub("dotposition", '', jacky_split)
jacky_split<-gsub("trialtype", '', jacky_split)
jacky_split<-gsub("trialindex", '', jacky_split)
jacky_split<-gsub("timeelapsed", '', jacky_split)
jacky_split<-gsub("internalnodeid", '', jacky_split)
jacky_split<-gsub("keypressed", '', jacky_split)
jacky_split<-gsub("participantresponse", '', jacky_split)
jacky_split<-gsub("correctresponse", '', jacky_split)
jacky_split<-gsub("rt", '', jacky_split)
jacky_split<-gsub("stimulus", '', jacky_split)
jacky_split<-gsub("keypress", '', jacky_split)
jacky_split<-gsub("blocktrial", '', jacky_split)
jacky_split<-gsub("trialtype", '', jacky_split)
jacky_split<-gsub("trialindex", '', jacky_split)
jacky_split<-gsub("timeelapsed", '', jacky_split)
jacky_split<-gsub("internalnodeid", '', jacky_split)


#read in subject numbers
colour_subject <- read.table("colour_subject.txt", quote="\"", comment.char="", header=T)

#remove practice runs, save to frame
jack_colour_pract<-jacky_split[,1:126]
#remove start of program columns
jack_colour_pract<-jack_colour_pract[,-c(1:6)]
#add participant names to file
jack_colour_pract<-cbind(colour_subject, jack_colour_pract)
#write practice to file
write.csv(jack_colour_pract, 'jack_colour_pract.csv')


#remove practice from main file
jacky_split<-jacky_split[,-c(1:126)]

#remove first and final 6 columns
jacky_split<-jacky_split[,-c(727:732)]
jacky_split<-jacky_split[,-c(1:6)]
jacky_split<-jacky_split[,c(1:720)]

jacky_colour<-jacky_split
#write to csv file
write.csv(jacky_split, 'jack_colour.csv')

#reread in file
jacky_colour <- read.delim("C:/Users/patty/Dropbox/R/jack_data/jacky_colour.txt")

#stack every 15th
names(jacky_colour)<-gsub("[[:punct:]]", '', names(jacky_colour))
names(jacky_colour)<-gsub("[[:digit:]]", '', names(jacky_colour))
k <- 15
nr <- nrow(jacky_colour)
nc <- ncol(jacky_colour)
unames <- unique(names(jacky_colour))


a <- array(as.matrix(jacky_colour), c(nr, k, nc/k))
m <- matrix(aperm(a, c(1, 3, 2)),, k, dimnames = list(NULL, unames))
jacky_colour_stack<-as.data.frame(m, stringsAsFactors = FALSE)

#add subject ID
colour_subject <- read.table("colour_subject.txt", quote="\"", comment.char="", header=T)
colour_subject <- colour_subject[-146,]
jacky_colour_stack2<-cbind(colour_subject, jacky_colour_stack)


#write full stacked data to file
write.csv(jacky_colour_stack, 'jacky_colour_stack.csv')

#split stacked by subject
colour_split<-split(jacky_colour_stack2, jacky_colour_stack2$SubjectID)


#write each subject ot own file
setwd("./colour_split/")

for(i in names(colour_split)){
  write.csv(colour_split[[i]], paste0(i,".csv"))
}

#setw wd back to default
setwd("C:/Users/patty/Dropbox/R/jack_data/")

#score this shit
library(plyr)
jacky_colour_means<-ddply(jacky_colour_stack, c("SubjectID", "blocknum", "wordtype"), summarise, mean=mean(as.numeric(rt)), sd=sd(as.numeric(rt)), pos_2sd=2*sd(rt)+mean(as.numeric(rt)), neg_2sd=mean(as.numeric(rt))-2*sd(as.numeric(rt)), outlier_high=length(which(as.numeric(rt) > pos_2sd)), outlier_low=length(which(as.numeric(rt) < neg_2sd)), n=length(rt))
jacky_colour_glob_means<-ddply(jacky_colour_stack, c("SubjectID"), summarise, mean=mean(as.numeric(rt)), sd=sd(as.numeric(rt)), pos_2sd=2*sd(rt)+mean(as.numeric(rt)), neg_2sd=mean(as.numeric(rt))-2*sd(as.numeric(rt)), outlier_high=length(which(as.numeric(rt) > pos_2sd)), outlier_low=length(which(as.numeric(rt) < neg_2sd)), n=length(rt))

#write to files
write.csv(jacky_colour_glob_means, "colour_global_mean.csv")
write.csv(jacky_colour_means, "colour_means.csv")
