#Colour
setwd("C:/Users/patty/Dropbox/R/jack_data/FINAL_colour_split/")
col_names <- list.files(".", pattern="*.csv", full.names=TRUE)

your_data_frame <- do.call(rbind,lapply(col_names,read.csv))
your_data_frame <- your_data_frame[,-1]

#re-read in fixed file
library(readr)
your_data_frame <- read_delim("C:/Users/patty/Dropbox/R/jack_data/col_full.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
length(unique(your_data_frame$SubjectID))
your_data_frame$rt<-as.numeric(your_data_frame$rt) 

#incorrect answers means
your_data_frame_in<- subset(your_data_frame, correctresponse=='incorrect')
jacky_colour_means_incorr<-ddply(your_data_frame_in, c("SubjectID"), summarise, incorr=length(SubjectID))
setwd("C:/Users/patty/Dropbox/R/jack_data/output")
write.csv(jacky_colour_means_incorr, 'jacky_colour_means_incorr.csv')
setwd("C:/Users/patty/Dropbox/R/jack_data")

library(plyr)
library(reshape2)
your_data_frame_cor<- subset(your_data_frame, correctresponse=='correct')

#global means, no need to fix
jacky_colour_glob_means<-ddply(your_data_frame_cor, c("SubjectID"), summarise, mean=mean(as.numeric(rt), na.rm=TRUE), sd=sd(as.numeric(rt), na.rm=TRUE), pos_2sd=2*sd(rt,na.rm=TRUE)+mean(as.numeric(rt),na.rm=TRUE), neg_2sd=mean(as.numeric(rt),na.rm=TRUE)-2*sd(as.numeric(rt),na.rm=TRUE), outlier_high=length(which(as.numeric(rt) > pos_2sd)), outlier_low=length(which(as.numeric(rt) < neg_2sd)))
setwd("C:/Users/patty/Dropbox/R/jack_data/output")
write.csv(jacky_colour_glob_means, 'jacky_colour_glob_means.csv')
setwd("C:/Users/patty/Dropbox/R/jack_data")

#means by block and word type
jacky_colour_means<-ddply(your_data_frame_cor, c("SubjectID", "blocknum", "wordtype"), summarise, mean=mean(as.numeric(rt), na.rm=TRUE), sd=sd(as.numeric(rt), na.rm=TRUE), pos_2sd=2*sd(rt,na.rm=TRUE)+mean(as.numeric(rt),na.rm=TRUE), neg_2sd=mean(as.numeric(rt),na.rm=TRUE)-2*sd(as.numeric(rt),na.rm=TRUE), outlier_high=length(which(as.numeric(rt) > pos_2sd)), outlier_low=length(which(as.numeric(rt) < neg_2sd)))
View(jacky_colour_means)
col_means<-split(jacky_colour_means, jacky_colour_means$wordtype)

col_neg<-split(col_means$negative, col_means$negative$blocknum)
col_pos<-split(col_means$positive, col_means$positive$blocknum)
col_sui<-split(col_means$suicide, col_means$suicide$blocknum)
col_neu<-split(col_means$neutral, col_means$neutral$blocknum)

cbindPad <- function(...){
args <- list(...)
n <- sapply(args,nrow)
mx <- max(n)
pad <- function(x, mx){
    if (nrow(x) < mx){
        nms <- colnames(x)
        padTemp <- matrix(NA, mx - nrow(x), ncol(x))
        colnames(padTemp) <- nms
        if (ncol(x)==0) {
          return(padTemp)
        } else {
        return(rbind(x,padTemp))
          }
    }
    else{
        return(x)
    }
}
rs <- lapply(args,pad,mx)
return(do.call(cbind,rs))
}

jacky_colour_means<-cbindPad(col_neg$num1, col_neg$num2, col_pos$num1, col_pos$num2, col_sui$num1, col_sui$num2, col_neu$num1, col_neu$num2)

setwd("C:/Users/patty/Dropbox/R/jack_data/output")
write.csv(jacky_colour_means, 'jacky_colour_means_blockword.csv')
setwd("C:/Users/patty/Dropbox/R/jack_data")


#means by word type
jacky_colour_means2<-ddply(your_data_frame_cor, c("SubjectID", "wordtype"), summarise, mean=mean(as.numeric(rt),na.rm=TRUE), sd=sd(as.numeric(rt), na.rm=TRUE), pos_2sd=2*sd(rt,na.rm=TRUE)+mean(as.numeric(rt),na.rm=TRUE), neg_2sd=mean(as.numeric(rt),na.rm=TRUE)-2*sd(as.numeric(rt),na.rm=TRUE), outlier_high=length(which(as.numeric(rt) > pos_2sd)), outlier_low=length(which(as.numeric(rt) < neg_2sd)))
colour_means2_split<-split(jacky_colour_means2, jacky_colour_means2$wordtype)
jacky_colour_means2<-cbind(colour_means2_split$negative, colour_means2_split$neutral, colour_means2_split$positive, colour_means2_split$suicide)
setwd("C:/Users/patty/Dropbox/R/jack_data/output")
write.csv(jacky_colour_means2, "jacky_colour_means_subjectword.csv")
setwd("C:/Users/patty/Dropbox/R/jack_data")


#global mean of word types
jacky_colour_means_glob_word_type<-ddply(your_data_frame, c("wordtype"), summarise, mean=mean(as.numeric(rt,na.rm=TRUE)), sd=sd(as.numeric(rt,na.rm=TRUE)), pos_2sd=2*sd(rt,na.rm=TRUE)+mean(as.numeric(rt,na.rm=TRUE)), neg_2sd=mean(as.numeric(rt,na.rm=TRUE))-2*sd(as.numeric(rt,na.rm=TRUE)), outlier_high=length(which(as.numeric(rt) > pos_2sd)), outlier_low=length(which(as.numeric(rt) < neg_2sd)))
setwd("C:/Users/patty/Dropbox/R/jack_data/output")
write.csv(jacky_colour_means_glob_word_type, "jacky_colour_means_glob_word_type.csv")
setwd("C:/Users/patty/Dropbox/R/jack_data")


######################################################
###################attention##########################
######################################################

setwd("C:/Users/patty/Dropbox/R/jack_data/")
library(plyr)
attn_names <- list.files(".", pattern="*.csv", full.names=TRUE)

your_data_frame2 <- do.call(rbind.fill,lapply(attn_names,read.csv))
your_data_frame2 <- your_data_frame2[,-1]

#read in fixed data
library(readr)
setwd("C:/Users/patty/Dropbox/R/jack_data/")
your_data_frame2 <- read_delim("full_attn.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
your_data_frame2 <- as.numeric(your_data_frame2$rt.1)

#only keep incorrects
your_data_frame2_in<- subset(your_data_frame2, correctresponse=='incorrect')
jacky_attn_means_incorr<-ddply(your_data_frame2_in, c("SubjectID"), summarise, incorr=length(SubjectID))

#remove incorrects
your_data_frame2<- subset(your_data_frame2, correctresponse=='correct')

#calculate the stats
#global mean
jacky_attn_glob_means<-ddply(your_data_frame2, c("SubjectID"), summarise, mean=mean(as.numeric(rt.1)), sd=sd(as.numeric(rt.1)), pos_2sd=2.5*sd(rt.1)+mean(as.numeric(rt.1)), neg_2sd=mean(as.numeric(rt.1))-2.5*sd(as.numeric(rt.1)), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))
jacky_attn_type_means<-ddply(your_data_frame2, c("SubjectID", "wordtype"), summarise, mean=mean(as.numeric(rt.1)), sd=sd(as.numeric(rt.1)), pos_2sd=2.5*sd(rt.1)+mean(as.numeric(rt.1)), neg_2sd=mean(as.numeric(rt.1))-2.5*sd(as.numeric(rt.1)), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))
type_split<-split(jacky_attn_type_means, jacky_attn_type_means$wordtype)
jacky_attn_type_means<-cbind(type_split$negative, type_split$neutral, type_split$positive, type_split$suicide)


#global means of valid, unvalid
valid_split<-split(your_data_frame2, your_data_frame2$validity)
invalid_means<-ddply(valid_split$validityinvalid, c("SubjectID", "wordtype"), summarise, mean=mean(rt.1, na.rm=TRUE), sd=sd(rt.1, na.rm=TRUE), pos_2sd=2.5*sd(rt.1, na.rm=TRUE)+mean(rt.1, na.rm=TRUE), neg_2sd=mean(rt.1, na.rm=TRUE)-2.5*sd(rt.1, na.rm=TRUE), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))
invalid_split<-split(invalid_means, invalid_means$wordtype)
#fuck you!!!!!!!!!!!!!!!!!!!

cbindPad <- function(...){
args <- list(...)
n <- sapply(args,nrow)
mx <- max(n)
pad <- function(x, mx){
    if (nrow(x) < mx){
        nms <- colnames(x)
        padTemp <- matrix(NA, mx - nrow(x), ncol(x))
        colnames(padTemp) <- nms
        if (ncol(x)==0) {
          return(padTemp)
        } else {
        return(rbind(x,padTemp))
          }
    }
    else{
        return(x)
    }
}
rs <- lapply(args,pad,mx)
return(do.call(cbind,rs))
}



invalid_means<-  cbindPad(invalid_split$negative, invalid_split$neutral, invalid_split$positive, invalid_split$suicide)



valid_means<-ddply(valid_split$validityvalid, c("SubjectID", "wordtype"), summarise, mean=mean(as.numeric(rt.1),na.rm=TRUE), sd=sd(as.numeric(rt.1),na.rm=TRUE), pos_2sd=2.5*sd(rt.1,na.rm=TRUE)+mean(as.numeric(rt.1),na.rm=TRUE), neg_2sd=mean(as.numeric(rt.1),na.rm=TRUE)-2.5*sd(as.numeric(rt.1),na.rm=TRUE), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))
valid_split<-split(valid_means, valid_means$wordtype)
valid_means<-cbind(valid_split$negative, valid_split$neutral, valid_split$positive, valid_split$suicide)

valid_global_mean<-ddply(valid_split$validityvalid, c("SubjectID"), summarise, mean=mean(as.numeric(rt.1),na.rm=TRUE), sd=sd(as.numeric(rt.1),na.rm=TRUE), pos_2sd=2.5*sd(rt.1,na.rm=TRUE)+mean(as.numeric(rt.1),na.rm=TRUE), neg_2sd=mean(as.numeric(rt.1),na.rm=TRUE)-2.5*sd(as.numeric(rt.1),na.rm=TRUE), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))
invalid_global_mean<-ddply(valid_split$validityinvalid, c("SubjectID"), summarise, mean=mean(as.numeric(rt.1),na.rm=TRUE), sd=sd(as.numeric(rt.1),na.rm=TRUE), pos_2sd=2.5*sd(rt.1,na.rm=TRUE)+mean(as.numeric(rt.1),na.rm=TRUE), neg_2sd=mean(as.numeric(rt.1),na.rm=TRUE)-2.5*sd(as.numeric(rt.1),na.rm=TRUE), outlier_high=length(which(as.numeric(rt.1) > pos_2sd)), outlier_low=length(which(as.numeric(rt.1) < neg_2sd)))

setwd("C:/Users/patty/Dropbox/R/jack_data/output/")
write.csv(jacky_attn_glob_means, "jacky_attn_glob_means.csv")
write.csv(jacky_attn_type_means, "jacky_attn_type_means.csv")

write.csv(invalid_means, 'attn_invalid_means.csv')

write.csv(invalid_global_mean, 'invalid_global_mean.csv')
write.csv(valid_means, "attn_valid_means.csv")
write.csv(valid_global_mean, "attn_valid_global_mean.csv")
write.csv(jacky_attn_means_incorr, 'jacky_attn_means_incorr.csv')

setwd("C:/Users/patty/Dropbox/R/jack_data/")