setwd("./jack_data/meta")
library(readr)
library(ggplot2)
library(metafor)
sleep <- read_csv("FINAL MASTER_sleepsuic.csv")

# make subsets of your data that you're grouping together, either because they're the same metric (e.g., correlation)
# or outcome (insomnia); david's code was the first subset of insomnia corr data.

########################################################


#SuicGrp - correlational data, 1 group; divided by sleep type
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia <- subset(sleep, basicsleepcon == "Insomnia")
insomnia.corr <- subset(insomnia, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia <- subset(sleep, basicsleepcon == "Hypersomnia")
hypersomnia.corr <- subset(hypersomnia, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares <- subset(sleep, basicsleepcon == "Nightmares")
nightmares.corr <- subset(nightmares, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb <- subset(sleep, basicsleepcon == "Sleep disturbance")
sleepdisturb.corr <- subset(sleepdisturb, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)


#CaseCtrl - m + sd data, 2 groups; divide by sleep type
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

#SuicGrpCaseOtherGrp - m + sd data, 3 groups; divided by sleep type
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.n; 
#Case M=case.m; Case SD= case.sd; Case N=case.n;
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#########

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

#CaseCtrlOthergrp - m + sd data, 3 groups; divided by sleep type
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.n; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

#CaseCtrlOthergrpOthergrp2 - m + sd data, 4 groups; divided by sleep type
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.n; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n;
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Othergrp2 M=othergrp2.m; Othergrp2 SD=othergrp2.sd; #Othergrp N=othergrp2.n; 
#Total N=totaln; 
############


####################################
####################################
####################################
####################################
#Above analyses to be run in 2 populations - adolescent and adult
#population=Adult
#population=Adolescent
####################################
####################################
####################################
####################################

#ADOLESCENT
#SuicGrp - correlational data, 1 group; divided by sleep type
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adol <- subset(sleep, population == "Adolescent", basicsleepcon == "Insomnia")
insomnia.corr <- subset(insomnia.adol, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adol <- subset(sleep, population == "Adolescent", basicsleepcon == "Hypersomnia")
hypersomnia.corr <- subset(hypersomnia.adol, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adol <- subset(sleep, population == "Adolescent", basicsleepcon == "Nightmares")
nightmares.corr <- subset(nightmares.adol, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adol <- subset(sleep, population == "Adolescent", basicsleepcon == "Sleep disturbance")
sleepdisturb.corr <- subset(sleepdisturb.adol, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)




####################################
####################################

#ADULT
#SuicGrp - correlational data, 1 group; divided by sleep type
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adult <- subset(sleep, population == "Adult", basicsleepcon == "Insomnia")
insomnia.corr <- subset(insomnia.adult, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adult <- subset(sleep, population == "Adult", basicsleepcon == "Hypersomnia")
hypersomnia.corr <- subset(hypersomnia.adult, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adult <- subset(sleep, population == "Adult", basicsleepcon == "Nightmares")
nightmares.corr <- subset(nightmares.adult, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adult <- subset(sleep, population == "Adult", basicsleepcon == "Sleep disturbance")
sleepdisturb.corr <- subset(sleepdisturb.adult, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)


####################################
####################################





# multi-level model; david says use this. 
mod.ml <- rma.mv(yi, vi, random = ~ 1 | metaid/esid, data = insomnia.corr)
summary(mod.ml)
mod.clusterrobust <- robust(mod.ml, cluster = insomnia.corr$metaid)
summary(mod.clusterrobust)


# non-multi-level model
mod <- rma(yi, vi, data = insomnia.corr, method = "REML")
summary(mod)
mod.clusterrobust <- robust(mod, cluster = insomnia.corr$metaid)
summary(mod.clusterrobust)

# multi-level model; david says use this. 
mod.ml <- rma.mv(yi, vi, random = ~ 1 | metaid/esid, data = insomnia.corr)
summary(mod.ml)
mod.clusterrobust <- robust(mod.ml, cluster = insomnia.corr$metaid)
summary(mod.clusterrobust)


#ignore below 
LRR<-escalc(measure="ROM", data=sleep, m1i=sleep$case.m, n1i=sleep$case.n, sd1i=sleep$case.sd, m2i=sleep$ctrl.m, n2i=sleep$ctrl.n, sd2i=sleep$ctrl.sd,
            var.names = c("corr.es.yi", "corr.var.vi"))


#calculate log response ratio
LRR<-escalc(measure="ROM", data=sleep, m1i=sleep$case.m, n1i=sleep$case.n, sd1i=sleep$case.sd, m2i=sleep$ctrl.m, n2i=sleep$ctrl.n, sd2i=sleep$ctrl.sd)
class(LRR)

# #merge to original frame
# sleep_2<-cbind(sleep, as.data.frame(LRR))
# funnel(sleep_2$yi, sleep_2$vi)

#calculated hedges g
hedges<-escalc(measure="ROM", data=sleep, m1i=sleep$case.m, n1i=sleep$case.n, sd1i=sleep$case.sd, m2i=sleep$ctrl.m, n2i=sleep$ctrl.n, sd2i=sleep$ctrl.sd)
class(hedges)

#bind data to original frame
sleep3<-cbind(sleep, as.data.frame(hedges))

#funnel plot
funnel(sleep3$yi, sleep3$vi)
funnel(sleep_2$yi, sleep_2$vi)

#forest plot
forest(sleep3$yi, sleep3$vi, slab=sleep3$metaid, pch=19, subset=order(sleep3$yi))
forest(sleep_2$yi, sleep_2$vi, slab=sleep_2$metaid, pch=19, subset=order(sleep_2$yi))

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