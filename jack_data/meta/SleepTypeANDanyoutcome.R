setwd("./jack_data/meta/")
sleep<-read.csv("FINAL MASTER_sleepsuic.csv", header=T)
library(metafor)
library(ggplot2)
sleep$population<-gsub("Adults", "Adult", sleep$population)
sleep<-subset(sleep, population == "Adult" | population == "Adolescent")

heges_grand<-escalc(measure="ROM", data=sleep, m1i=case.m, n1i=case.n, sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd)

ggplot(heges_grand, aes(population, yi))+
  geom_boxplot()+
  coord_flip()+
  ylab("Hedges G")+
  xlab("")+
  theme_bw()+
  theme(text = element_text(size=16))

#t.test for difference between effect size and variance between the populations
t.test(heges_grand$yi ~ heges_grand$population, p.adjust='FDR')
t.test(heges_grand$vi ~ heges_grand$population, p.adjust='FDR')



#splut and funnel by population
heges_split<-split(heges_grand, heges_grand$population)
funnel(heges_split$Adolescent$yi, heges_split$Adolescent$vi)
funnel(heges_split$Adult$yi, heges_split$Adult$vi)

#grand funnel
funnel(x=heges_grand$yi, vi=heges_grand$vi)


cor_grand<-escalc(measure="COR", data=sleep, ri = suicgrp.corr, ni = totaln)
funnel(cor_grand$yi, cor_grand$vi)

ggplot(cor_grand, aes(population, yi))+
  geom_boxplot()+
  coord_flip()+
  ylab("Cor")+
  xlab("")+
  theme_bw()

#t.test for difference between effect size and variance between the populations
t.test(cor_grand$yi ~ heges_grand$population, p.adjust='FDR')
t.test(cor_grand$vi ~ heges_grand$population, p.adjust='FDR')

length(which(is.na(cor_grand$yi) == TRUE))
length(which(is.na(heges_grand$yi) == TRUE))


#remove non target sleep conditions
setwd("./jack_data/meta/")
sleep<-read.csv("FINAL MASTER_sleepsuic.csv", header=T)
sleep$population<-gsub("Adults", "Adult", sleep$population)

library(dplyr)
sleep.m<-sleep %>% select("ctrl.n","ctrl.sd","ctrl.m", "population", "basicsleepcon", "esid", "metaid", "basicSTB")

sleep.m.case <- sleep %>% select("case.n","case.sd","case.m", "population", "basicsleepcon", "esid", "metaid", "basicSTB")
sleep.m.case$type<-rep("case", nrow(sleep.m.case))
names(sleep.m.case)<-c("treat.n", "treat.sd", "treat.m", "population", "basicsleepcon", "esid", "metaid", "basicSTB", "type")

sleep.m.suic<- sleep %>% select("suicgrp.tot.n","suicgrp.sd","suicgrp.m", "population", "basicsleepcon", "esid", "metaid", "basicSTB")
sleep.m.suic$type<-rep("suic", nrow(sleep.m))
names(sleep.m.suic)<-c("treat.n", "treat.sd", "treat.m", "population", "basicsleepcon", "esid", "metaid", "basicSTB", "type")

sleep.m.other<- sleep %>% select("othergrp.n","othergrp.sd","othergrp.m", "population", "basicsleepcon", "esid", "metaid", "basicSTB")
sleep.m.other$type<-rep("othergrp", nrow(sleep.m))
names(sleep.m.other)<-c("treat.n", "treat.sd", "treat.m", "population", "basicsleepcon", "esid", "metaid", "basicSTB", "type")

#look at correlations
sleep.m.suic.cor<- sleep %>% select("suicgrp.tot.n","suicgrp.sd","suicgrp.m", "suicgrp.corr", "population", "basicsleepcon", "esid", "metaid", "basicSTB")
sleep.m.suic.cor$type<-rep("suic", nrow(sleep.m))
names(sleep.m.suic.cor)<-c("treat.n", "treat.sd", "treat.m", "treat.corr", "population", "basicsleepcon", "esid", "metaid", "basicSTB", "type")

sleep.m.cor<-sleep %>% select("ctrl.n","ctrl.sd","ctrl.m", "ctrl.corr", "population", "basicsleepcon", "esid", "metaid", "basicSTB")

sleep.cor<-merge(sleep.m.cor, sleep.m.suic.cor, by="esid")
cor.ES<-escalc(measure = "COR", sleep.cor$ctrl.corr, sleep.cor$treat.corr)




sleep.treat<-rbind(sleep.m.case, sleep.m.suic, sleep.m.other)
dim(sleep.treat)
#2040 by 9

length(unique(sleep2$metaid))

sleep2<-merge(sleep.m, sleep.m.case)

sleep1<-merge(sleep.m, sleep.treat, by="esid")
dim(sleep1)
#2040 by 16


library(metafor)
library(ggplot2)
test2<-escalc(measure="ROM", data=sleep2, m1i=treat.m, n1i=as.numeric(treat.n), sd1i=treat.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd, append=T)

test<-escalc(measure="ROM", data=sleep1, m1i=treat.m, n1i=as.numeric(treat.n), sd1i=treat.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd, append=T)
test$basicsleepcon.x<-gsub("Sleep quality", "Sleep disturbance",  test$basicsleepcon.x)
test$basicsleepcon.x<-gsub("sleep quality", "Sleep disturbance",  test$basicsleepcon.x)
test$basicsleepcon.x<-gsub("Nonrestorative sleep", "Sleep disturbance",  test$basicsleepcon.x)
test$basicsleepcon.x<-gsub("Non-restorative Sleep", "Sleep disturbance",  test$basicsleepcon.x)
test$basicsleepcon.x<-gsub("Trouble waking", "Sleep disturbance",  test$basicsleepcon.x)
test$basicsleepcon.x<-gsub("Sleep disorder", "Sleep disturbance",  test$basicsleepcon.x)
test$basicsleepcon.x<-gsub("Nocturnal leg jerks", "Sleep disturbance",  test$basicsleepcon.x)
test$basicsleepcon.x<-gsub("Insomnia + Nightmares", "Sleep disturbance",  test$basicsleepcon.x)
test$basicsleepcon.x<-gsub("Circadian reversal", "Sleep disturbance",  test$basicsleepcon.x)
test$basicsleepcon.x<-gsub("Lack of deep sleep", "Sleep disturbance",  test$basicsleepcon.x)

test$population.x<-gsub("Adults", "Adult", test$population.x)


test2<-subset(test2, basicsleepcon == "Insomnia" | basicsleepcon == "Hypersomnia" | basicsleepcon == "Nightmares" | basicsleepcon == "Sleep disturbance")
test<-subset(test, basicsleepcon.x == "Insomnia" | basicsleepcon.x == "Hypersomnia" | basicsleepcon.x == "Nightmares" | basicsleepcon.x == "Sleep disturbance")
write.csv(test2, 'data.csv', row.names=F, quote=F)
write.csv(test, 'dat2a.csv', row.names=F, quote=F)

length(unique(test2$metaid))

test_ado<-subset(test, population.x == "Adolescent")
test_adul<-subset(test, population.x == "Adult")

###Rank correlation test
ranktest(test2$yi, test2$vi)
#Kendall's tau=0.156, p=0.066

ranktest(test_ado$yi, test_ado$vi)
ranktest(test_adul$yi, test_adul$vi)

funnel()

t.test(test$yi ~ test$population.x, p.adjust="FDR")
t.test(test$vi ~ test$population.x, p.adjust="FDR")

#fail safe test
fsn(test2$yi, test2$vi)
#Observed Significance Level: <.0001 
#Target Significance Level:   0.05 
#Fail-safe N: 8452 

fsn(test_ado$yi, test_ado$vi)

fsn(test_adul$yi, test_adul$vi)


funnel(test2$yi, test2$vi)
funnel(test_ado$yi, test_ado$vi)
funnel(test_adul$yi, test_adul$vi)

library(ggplot2)
ggplot(test, aes(population.x, yi))+
  geom_boxplot()+
  coord_flip()+
  ylab("Hedges G")+
  xlab("")+
  theme_bw()+
  theme(text = element_text(size=16))


#total models
rma.mv(yi, vi, data=test2, mods=~population)
rma.mv(yi, vi, data=test_adul)
rma.mv(yi, vi, data=test_ado)

rma.mv(yi, vi, data=test_adul, mods=cbind(basicSTB.y, basicsleepcon.y))
rma.mv(yi, vi, data=test2, random = ~population, mods= ~ population)

test_adul$basicsleepcon.y

#everything
rma.mv(yi, vi, data=hsleep1, mod= ~ population)


heges.sleep1<-subset(heges.sleep1, basicsleepcon.x == "Insomnia" | basicsleepcon.x == "Hypersomnia" | basicsleepcon.x == "Nightmares" | basicsleepcon.x == "Sleep disturbance")

heges.sleep1_split <- split(heges.sleep1, heges.sleep1$population.x)
adul<-heges.sleep1_split$Adult
ado<-heges.sleep1_split$Adolescent


rma.mv(yi, vi, data=heges.sleep1, random=~population.x)


#calculate model
model1<-
rma.mv(yi, vi, data=adul, mods= ~ basicSTB.x * basicsleepcon.x)
rma(yi, vi, data=adul, mods= ~ basicSTB.x * basicsleepcon.x)

rma(yi, vi, data=heges.sleep1, mods= ~ basicsleepcon.x * basicSTB.x)

rma.mv(yi, vi, data=adul, mods = ~basicSTB.x + ~ basicsleepcon.x)
       
       
       

rma.mv(yi, vi, data=adul, random= list(basicSTB.x, basicsleepcon.x), mods= ~ basicsleepcon.x)



rma.mv(yi, vi, data=adul, random= ~basicsleepcon.x)

rma.mv(yi, vi, data=adul, random= ~ basicsleepcon.x | basicSTB.x, mods= ~ basicsleepcon.x)


model<-rma.mv(yi, vi, data=adul, mods= ~ basicsleepcon.x | basicSTB.x)

model_a<-rma.mv(yi, vi, data=ado, mods= ~basicsleepcon.x)


model_a<-rma.mv(yi, vi, data=ado, random = ~basicsleepcon.x)







sleep$population<-gsub("Adults", "Adult", sleep$population)
sleep<-subset(sleep, population == "Adult" | population == "Adolescent")
sleep<-sleep[!is.na(sleep$ctrl.sd),]
nrow(sleep)
#161

heges_grand<-escalc(measure="ROM", data=sleep, m1i=case.m, n1i=case.n, sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd)
heges_grand<-subset(heges_grand, basicsleepcon == "Insomnia" | basicsleepcon == "Hypersomnia" | basicsleepcon == "Nightmares" | basicsleepcon == "Sleep disturbance")
nrow(heges_grand)
#20

#split data frame
heges_split<-split(heges_grand,heges_grand$population)
ado<-heges_split$Adolescent
nrow(ado)
#0
adul<-heges_split$Adult
nrow(adul)
#

#calculate model
model1<-rma.mv(yi, vi, data=adul, random= ~ basicsleepcon)
model1

model<-rma.mv(yi, vi, data=adul, mods= ~ basicsleepcon)
model


model_a<-rma.mv(yi, vi, data=ado, mods= ~basicsleepcon)
model_a

model_a<-rma.mv(yi, vi, data=ado, random = ~basicsleepcon)
model_a

#SuicGrp - correlational data, 1 group; divided by sleep type and ANY suicide outcome.
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia <- subset(sleep, basicsleepcon == "Insomnia")
insomnia.corr <- subset(insomnia, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

#you don't have any values in the ..corr or n columns to calcualte it sucka
hypersomnia <- subset(sleep, basicsleepcon == "Hypersomnia")
hypersomnia.corr <- subset(hypersomnia, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares <- subset(sleep, basicsleepcon == "Nightmares")
nightmares.corr <- subset(nightmares, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb <- subset(sleep, basicsleepcon == "Sleep disturbance")
sleepdisturb.corr <- subset(sleepdisturb, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and ANY suicide outcome.
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insom_tot<-escalc(measure="ROM", data=insomnia, m1i=case.m, n1i=case.n, 
                         sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hyper_tot<-escalc(measure="ROM", data=hypersomnia, m1i=case.m, n1i=case.n, 
                         sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighthorse_tot<-escalc(measure="ROM", data=nightmares, m1i=case.m, n1i=case.n, 
                              sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturb_tot<-escalc(measure="ROM", data=sleepdisturb, m1i=case.m, n1i=case.n, 
                           sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.tot.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insom_tot2<-escalc(measure="ROM", data=insomnia, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                          sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper_tot2<-escalc(measure="ROM", data=hypersomnia, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                          sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorse_tot2<-escalc(measure="ROM", data=nightmares, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturb_tot2<-escalc(measure="ROM", data=sleepdisturb, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                            sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insom_tot3<-escalc(measure="ROM", data=insomnia, m1i=case.m, n1i=case.n, 
                          sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper_tot3<-escalc(measure="ROM", data=hypersomnia, m1i=case.m, n1i=case.n, 
                          sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorse_tot3<-escalc(measure="ROM", data=nightmares, m1i=case.m, n1i=case.n, 
                               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturb_tot3<-escalc(measure="ROM", data=sleepdisturb, m1i=case.m, n1i=case.n, 
                            sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 



#####################################
#BY OUTCOME
#####################################




############################### now divide by suicide outcome #####################################

#SuicGrp - correlational data, 1 group; divided by sleep type and SI
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.SI <- subset(sleep, basicsleepcon == "Insomnia" | basicSTB == "SI")
insomnia.corr <- subset(insomnia.SI, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.SI <- subset(sleep, basicsleepcon == "Hypersomnia" | basicSTB == "SI")
hypersomnia.corr <- subset(hypersomnia.SI, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.SI <- subset(sleep, basicsleepcon == "Nightmares" | basicSTB == "SI")
nightmares.corr <- subset(nightmares.SI, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.SI <- subset(sleep, basicsleepcon == "Sleep disturbance" | basicSTB == "SI")
sleepdisturb.corr <- subset(sleepdisturb.SI, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)



#CaseCtrl - m + sd data, 2 groups; divide by sleep type and SI
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insom_tot_SI<-escalc(measure="ROM", data=insomnia.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hypera_SI<-escalc(measure="ROM", data=hypersomnia.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighta_SI<-escalc(measure="ROM", data=nightmares.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturba_SI<-escalc(measure="ROM", data=sleepdisturb.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type and SI
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.tot.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insom_tot_SI2<-escalc(measure="ROM", data=insomnia.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_SI2<-escalc(measure="ROM", data=hypersomnia.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighta_SI2<-escalc(measure="ROM", data=nightmares.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_SI2<-escalc(measure="ROM", data=sleepdisturb.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type and SI
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insom_tot_SI3<-escalc(measure="ROM", data=insomnia.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_SI4<-escalc(measure="ROM", data=hypersomnia.SI,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighta_SI3<-escalc(measure="ROM", data=nightmares.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_SI3<-escalc(measure="ROM", data=sleepdisturb.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

##################### suicide attempt #############

#SuicGrp - correlational data, 1 group; divided by sleep type and SA
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.SA <- subset(sleep, basicsleepcon == "Insomnia" | basicSTB == "SA")
insomnia.corr <- subset(insomnia.SA, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.SA <- subset(sleep, basicsleepcon == "Hypersomnia" | basicSTB == "SA")
hypersomnia.corr <- subset(hypersomnia.SA, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.SA <- subset(sleep, basicsleepcon == "Nightmares" | basicSTB == "SA")
nightmares.corr <- subset(nightmares.SA, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.SA <- subset(sleep, basicsleepcon == "Sleep disturbance" | basicSTB == "SA")
sleepdisturb.corr <- subset(sleepdisturb.SA, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and SA
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insoma_SA<-escalc(measure="ROM", data=insomnia.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hypera_SA<-escalc(measure="ROM", data=hypersomnia.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighta_SA<-escalc(measure="ROM", data=nightmares.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturba_SA<-escalc(measure="ROM", data=sleepdisturb.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#####################

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.tot.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insoma_SA2<-escalc(measure="ROM", data=insomnia.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_SA2<-escalc(measure="ROM", data=hypersomnia.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighta_SA2<-escalc(measure="ROM", data=nightmares.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_SA2<-escalc(measure="ROM", data=sleepdisturb.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insoma_SA3<-escalc(measure="ROM", data=insomnia.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_SA3<-escalc(measure="ROM", data=hypersomnia.SA,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighta_SA3<-escalc(measure="ROM", data=nightmares.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_SA3<-escalc(measure="ROM", data=sleepdisturb.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

##################### suicide death #############

#SuicGrp - correlational data, 1 group; divided by sleep type and SD
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.SD <- subset(sleep, basicsleepcon == "Insomnia" | basicSTB == "SD")
insomnia.corr <- subset(insomnia.SD, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

#can't run this one because there's no values
hypersomnia.SD <- subset(sleep, basicsleepcon == "Hypersomnia" | basicSTB == "SD")
hypersomnia.corr <- subset(hypersomnia.SD, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.SD <- subset(sleep, basicsleepcon == "Nightmares" | basicSTB == "SD")
nightmares.corr <- subset(nightmares.SD, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.SD <- subset(sleep, basicsleepcon == "Sleep disturbance" | basicSTB == "SD")
sleepdisturb.corr <- subset(sleepdisturb.SD, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and SD
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insoma_SD<-escalc(measure="ROM", data=insomnia.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hypera_SD<-escalc(measure="ROM", data=hypersomnia.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighta_SD<-escalc(measure="ROM", data=nightmares.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_distuba_SD<-escalc(measure="ROM", data=sleepdisturb.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type and SD
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.tot.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insoma_SD2<-escalc(measure="ROM", data=insomnia.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_SD2<-escalc(measure="ROM", data=hypersomnia.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighta_SD2<-escalc(measure="ROM", data=nightmares.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_distuba_SD2<-escalc(measure="ROM", data=sleepdisturb.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type and SD
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insoma_SD3<-escalc(measure="ROM", data=insomnia.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_SD3<-escalc(measure="ROM", data=hypersomnia.SD,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighta_SD3<-escalc(measure="ROM", data=nightmares.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_distuba_SD3<-escalc(measure="ROM", data=sleepdisturb.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 


##################### suicdality #############

#SuicGrp - correlational data, 1 group; divided by sleep type and suicdality
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.Suic <- subset(sleep, basicsleepcon == "Insomnia" | basicSTB == "Suicidality")
insomnia.corr <- subset(insomnia.Suic, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.Suic <- subset(sleep, basicsleepcon == "Hypersomnia" | basicSTB == "Suicidality")
hypersomnia.corr <- subset(hypersomnia.Suic, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.Suic <- subset(sleep, basicsleepcon == "Nightmares" | basicSTB == "Suicidality")
nightmares.corr <- subset(nightmares.Suic, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.Suic <- subset(sleep, basicsleepcon == "Sleep disturbance" | basicSTB == "Suicidality")
sleepdisturb.corr <- subset(sleepdisturb.Suic, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and suicdality
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insoma_suic<-escalc(measure="ROM", data=insomnia.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hypera_suic<-escalc(measure="ROM", data=hypersomnia.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighta_suic<-escalc(measure="ROM", data=nightmares.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturba_suic<-escalc(measure="ROM", data=sleepdisturb.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type and suicidality 
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.tot.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insoma_suic2<-escalc(measure="ROM", data=insomnia.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_suic2<-escalc(measure="ROM", data=hypersomnia.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighta_suic2<-escalc(measure="ROM", data=nightmares.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_suic2<-escalc(measure="ROM", data=sleepdisturb.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type and suicidality 
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insoma_suic3<-escalc(measure="ROM", data=insomnia.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_suic3<-escalc(measure="ROM", data=hypersomnia.Suic,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighta_suic3<-escalc(measure="ROM", data=nightmares.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_suic3<-escalc(measure="ROM", data=sleepdisturb.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 




