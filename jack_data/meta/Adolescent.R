setwd("./jack_data/meta/")
sleep<-read.csv("FINAL MASTER_sleepsuic.csv", header=T)
library(metafor)
library(ggplot2)

#ADOLESCENT
#SuicGrp - correlational data, 1 group; divided by sleep type and ANY suicide outcome
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adol <- subset(sleep, population == "Adolescent" | basicsleepcon == "Insomnia")
insomnia.corr <- subset(insomnia.adol, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adol <- subset(sleep, population == "Adolescent" | basicsleepcon == "Hypersomnia")
hypersomnia.corr <- subset(hypersomnia.adol, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adol <- subset(sleep, population == "Adolescent" | basicsleepcon == "Nightmares")
nightmares.corr <- subset(nightmares.adol, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adol <- subset(sleep, population == "Adolescent" | basicsleepcon == "Sleep disturbance")
sleepdisturb.corr <- subset(sleepdisturb.adol, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)



#CaseCtrl - m + sd data, 2 groups; divide by sleep type
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insom<-escalc(measure="ROM", data=insomnia.adol, m1i=case.m, n1i=case.n, 
                     sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hyper<-escalc(measure="ROM", data=hypersomnia.adol, m1i=case.m, n1i=case.n, 
                     sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighthorse<-escalc(measure="ROM", data=nightmares.adol, m1i=case.m, n1i=case.n, 
                          sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturb<-escalc(measure="ROM", data=sleepdisturb.adol, m1i=case.m, n1i=case.n, 
                       sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insom2<-escalc(measure="ROM", data=insomnia.adol, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                      sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper2<-escalc(measure="ROM", data=hypersomnia.adol, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                      sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorse2<-escalc(measure="ROM", data=nightmares.adol, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                           sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturb2<-escalc(measure="ROM", data=sleepdisturb.adol, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                        sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insom3<-escalc(measure="ROM", data=insomnia.adol, m1i=case.m, n1i=case.n, 
                      sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper3<-escalc(measure="ROM", data=hypersomnia.adol,m1i=case.m, n1i=case.n, 
                      sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorse3<-escalc(measure="ROM", data=nightmares.adol, m1i=case.m, n1i=case.n, 
                           sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturb3<-escalc(measure="ROM", data=sleepdisturb.adol, m1i=case.m, n1i=case.n, 
                        sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 


############################### now divide by suicide outcome

#SuicGrp - correlational data, 1 group; divided by sleep type and SI
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adol.SI <- subset(sleep, population == "Adolescent" | basicsleepcon == "Insomnia" | basicSTB == "SI")
insomnia.corr <- subset(insomnia.adol, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adol.SI <- subset(sleep, population == "Adolescent" | basicsleepcon == "Hypersomnia" | basicSTB == "SI")
hypersomnia.corr <- subset(hypersomnia.adol.SI, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adol.SI <- subset(sleep, population == "Adolescent" | basicsleepcon == "Nightmares" | basicSTB == "SI")
nightmares.corr <- subset(nightmares.adol.SI, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adol.SI <- subset(sleep, population == "Adolescent" | basicsleepcon == "Sleep disturbance" | basicSTB == "SI")
sleepdisturb.corr <- subset(sleepdisturb.adol.SI, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)



#CaseCtrl - m + sd data, 2 groups; divide by sleep type and SI
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insom_SI<-escalc(measure="ROM", data=insomnia.adol.SI, m1i=case.m, n1i=case.n, 
                        sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hyper_SI<-escalc(measure="ROM", data=hypersomnia.adol.SI, m1i=case.m, n1i=case.n, 
                        sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighthorse_SI<-escalc(measure="ROM", data=nightmares.adol.SI, m1i=case.m, n1i=case.n, 
                             sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturb_SI<-escalc(measure="ROM", data=sleepdisturb.adol.SI, m1i=case.m, n1i=case.n, 
                          sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type and SI
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insom_SI2<-escalc(measure="ROM", data=insomnia.adol.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper_SI2<-escalc(measure="ROM", data=hypersomnia.adol.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorse_SI2<-escalc(measure="ROM", data=nightmares.adol.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturb_SI2<-escalc(measure="ROM", data=sleepdisturb.adol.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type and SI
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insom_SI3<-escalc(measure="ROM", data=insomnia.adol.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper_SI3<-escalc(measure="ROM", data=hypersomnia.adol.SI,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorse_SI3<-escalc(measure="ROM", data=nightmares.adol.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturb_SI3<-escalc(measure="ROM", data=sleepdisturb.adol.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

##################### suicide attempt #############

#SuicGrp - correlational data, 1 group; divided by sleep type and SA
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adol.SA <- subset(sleep, population == "Adolescent" | basicsleepcon == "Insomnia" | basicSTB == "SA")
insomnia.corr <- subset(insomnia.adol.SA, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adol.SA <- subset(sleep, population == "Adolescent" | basicsleepcon == "Hypersomnia" | basicSTB == "SA")
hypersomnia.corr <- subset(hypersomnia.adol.SA, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adol.SA <- subset(sleep, population == "Adolescent" | basicsleepcon == "Nightmares" | basicSTB == "SA")
nightmares.corr <- subset(nightmares.adol.SA, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adol.SA <- subset(sleep, population == "Adolescent" | basicsleepcon == "Sleep disturbance" | basicSTB == "SA")
sleepdisturb.corr <- subset(sleepdisturb.adol.SA, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and SA
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insom_SA<-escalc(measure="ROM", data=insomnia.adol.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hyper_SA<-escalc(measure="ROM", data=hypersomnia.adol.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighthorse_SA<-escalc(measure="ROM", data=nightmares.adol.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturb_SA<-escalc(measure="ROM", data=sleepdisturb.adol.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#####################

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insom_SA2<-escalc(measure="ROM", data=insomnia.adol.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper_SA2<-escalc(measure="ROM", data=hypersomnia.adol.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorse_SA2<-escalc(measure="ROM", data=nightmares.adol.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedgesdisturb_SA2<-escalc(measure="ROM", data=sleepdisturb.adol.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insom_SA3<-escalc(measure="ROM", data=insomnia.adol.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper_SA3<-escalc(measure="ROM", data=hypersomnia.adol.SA,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorse_SA3<-escalc(measure="ROM", data=nightmares.adol.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturb_SA3<-escalc(measure="ROM", data=sleepdisturb.adol.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

##################### suicide death #############

#SuicGrp - correlational data, 1 group; divided by sleep type and SD
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adol.SD <- subset(sleep, population == "Adolescent" | basicsleepcon == "Insomnia" | basicSTB == "SD")
insomnia.corr <- subset(insomnia.adol.SD, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adol.SD <- subset(sleep, population == "Adolescent" | basicsleepcon == "Hypersomnia" | basicSTB == "SD")
hypersomnia.corr <- subset(hypersomnia.adol.SD, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adol.SD <- subset(sleep, population == "Adolescent" | basicsleepcon == "Nightmares" | basicSTB == "SD")
nightmares.corr <- subset(nightmares.adol.SD, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adol.SD <- subset(sleep, population == "Adolescent" | basicsleepcon == "Sleep disturbance" | basicSTB == "SD")
sleepdisturb.corr <- subset(sleepdisturb.adol.SD, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and SD
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insom_SD<-escalc(measure="ROM", data=insomnia.adol.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hyper_SD<-escalc(measure="ROM", data=hypersomnia.adol.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighthorse_SD<-escalc(measure="ROM", data=nightmares.adol.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_distrub_SD<-escalc(measure="ROM", data=sleepdisturb.adol.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type and SD
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insom_SD2<-escalc(measure="ROM", data=insomnia.adol.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper_SD2<-escalc(measure="ROM", data=hypersomnia.adol.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorse_SD2<-escalc(measure="ROM", data=nightmares.adol.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_distrub_SD2<-escalc(measure="ROM", data=sleepdisturb.adol.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type and SD
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insom_SD3<-escalc(measure="ROM", data=insomnia.adol.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper_SD3<-escalc(measure="ROM", data=hypersomnia.adol.SD,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorse_SD3<-escalc(measure="ROM", data=nightmares.adol.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_distrub_SD3<-escalc(measure="ROM", data=sleepdisturb.adol.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 


##################### suicdality #############

#SuicGrp - correlational data, 1 group; divided by sleep type and suicdality
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adol.Suic <- subset(sleep, population == "Adolescent" | basicsleepcon == "Insomnia" | basicSTB == "Suicidality")
insomnia.corr <- subset(insomnia.adol.Suic, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adol.Suic <- subset(sleep, population == "Adolescent" | basicsleepcon == "Hypersomnia" | basicSTB == "Suicidality")
hypersomnia.corr <- subset(hypersomnia.adol.Suic, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adol.Suic <- subset(sleep, population == "Adolescent" | basicsleepcon == "Nightmares" | basicSTB == "Suicidality")
nightmares.corr <- subset(nightmares.adol.Suic, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adol.Suic <- subset(sleep, population == "Adolescent" | basicsleepcon == "Sleep disturbance" | basicSTB == "Suicidality")
sleepdisturb.corr <- subset(sleepdisturb.adol.Suic, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and suicdality
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insom_suic<-escalc(measure="ROM", data=insomnia.adol.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hyper_suic<-escalc(measure="ROM", data=hypersomnia.adol.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighthor_suic<-escalc(measure="ROM", data=nightmares.adol.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturb_suic<-escalc(measure="ROM", data=sleepdisturb.adol.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type and suicidality 
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insom_suic2<-escalc(measure="ROM", data=insomnia.adol.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper_suic2<-escalc(measure="ROM", data=hypersomnia.adol.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthor_suic2<-escalc(measure="ROM", data=nightmares.adol.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturb_suic2<-escalc(measure="ROM", data=sleepdisturb.adol.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type and suicidality 
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insom_suic3<-escalc(measure="ROM", data=insomnia.adol.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hyper_suic3<-escalc(measure="ROM", data=hypersomnia.adol.Suic,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthor_suic3<-escalc(measure="ROM", data=nightmares.adol.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturb_suic3<-escalc(measure="ROM", data=sleepdisturb.adol.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

