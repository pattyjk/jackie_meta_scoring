setwd("./jack_data/meta/")
sleep<-read.csv("FINAL MASTER_sleepsuic.csv", header=T)
library(metafor)
library(ggplot2)

#ADULT
#SuicGrp - correlational data, 1 group; divided by sleep type and ANY suicide outcome
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adult <- subset(sleep, population == "Adult" | basicsleepcon == "Insomnia")
insomnia.corr <- subset(insomnia.adult, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adult <- subset(sleep, population == "Adult" | basicsleepcon == "Hypersomnia")
hypersomnia.corr <- subset(hypersomnia.adult, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adult <- subset(sleep, population == "Adult" | basicsleepcon == "Nightmares")
nightmares.corr <- subset(nightmares.adult, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adult <- subset(sleep, population == "Adult" | basicsleepcon == "Sleep disturbance")
sleepdisturb.corr <- subset(sleepdisturb.adult, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)


#CaseCtrl - m + sd data, 2 groups; divide by sleep type  and ANY suicide outcome
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insoma<-escalc(measure="ROM", data=insomnia.adult, m1i=case.m, n1i=case.n, 
                      sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hypera<-escalc(measure="ROM", data=hypersomnia.adult, m1i=case.m, n1i=case.n, 
                      sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighthorsea<-escalc(measure="ROM", data=nightmares.adult, m1i=case.m, n1i=case.n, 
                           sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturba<-escalc(measure="ROM", data=sleepdisturb.adult, m1i=case.m, n1i=case.n, 
                        sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type and ANY suicide outcome
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.tot.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insoma2<-escalc(measure="ROM", data=insomnia.adult, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                       sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera2<-escalc(measure="ROM", data=hypersomnia.adult, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                       sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorsea2<-escalc(measure="ROM", data=nightmares.adult, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                            sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba2<-escalc(measure="ROM", data=sleepdisturb.adult, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
                         sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type and ANY suicide outcome
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insoma3<-escalc(measure="ROM", data=insomnia.adult, m1i=case.m, n1i=case.n, 
                       sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera3<-escalc(measure="ROM", data=hypersomnia.adult,m1i=case.m, n1i=case.n, 
                       sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthorsea3<-escalc(measure="ROM", data=nightmares.adult, m1i=case.m, n1i=case.n, 
                            sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba3<-escalc(measure="ROM", data=sleepdisturb.adult, m1i=case.m, n1i=case.n, 
                         sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 


############################### now divide by suicide outcome

#SuicGrp - correlational data, 1 group; divided by sleep type and SI
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adult.SI <- subset(sleep, population == "Adult" | basicsleepcon == "Insomnia" | basicSTB == "SI")
insomnia.corr <- subset(insomnia.adult.SI, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adult.SI <- subset(sleep, population == "Adult" | basicsleepcon == "Hypersomnia" | basicSTB == "SI")
hypersomnia.corr <- subset(hypersomnia.adult.SI, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adult.SI <- subset(sleep, population == "Adult" | basicsleepcon == "Nightmares" | basicSTB == "SI")
nightmares.corr <- subset(nightmares.adult.SI, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adult.SI <- subset(sleep, population == "Adult" | basicsleepcon == "Sleep disturbance" | basicSTB == "SI")
sleepdisturb.corr <- subset(sleepdisturb.adult.SI, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and SI
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insoma_SI<-escalc(measure="ROM", data=insomnia.adult.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hypera_SI<-escalc(measure="ROM", data=hypersomnia.adult.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighthora_SI<-escalc(measure="ROM", data=nightmares.adult.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturba_SI<-escalc(measure="ROM", data=sleepdisturb.adult.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type and SI
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.tot.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insoma_SI2<-escalc(measure="ROM", data=insomnia.adult.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_SI2<-escalc(measure="ROM", data=hypersomnia.adult.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthora_SI2<-escalc(measure="ROM", data=nightmares.adult.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_SI2<-escalc(measure="ROM", data=sleepdisturb.adult.SI, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type and SI
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insoma_SI3<-escalc(measure="ROM", data=insomnia.adult.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_SI3<-escalc(measure="ROM", data=hypersomnia.adult.SI,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthora_SI3<-escalc(measure="ROM", data=nightmares.adult.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_SI3<-escalc(measure="ROM", data=sleepdisturb.adult.SI, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

##################### suicide attempt #############

#SuicGrp - correlational data, 1 group; divided by sleep type and SA
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adult.SA <- subset(sleep, population == "Adult" | basicsleepcon == "Insomnia" | basicSTB == "SA")
insomnia.corr <- subset(insomnia.adult.SA, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adult.SA <- subset(sleep, population == "Adult" | basicsleepcon == "Hypersomnia" | basicSTB == "SA")
hypersomnia.corr <- subset(hypersomnia.adult.SA, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adult.SA <- subset(sleep, population == "Adult" | basicsleepcon == "Nightmares" | basicSTB == "SA")
nightmares.corr <- subset(nightmares.adult.SA, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adult.SA <- subset(sleep, population == "Adult" | basicsleepcon == "Sleep disturbance" | basicSTB == "SA")
sleepdisturb.corr <- subset(sleepdisturb.adult.SA, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and SA
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insoma_SA<-escalc(measure="ROM", data=insomnia.adult.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hypera_SA<-escalc(measure="ROM", data=hypersomnia.adult.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighthora_SA<-escalc(measure="ROM", data=nightmares.adult.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturba_SA<-escalc(measure="ROM", data=sleepdisturb.adult.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type and SA
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.tot.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insoma_SA2<-escalc(measure="ROM", data=insomnia.adult.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_SA2<-escalc(measure="ROM", data=hypersomnia.adult.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthora_SA2<-escalc(measure="ROM", data=nightmares.adult.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_SA2<-escalc(measure="ROM", data=sleepdisturb.adult.SA, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type and SA
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insoma_SA3<-escalc(measure="ROM", data=insomnia.adult.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_SA3<-escalc(measure="ROM", data=hypersomnia.adult.SA,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthora_SA3<-escalc(measure="ROM", data=nightmares.adult.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_SA3<-escalc(measure="ROM", data=sleepdisturb.adult.SA, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

##################### suicide death #############

#SuicGrp - correlational data, 1 group; divided by sleep type and SD
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adult.SD <- subset(sleep, population == "Adult" | basicsleepcon == "Insomnia" | basicSTB == "SD")
insomnia.corr <- subset(insomnia.adult.SD, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adult.SD <- subset(sleep, population == "Adult" | basicsleepcon == "Hypersomnia" | basicSTB == "SD")
hypersomnia.corr <- subset(hypersomnia.adult.SD, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adult.SD <- subset(sleep, population == "Adult" | basicsleepcon == "Nightmares" | basicSTB == "SD")
nightmares.corr <- subset(nightmares.adult.SD, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adult.SD <- subset(sleep, population == "Adult" | basicsleepcon == "Sleep disturbance" | basicSTB == "SD")
sleepdisturb.corr <- subset(sleepdisturb.adult.SD, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and SD
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insoma_sd<-escalc(measure="ROM", data=insomnia.adult.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hypera_sd<-escalc(measure="ROM", data=hypersomnia.adult.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighhor_sd<-escalc(measure="ROM", data=nightmares.adult.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturba_sd<-escalc(measure="ROM", data=sleepdisturb.adult.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.tot.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insoma_sd2<-escalc(measure="ROM", data=insomnia.adult.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_sd2<-escalc(measure="ROM", data=hypersomnia.adult.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighhor_sd2<-escalc(measure="ROM", data=nightmares.adult.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_sd2<-escalc(measure="ROM", data=sleepdisturb.adult.SD, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insoma_sd3<-escalc(measure="ROM", data=insomnia.adult.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_sd3<-escalc(measure="ROM", data=hypersomnia.adult.SD,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighhor_sd3<-escalc(measure="ROM", data=nightmares.adult.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_sd3<-escalc(measure="ROM", data=sleepdisturb.adult.SD, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 


##################### suicdality #############

#SuicGrp - correlational data, 1 group; divided by sleep type and suicdality
#Suic Corr= suicgrp.corr;
#Total N=totaln; 
########

insomnia.adult.Suic <- subset(sleep, population == "Adult" | basicsleepcon == "Insomnia" | basicSTB == "Suicidality")
insomnia.corr <- subset(insomnia.adult.Suic, suicgrp.corr != 0)
insomnia.corr <- escalc(measure = "COR", data = insomnia.corr, ri = suicgrp.corr, ni = totaln)

hypersomnia.adult.Suic <- subset(sleep, population == "Adult" | basicsleepcon == "Hypersomnia" | basicSTB == "Suicidality")
hypersomnia.corr <- subset(hypersomnia.adult.Suic, suicgrp.corr != 0)
hypersomnia.corr <- escalc(measure = "COR", data = hypersomnia.corr, ri = suicgrp.corr, ni = totaln)

nightmares.adult.Suic <- subset(sleep, population == "Adult" | basicsleepcon == "Nightmares" | basicSTB == "Suicidality")
nightmares.corr <- subset(nightmares.adult.Suic, suicgrp.corr != 0)
nightmares.corr <- escalc(measure = "COR", data = nightmares.corr, ri = suicgrp.corr, ni = totaln)

sleepdisturb.adult.suic <- subset(sleep, population == "Adult" | basicsleepcon == "Sleep disturbance" | basicSTB == "Suicidality")
sleepdisturb.corr <- subset(sleepdisturb.adult.suic, suicgrp.corr != 0)
sleepdisturb.corr <- escalc(measure = "COR", data = sleepdisturb.corr, ri = suicgrp.corr, ni = totaln)

#CaseCtrl - m + sd data, 2 groups; divide by sleep type and suicdality
#Case M= case.m; Case SD= case.sd; Case N=case.n; 
#Ctrl M=ctrl.m; Ctrl SD=ctrl.sd; Ctrl N=ctrl.n; 
#Total N=totaln; 
#########

hedges_insoma_suic<-escalc(measure="ROM", data=insomnia.adult.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_hypera_suic<-escalc(measure="ROM", data=hypersomnia.adult.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_nighthora_suic<-escalc(measure="ROM", data=nightmares.adult.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

hedges_disturba_suic<-escalc(measure="ROM", data=sleepdisturb.adult.suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=ctrl.m, n2i=ctrl.n, sd2i=ctrl.sd) 

#SuicGrpOtherGrp - m + sd data, 2 groups; divided by sleep type and suicidality 
#Suicgrp M=suicgrp.m; Suicgrp SD= suicgrp.sd; Suicgrp N=suicgrp.tot.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; Othergrp N=othergrp.n; 
#Total N=totaln; 
#######

hedges_insoma_suic2<-escalc(measure="ROM", data=insomnia.adult.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_suic2<-escalc(measure="ROM", data=hypersomnia.adult.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthora_suic2<-escalc(measure="ROM", data=nightmares.adult.Suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_suic2<-escalc(measure="ROM", data=sleepdisturb.adult.suic, m1i=suicgrp.m, n1i=suicgrp.tot.n, 
               sd1i=suicgrp.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

#CaseOthergrp - m + sd data, 2 groups; divided by sleep type and suicidality 
#Case M=case.m; Case SD= case.sd; Case N=case.n; 
#Othergrp M=othergrp.m; Othergrp SD=othergrp.sd; #Othergrp N=othergrp.n; 
#Total N=totaln; 
#############

hedges_insoma_suic3<-escalc(measure="ROM", data=insomnia.adult.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_hypera_suic3<-escalc(measure="ROM", data=hypersomnia.adult.Suic,m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_nighthora_suic3<-escalc(measure="ROM", data=nightmares.adult.Suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

hedges_disturba_suic3<-escalc(measure="ROM", data=sleepdisturb.adult.suic, m1i=case.m, n1i=case.n, 
               sd1i=case.sd, m2i=othergrp.m, n2i=othergrp.n, sd2i=othergrp.sd) 

