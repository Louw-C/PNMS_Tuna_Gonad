library(readxl)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(car)


#Gonad data
#Load data
PNMS_Gonad<-read.csv(file.choose(),header=T,sep=",")
names(PNMS_Gonad)


#summary statistics
# run the script called "SummarySE()_functon.R" then run the code below to get your mean + standard errors
library(plyr)
library(Rmisc)
#Fork length
data_sum_Length <- summarySE(PNMS_Gonad, measurevar="FL_cm", groupvars=c("Sex","Species"))
data_sum_Length 
#Weight
data_sum_Weight<- summarySE(PNMS_Gonad, measurevar="Total_weight_kg", groupvars=c("Sex","Species"))
data_sum_Weight


#Make basic figures

require(Rmisc)
require(ggplot2)
#Tuna length
Gonad_Length<-summarySE(PNMS_Gonad, measurevar="FL_cm", groupvars=c("Sex","Species"))
Gonad1<-ggplot(Gonad_Length, aes(x=factor(Species), y=FL_cm,fill=factor(Sex)))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Tuna fork length in cm")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  geom_errorbar(aes(ymin=FL_cm-se, ymax=FL_cm+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

Gonad1

#Tuna weight
Gonad_weight<-summarySE(PNMS_Gonad, measurevar="Total_weight_kg", groupvars=c("Sex","Species"))
Gonad2<-ggplot(Gonad_weight, aes(x=factor(Species), y=Total_weight_kg,fill=factor(Sex)))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Tuna weight in kg")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  geom_errorbar(aes(ymin=Total_weight_kg-se, ymax=Total_weight_kg+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

Gonad2

#Average GSI across time/species
Gonad_GSI<-summarySE(PNMS_Gonad, measurevar="GSI", groupvars=c("Month", "Species"))
Gonad3<-ggplot(Gonad_GSI, aes(x=factor(Month), y=GSI,fill=factor(Species)))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Tuna weight in kg")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  geom_errorbar(aes(ymin=GSI-se, ymax=GSI+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

Gonad3
Gonad_GSI$Month <- factor(Gonad_GSI$Month, levels = c( "September","October", "November","December","January","February", "March"))


#Plot fish weight and gonad weight (g)
plot(PNMS_Gonad$Gonad_weight_g~PNMS_Gonad$Total_weight_g)


#Separate YFT and BET

YFT<-subset(PNMS_Gonad, Species_Common!="Big Eye")
BET<-subset(PNMS_Gonad, Species_Common!="Yellowfin Tuna")

#Plot a histogram for each - sizes

YFT_Sizes<-ggplot(YFT, aes(x = FL_cm, fill=Sex)) +
  geom_histogram(position = "identity", alpha = 0.4, binwidth=5)

YFT_Sizes

BET_Sizes<-ggplot(BET, aes(x = FL_cm, fill=Sex)) +
  geom_histogram(position = "identity", alpha = 0.4, binwidth=5)

BET_Sizes


#Look at GSI per species
YFT_GSI<-summarySE(YFT, measurevar="GSI", groupvars=c("Month", "Sex"))
YFT_Plot<-ggplot(YFT_GSI, aes(x=factor(Month), y=GSI,fill=factor(Sex)))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average YFT GSI per month")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  geom_errorbar(aes(ymin=GSI-se, ymax=GSI+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
YFT_Plot
YFT_GSI$Month <- factor(YFT_GSI$Month , levels = c( "September","October", "November","December","January","February", "March"))


BET_GSI<-summarySE(BET, measurevar="GSI", groupvars=c("Month", "Sex"))
BET_Plot<-ggplot(BET_GSI, aes(x=factor(Month), y=GSI,fill=factor(Sex)))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average BET GSI per month")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  geom_errorbar(aes(ymin=GSI-se, ymax=GSI+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
BET_Plot
BET_GSI$Month <- factor(BET_GSI$Month , levels = c( "September","October", "November","December","January","February", "March"))


