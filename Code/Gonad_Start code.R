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


#GSI across time/species
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


#Plot fork length (cm) and weight (g)
plot(PNMS_Gonad$FL_cm~PNMS_Gonad$Total_weight_g)

#Developed code up the here - need to edit below this

#Kruskal-Wallis to look at overall changes across space and time
require(FSA) 
kruskal.test(SANS_m2~Year_Protection, data=Ngerumekaol_Fish_All)
PT = dunnTest(Abundance_Total~Year_Protection, data=Ngerumekaol_Fish_All,
              method="bh")
PT

#Mixed Effects model
library(lme4)
require(lmerTest)

#Model with effect of interaction protection, year and habitat - for data with snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
fish_all1<-lmer(Total_Biomass~Protection*Year*Habitat+(1|Station),data=Ngerumekaol_Fish_All)
drop1(fish_all1, test="Chi")

fish_all2<-lmer(Abundance_Total~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all2, test="Chi")

fish_all3<-lmer(Abundance_Total~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all3, test="Chi")

AIC(fish_all1,fish_all2,fish_all3)
library(effects)
plot(allEffects(fish_all1))
plot(allEffects(fish_all2))
plot(allEffects(fish_all3))

#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(fish_all1, pairwise ~ Year*Protection*Habitat)

fit3.res = resid(fish_all2)
plot(fit3.res)
abline(0, 0)

#Check collinearity of parameters
require(performance)
check_collinearity(fish_all2)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(fish_all2)
plot(x)

#Model with effect of interaction protection, year and habitat - for data without snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
fish1<-lmer(Total_Sans~Protection*Year*Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish1, test="Chi")

fish2<-lmer(Total_Sans~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish2, test="Chi")

fish3<-lmer(Total_Sans~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish3, test="Chi")

AIC(fish1,fish2,fish3)
library(effects)
plot(allEffects(fish1))
plot(allEffects(fish2))
plot(allEffects(fish3))

#check normality of residuals
fit3.res = resid(fish1)
plot(fit3.res)
abline(0, 0)    

Plot.fit3.Linearity<-plot(resid(fish1))
plot(fish1)
qqmath(fit3)

#Check collinearity of parameters
require(performance)
check_collinearity(fish1)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(fish1)
plot(x)


#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(fish1, pairwise ~ Year*Protection*Habitat)


##############BIOMASS##############
#Make basic figures
#All fish data
require(Rmisc)
require(ggplot2)
Fish_BIOMASS<-summarySE(Ngerumekaol_Fish_All, measurevar="Grams_square_meter", groupvars=c("Year", "Habitat", "Protection"))
FishBIO<-ggplot(Fish_BIOMASS, aes(x=factor(Year), y=Grams_square_meter,fill=factor(Protection)))+
  facet_grid(Habitat~.)+
  geom_col(position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=Grams_square_meter-se,max=Grams_square_meter+se),position=position_dodge(0.9), width=0.4)+
  labs(y = "Fish biomass (Grams/m2)")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
FishBIO

#Figure wihtout snapper biomass
Fish_BIOMASS_SANS<-summarySE(Ngerumekaol_Fish_All, measurevar="Biomass_Sans_g_m2", groupvars=c("Year", "Habitat", "Protection"))
FishBIO_SANS<-ggplot(Fish_BIOMASS_SANS, aes(x=factor(Year), y=Biomass_Sans_g_m2,fill=factor(Protection)))+
  facet_grid(Habitat~.)+
  geom_col(position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=Biomass_Sans_g_m2-se,max=Biomass_Sans_g_m2+se),position=position_dodge(0.9), width=0.4)+
  labs(y = "Fish biomass without Lutjanus gibbus (Grams/m2)")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
FishBIO_SANS


#Mixed Effects model for biomass
library(lme4)
require(lmerTest)

#Model with effect of interaction protection, year and habitat - for data with snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
fish_all1<-lmer(Total_Biomass~Protection*Year*Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all1, test="Chi")

fish_all2<-lmer(Total_Biomass~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all2, test="Chi")

fish_all3<-lmer(Total_Biomass~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all3, test="Chi")

AIC(fish_all1,fish_all2,fish_all3)
library(effects)
plot(allEffects(fish_all1))
plot(allEffects(fish_all2))
plot(allEffects(fish_all3))

#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(fish_all1, pairwise ~ Year*Protection*Habitat)

#Check your model
#Plot residuals

fit3.res = resid(fish_all1)
plot(fit3.res)
abline(0, 0)

#Check collinearity of parameters
require(performance)
check_collinearity(fish_all1)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(fish_all1)
plot(x)


#Model with effect of interaction protection, year and habitat - for data without snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
Biomass_Sans1<-lmer(Biomass_Sans~Protection*Year*Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish1, test="Chi")

Biomass_Sans2<-lmer(Biomass_Sans~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish2, test="Chi")

Biomass_Sans3<-lmer(Biomass_Sans~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish3, test="Chi")

AIC(Biomass_Sans1,Biomass_Sans2,Biomass_Sans3)
library(effects)
plot(allEffects(Biomass_Sans1))
plot(allEffects(Biomass_Sans2))
plot(allEffects(Biomass_Sans3))

#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(Biomass_Sans1.res, pairwise ~ Year*Protection*Habitat)

#check normality of residuals
Biomass_Sans1.res = resid(Biomass_Sans1)
plot(Biomass_Sans.res)
abline(0, 0)    

#Check collinearity of parameters
require(performance)
check_collinearity(Biomass_Sans1.res)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(Biomass_Sans1.res)
plot(x)

##########################################DIVERSITY###############################################

#Fish diversity_All fish species
require(vegan)
Ngerumekaol_Fish_diversity<-read.csv(file.choose(),header=T, row.names = 1,sep=",")
Diversity_Fish_Sites<-read.csv(file.choose(),header=T, row.names = 1,sep=",")

Fish_sr <- specnumber(Ngerumekaol_Fish_diversity)
boxplot(Fish_sr~Diversity_Fish_Sites$Protection)
Fish_shannon <- diversity(Ngerumekaol_Fish_diversity, index = 'shannon')
par(mfrow=c(1,2))
boxplot(Fish_shannon~Diversity_Fish_Sites$Protection, xlab="Protection status", ylab="Shannon Wiener Diversity Index")
boxplot(Fish_shannon~Diversity_Fish_Sites$Habitat, xlab="Habitat", ylab="Shannon Wiener Diversity Index")
