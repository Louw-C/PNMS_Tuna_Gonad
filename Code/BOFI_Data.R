library(readxl)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(car)
require(Rmisc)
require(ggplot2)

#BOFI data
#Load data
PNMS_BOFI<-read.csv(file.choose(),header=T,sep=",")
names(PNMS_BOFI)

#Change the format of the data from wide to long
library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
#Data now gives all types if fish in long format.
data_long <- gather(PNMS_BOFI, Type, Abundance, Big.eye:Skipjack, factor_key=TRUE)
names(data_long)

#Group data to months - to plot total number of fish per month. 
BOFI_Month_All<-data_long%>%
  group_by(Month, Type) %>%
  summarize(Abundance=sum(Abundance))

#Total number of fish caught per month - grouped into types 
#(note that number of trips/effort varies across months)

BOFI1<-ggplot(BOFI_Month_All, aes(x=factor(Month), y=Abundance, fill=Type))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Total monthly catch (number of fish)")+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, angle=90),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
BOFI1

#Order the x-axis labels
data_long$Month <- factor(data_long$Month, levels = c("June","July","August", "September","October", "November","December","January","February", "March"))


##BOFI tuna weights
BOFI_weight<-read.csv(file.choose(),header=T,sep=",")
names(BOFI_weight)
hist(BOFI_weight$Weight_kg)

#Remove Albacore

sans_albacore<-subset(BOFI_weight, Species!="Albacore")
sans_albacore<-subset(sans_albacore, Grade!="Not graded")

ggplot(sans_albacore, aes(x = Weight_kg,)) +
  geom_histogram(fill = "white", colour = "black", bins=100)

ggplot(sans_albacore, aes(x = Weight_kg,)) +
  geom_histogram(fill = "white", colour = "black", bins=100) +
  facet_grid(Species~.)

ggplot(sans_albacore, aes(x = Weight_kg,)) +
  geom_histogram(fill = "white", colour = "black", bins=50) +
  facet_grid(Species~Grade)

ggplot(BOFI_weight, aes(x = Weight_kg, fill=Species)) +
  geom_histogram(fill = "white", colour = "black", bins=50) +
  facet_grid(Grade~.)

sans_albacore$Grade <- factor(sans_albacore$Grade, levels = c("A","B+","B","R"))

#Remove outliers just for interest
sans_outliers<-subset(sans_albacore,Weight_kg<75)

ggplot(sans_outliers, aes(x = Weight_kg,)) +
  geom_histogram(fill = "white", colour = "black", bins=50) +
  facet_grid(Species~Grade)

#Plot tuna weight monthly averages - incudes all tuna and outliers

Tuna_Weight<-summarySE(BOFI_weight, measurevar="Weight_kg", groupvars=c("Month","Species"))
BOFI2<-ggplot(Tuna_Weight, aes(x=factor(Month), y=Weight_kg, fill=Species))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average monthly weight of catch (kg)")+
  geom_errorbar(aes(ymin=Weight_kg-se, ymax=Weight_kg+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, angle=90),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
BOFI2
BOFI_weight$Month <- factor(BOFI_weight$Month, levels = c("June","July","August", "September","October", "November","December","January","February", "March"))


#group weights per month to plot total catch in kg per month for different species.
BOFI_Month<-BOFI_weight%>%
  group_by(Month, Species, Grade) %>%
  summarize(Weight_kg=sum(Weight_kg))

BOFI3<-ggplot(BOFI_Month, aes(x=factor(Month), y=Weight_kg, fill=Species))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Total monthly weight of catch (kg)")+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, angle=90),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
BOFI3
BOFI_Month$Month <- factor(BOFI_Month$Month, levels = c("June","July","August", "September","October", "November","December","January","February", "March"))
