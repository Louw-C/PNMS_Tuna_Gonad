library(readxl)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(car)



#BOFI data
#Load data
PNMS_BOFI<-read.csv(file.choose(),header=T,sep=",")
names(PNMS_BOFI)

require(Rmisc)
require(ggplot2)
#Tuna length
BOFI_Total<-summarySE(data_long, measurevar="Abundance", groupvars=c("Month", "Type"))
BOFI1<-ggplot(BOFI_Total, aes(x=factor(Month), y=Abundance, fill=Type))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average catch per month")+
  geom_errorbar(aes(ymin=Abundance-se, ymax=Abundance+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
BOFI1

#Melt data
library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(PNMS_BOFI, Type, Abundance, Big.eye:Skipjack, factor_key=TRUE)
data_long
