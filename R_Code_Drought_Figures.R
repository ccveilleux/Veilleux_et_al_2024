### DROUGHT ANALYSIS FIGURE CODES #####

setwd("C:/Users/cveill/Dropbox/Kirindy_Working_Files_carrie/Manuscripts/Drought&Sifaka/Data/Final_Data/De-IdentifiedData")

library(ggplot2)
library(ggsci)
library(Rmisc)
library(stringr)
library(dplyr)

#### FIG 1: PHENOLOGY DURING DROUGHT ####
## Relative abundance of significant food items
## barplots with relative abundance of food items by month for drought and non-drought conditions

###### (1A & 1D) Young leaves ######
plotYL<-read.csv("Fig1a&d_YoungLeaves_data.csv",header=T,stringsAsFactors = T)
plotYL$Category<-factor(plotYL$YL,order=TRUE,levels=c("0","1","2","3","4"))
plotYL$Months <- factor(plotYL$Month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov"))
plotYL$Period<-factor(plotYL$Period,levels=c("Green","Yellow"))

##Monthly Average By Drought (1A)
ggplot(plotYL, aes(x = Drought, y=n, fill = Category)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~Months, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())+ylab("Proportion of Trees")
##Resource Period Average by Drought (1D)
ggplot(plotYL, aes(x = Period, y=n, fill = Category)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~Drought, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())+ylab("Proportion of Trees")

###### (1B & 1E) Mature leaves ######
plotML<-read.csv("Fig1b&e_MatureLeaves_data.csv",header=T,stringsAsFactors = T)
plotML$Category<-factor(plotML$ML,order=TRUE,levels=c("0","1","2","3","4"))
plotML$Months <- factor(plotML$Month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov"))
plotML$Period<-factor(plotML$Period,levels=c("Green","Yellow"))

##Monthly Average By Drought (1B)
ggplot(plotML, aes(x = Drought, y=n, fill = Category)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~Months, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())+ylab("Proportion of Trees")
##Resource Period Average by Drought (1D)
ggplot(plotML, aes(x = Period, y=n, fill = Category)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~Drought, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())+ylab("Proportion of Trees")

###### (1C & 1F) Ripe Fruit ######
plotFr<-read.csv("Fig1c&f_RipeFruit_data.csv",header=T,stringsAsFactors = T)

plotFr$Category<-factor(plotFr$Ripe.fruit,order=TRUE,levels=c("0","1","2","3","4"))
plotFr$Months <- factor(plotFr$Month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov"))
plotFr$Period<-factor(plotFr$Period,levels=c("Green","Yellow"))

##Monthly Average By Drought (1C)
ggplot(plotFr, aes(x = Drought, y=n, fill = Category)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~Months, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())+ylab("Proportion of Trees")
##Resource Period Average by Drought (1F)
ggplot(plotFr, aes(x = Period, y=n, fill = Category)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~Drought, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())+ylab("Proportion of Trees")


#### FIG 2 - PHENOLOGY YEAR AFTER DROUGHT ####
phen2<-subset(phen,YrAfter=="Yes")
phen3<-subset(phen,YrAfter=="No")
phen2<-rbind(phen2,phen3)
phen2<-subset(phen2,Green.Year>2011)

######(2a) Ripe Fruit ######
plotFr2<-read.csv("Fig2a_RipeFruit_YrAfter.csv",header=T, stringsAsFactors = T)

plotFr2$Category<-factor(plotFr2$Ripe.fruit,order=TRUE,levels=c("0","1","2","3","4"))
plotFr2$Period<-factor(plotFr2$Period,levels=c("Green","Yellow"))

ggplot(plotFr2, aes(x = Period, y=n, fill = Category)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~YrAfter, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())+ylab("Proportion of Trees")

##### (2B) Unripe Fruit #####
plotUF<-read.csv("Fig2b_UnripeFruit_YrAfter.csv",header=T, stringsAsFactors = T)

plotUF$Category<-factor(plotUF$unripe,order=TRUE,levels=c("0","1","2","3","4"))
plotUF$Period<-factor(plotUF$Period,levels=c("Green","Yellow"))

ggplot(plotUF, aes(x = Period, y=n, fill = Category)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~YrAfter, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())+ylab("Proportion of Trees")

###### (2C) Mature leaves ######
plotML2<-read.csv("Fig2c_MatureLeaves_YrAfter.csv",header=T, stringsAsFactors = T)

plotML2$Category<-factor(plotML2$ML,order=TRUE,levels=c("0","1","2","3","4"))
plotML2$Period<-factor(plotML2$Period,levels=c("Green","Yellow"))

ggplot(plotML2, aes(x = Period, y=n, fill = Category)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~YrAfter, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())+ylab("Proportion of Trees")


#### FIG 3: Total body fat for individual sifaka with before, during, and after drought measures ####
# Line plots of individual total body fat measures before and during drought:
# (A) Hand drawing of sifaka with fat measuring locations
# (B) Pre and Post 2016-2017 Drought estimates with black lines depicting individuals with before, during and after a drought measurements.
# (C) Pre and Post 2022 Drought estimates with black lines depicting individuals with before, during and after a drought measurements.
# "Code" category refers to [1] individuals that were only sampled before/during or during/after a drought (gold lines) and
# [2] individuals that were sampled before/during/after a drought (black lines)

##### (3b) - 2016-2017 Droughts #####
data1<-read.csv("Fig3b_2016-27_drought_TotalFat.csv",header=T,stringsAsFactors = T)
data1$Code<-as.factor(data1$Code)
p<-ggplot(data=data1, aes(x=Year2, y=Total.Fat, group=Lemur)) +
  geom_line(aes(color=Code))+geom_point() +theme_classic() +
  theme(legend.position="none")+scale_color_manual(values=c("goldenrod3","gray4"))
p+ ylab(c("Subcutaneous Body Fat"))

#####3c - 2022 Drought #####
data2<-read.csv("Fig3c_2022_drought_TotalFat.csv",header=T,stringsAsFactors = T)
data2$Code<-as.factor(data2$Code)
p<-ggplot(data=data2, aes(x=Year2, y=Total.Fat, group=Lemur)) +
  geom_line(aes(color=Code))+geom_point() +theme_classic() +
  theme(legend.position="none")+scale_color_manual(values=c("goldenrod3","gray4"))
p+ ylab(c("Subcutaneous Body Fat"))

#### FIG 4 - Feeding Behavior ####
##### (4a) - Fruit #####
fruit<-read.csv("Fig4a-FruitFeeding.csv",header=T, stringsAsFactors = T)
p<-ggplot(fruit, aes(x=Green.Year, y=PercFruit, colour=Drought,group=Drought)) +
  geom_errorbar(aes(ymin=PercFruit-sd, ymax=PercFruit+sd),width=0.1) +
  geom_point(aes(size = N))+theme_classic()
p+scale_color_manual(values=c("grey4","gold3"))+scale_x_continuous(breaks= pretty(fruit$Green.Year, n=13))+
  theme(axis.text.x = element_text(angle = 45))

##### (4b) - Flowers #####
flowers<-read.csv("Fig4b-FlowerFeeding.csv",header=T, stringsAsFactors = T)
p<-ggplot(flowers, aes(x=Green.Year, y=PercFlowers, colour=Drought,group=Drought)) +
  geom_errorbar(aes(ymin=PercFlowers-sd, ymax=PercFlowers+sd,width=0.1)) +  geom_point(aes(size = N))+theme_classic()
p+scale_color_manual(values=c("grey4","gold3"))+scale_x_continuous(breaks= pretty(flowers$Green.Year, n=13))+
  theme(axis.text.x = element_text(angle = 45))

##### (4c) - Sugars (Fruit + Flowers) #####
sugar<-read.csv("Fig4c-SugarFeeding.csv",header=T, stringsAsFactors = T)
p<-ggplot(sugar, aes(x=Green.Year, y=Sugar, colour=Drought,group=Drought)) +
  geom_errorbar(aes(ymin=Sugar-sd, ymax=Sugar+sd),width=0.1) +
  geom_point(aes(size = N))+theme_classic()
p+scale_color_manual(values=c("grey4","gold3"))+scale_x_continuous(breaks= pretty(sugar$Green.Year, n=13))+
  theme(axis.text.x = element_text(angle = 45))

##### (4d) - Young Leaves #####
YL<-read.csv("Fig4d-YoungLeavesFeeding.csv",header=T, stringsAsFactors = T)
p<-ggplot(YL, aes(x=Green.Year, y=PercYL, colour=Drought,group=Drought)) +
  geom_errorbar(aes(ymin=PercYL-sd, ymax=PercYL+sd),width=0.1) +
  geom_point(aes(size = N))+theme_classic()
p+scale_color_manual(values=c("grey4","gold3"))+scale_x_continuous(breaks= pretty(YL$Green.Year, n=13))+
  theme(axis.text.x = element_text(angle = 45))

##### (4e) - Mature Leaves #####
ML<-read.csv("Fig4e-MatureLeavesFeeding.csv",header=T, stringsAsFactors = T)
p<-ggplot(ML, aes(x=Green.Year, y=PercML, colour=Drought,group=Drought)) +
  geom_errorbar(aes(ymin=PercML-sd, ymax=PercML+sd),width=0.1) +
  geom_point(aes(size = N))+theme_classic()
p+scale_color_manual(values=c("grey4","gold3"))+scale_x_continuous(breaks= pretty(ML$Green.Year, n=13))+
  theme(axis.text.x = element_text(angle = 45))

##### (4f) - Seeds #####
seeds<-read.csv("Fig4f-SeedFeeding.csv",header=T, stringsAsFactors = T)
p<-ggplot(seeds, aes(x=Green.Year, y=PercSeeds, colour=Drought,group=Drought)) +
  geom_errorbar(aes(ymin=PercSeeds-sd, ymax=PercSeeds+sd),width=0.1) +
  geom_point(aes(size = N))+theme_classic()
p+scale_color_manual(values=c("grey4","gold3"))+scale_x_continuous(breaks= pretty(seeds$Green.Year, n=13))+
  theme(axis.text.x = element_text(angle = 45))+ylim(c(0,1))
