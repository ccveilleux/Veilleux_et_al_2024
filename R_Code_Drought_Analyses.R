#### SIFAKA DROUGHT ANALYSIS CODE #####

#packages needed
library(lme4) #GLMMs and LMMs
library(lmerTest) #statistical analyses of LMMs
library(ordinal) #cumulative link mixed models with ordinal data
library(glmmTMB) # 

#### I. Phenology ######
#package to use:
library(ordinal) #cumulative link mixed models with ordinal data

phen<-read.csv('Phenology_Data_2024_deID.csv',header=T, stringsAsFactors=T)

#recode abundance data as ordinal
phen$YL<-factor(phen$YL,order=TRUE,levels=c("0","1","2","3","4"))
phen$Ripe.fruit<-factor(phen$Ripe.fruit,order=TRUE,levels=c("0","1","2","3","4"))
phen$ML<-factor(phen$ML,order=TRUE,levels=c("0","1","2","3","4"))
phen$unripe<-factor(phen$unripe,order=TRUE,levels=c("0","1","2","3","4"))
phen$Flowers<-factor(phen$Flowers,order=TRUE,levels=c("0","1","2","3","4"))

#recode tree number and species to have shorter names (so no errors in clmm)
phen$tree<-phen$treeID
phen$spp<-phen$SpeciesID

summary(phen)

##### A. Relative abundance analyses: drought year #####
## Run two models (null and drought) for each response variable
## Then perform likelihood ratio test (LRT) with anova()

##Young Leaves
mm1<-clmm(YL~Season2*Drought + (1|tree:spp) + (1|Month)+(1|Green.Year), 
          data = phen, na.action = na.exclude)
mm1.null<-clmm(YL~Season2 + (1|tree:spp) + (1|Month)+(1|Green.Year), 
               data = phen, na.action = na.exclude)
anova(mm1.null,mm1)

##Mature Leaves
mm2<-clmm(ML~Season2*Drought + (1|tree:spp) + (1|Month)+(1|Green.Year), 
          data = phen, na.action = na.exclude)
mm2.null<-clmm(ML~Season2 + (1|tree:spp) + (1|Month)+(1|Green.Year), 
               data = phen, na.action = na.exclude)
anova(mm2.null,mm2)

##Ripe Fruit
mm3.null<-clmm(Ripe.fruit~Season2 + (1|tree:spp) + (1|Month)+(1|Green.Year), 
               data = phen, na.action = na.exclude)
mm3<-clmm(Ripe.fruit~Season2*Drought + (1|tree:spp) + (1|Month)+(1|Green.Year), 
          data = phen, na.action = na.exclude)
anova(mm3.null,mm3)

##Unripe Fruit
mm4.null<-clmm(unripe~Season2 + (1|tree:spp) + (1|Month)+(1|Green.Year), 
               data = phen, na.action = na.exclude)
mm4<-clmm(unripe~Season2*Drought + (1|tree:spp) + (1|Month)+(1|Green.Year), 
          data = phen, na.action = na.exclude)
anova(mm4.null,mm4)

##Flowers
mm5.null<-clmm(Flowers~Season2 + (1|tree:spp) + (1|Month)+(1|Green.Year), 
               data = phen, na.action = na.exclude)
mm5<-clmm(Flowers~Drought*Season2 + (1|tree:spp) + (1|Month)+(1|Green.Year), 
          data = phen, na.action = na.exclude)
anova(mm5.null,mm5)


##### C. Relative abundance analyses: year after drought #####
#remove observations before 2011 (since no rainfall data for 2010) 
#and observations from drought years (coded as "NA" for YrAfter)
#Then repeat models & LRTs

phen.sub<-subset(phen,Actual.Year>2011)
phen.subA<-subset(phen.sub,YrAfter=="No")
phen.subB<-subset(phen.sub,YrAfter=="Yes")
phen.sub<-rbind(phen.subA,phen.subB)

##Young Leaves
yrm1<-clmm(YL~Season2*YrAfter + (1|tree:spp) + (1|Month) + (1|Green.Year), 
          data = phen.sub, na.action = na.exclude)
yrm1.null<-clmm(YL~Season2 + (1|tree:spp) + (1|Month) + (1|Green.Year), 
           data = phen.sub, na.action = na.exclude)
anova(mm1,mm1B)

##Mature Leaves
yrm2<-clmm(ML~Season2*YrAfter + (1|tree:spp) + (1|Month) + (1|Green.Year), 
           data = phen.sub, na.action = na.exclude)
yrm2.null<-clmm(ML~Season2 + (1|tree:spp) + (1|Month) + (1|Green.Year), 
                data = phen.sub, na.action = na.exclude)
anova(yrm2.null,yrm2)

##Ripe Fruit
yr.mm3<-clmm(Ripe.fruit~Season2*YrAfter + (1|tree:spp) + (1|Month) + (1|Green.Year), 
             data = phen.sub, na.action = na.exclude)
yr.mm3.null<-clmm(Ripe.fruit~Season2 + (1|tree:spp) + (1|Month) + (1|Green.Year), 
                  data = phen.sub, na.action = na.exclude)
anova(yr.mm3.null,yr.mm3)

##Unripe fruit
yr.mm4<-clmm(unripe~Season2*YrAfter + (1|tree:spp) + (1|Month) + (1|Green.Year), 
             data = phen.sub, na.action = na.exclude)
yr.mm4.null<-clmm(unripe~Season2 + (1|tree:spp) + (1|Month) + (1|Green.Year), 
                  data = phen.sub, na.action = na.exclude)
anova(yr.mm4.null,yr.mm4)

##Flowers
yr.mm5<-clmm(Flowers~Season*YrAfter + (1|tree:spp) + (1|Month) + (1|Green.Year), 
             data = phen.sub, na.action = na.exclude)
yr.mm5.null<-clmm(Flowers~Season + (1|tree:spp) + (1|Month) + (1|Green.Year), 
                  data = phen.sub, na.action = na.exclude)
anova(yr.mm5.null,yr.mm5)


##### D. Feeding tree/vine mortality and drought #####

dead<-read.csv("Tree_mortality_summary_data.csv",header=T, stringsAsFactors = T,fileEncoding="UTF-8-BOM")

# one tailed test for increased mortality during drought
wilcox.test(dead$Dead.Trees~dead$Drought, alternative="less")

#one tailed test for increased mortality during year following drought
wilcox.test(dead$Dead.Trees~dead$Yr.After, alternative="less")


#### II: BODY CONDITION ####
## body condition data (body mass, BMI, fat measures) by animal ID, age class, 
#sex, sampling year, and drought year
data1<-read.csv("Body_Condition_2024_deID.csv",header=T, stringsAsFactors = T,fileEncoding="UTF-8-BOM")

###### A. Total Body Fat Models ######
##Models to compare
fat.m1<-lmer(Total.Fat~Pregnant+(1|Lemur)+(1|Year),data=data1)
fat.m2<-lmer(Total.Fat~AgeClass+(1|Lemur)+(1|Year),data=data1)
fat.m3<-lmer(Total.Fat~Drought+(1|Lemur)+(1|Year),data=data1)
fat.m4<-lmer(Total.Fat~Pregnant+AgeClass+(1|Lemur)+(1|Year),data=data1)
fat.m5<-lmer(Total.Fat~Pregnant+AgeClass+Drought+(1|Lemur)+(1|Year),data=data1)
fat.m6<-lmer(Total.Fat~Pregnant+Drought+(1|Lemur)+(1|Year),data=data1)
fat.m7<-lmer(Total.Fat~Pregnant*Drought+AgeClass+(1|Lemur)+(1|Year),data=data1)
fat.null<-lmer(Total.Fat~1+(1|Lemur)+(1|Year),data=data1)

## calculate AIC to find best fit model
AIC(fat.m1,fat.m2,fat.m3,fat.m4,fat.m5,fat.m6,fat.m7,fat.null)

## Change default Sex/Pregnant category to compare between males and two pregant categories
fat.m6B<-lmer(Total.Fat~Pregnant2+Drought+(1|Lemur)+(1|Year),data=data1)

exp(confint(fat.m6,method="Wald")) ## confidence interval

##Perform models for separate fat measures (insert into model): 
#Fat.back #Fat.belly #Fat.arm #Fat.hip
fat.m1<-lmer(Fat.hip~Pregnant+(1|Lemur)+(1|Year),data=data1)
fat.m2<-lmer(Fat.hip~AgeClass+(1|Lemur)+(1|Year),data=data1)
fat.m3<-lmer(Fat.hip~Drought+(1|Lemur)+(1|Year),data=data1)
fat.m4<-lmer(Fat.hip~Pregnant+AgeClass+(1|Lemur)+(1|Year),data=data1)
fat.m5<-lmer(Fat.hip~Pregnant+AgeClass+Drought+(1|Lemur)+(1|Year),data=data1)
fat.m6<-lmer(Fat.hip~Pregnant+Drought+(1|Lemur)+(1|Year),data=data1)
fat.m7<-lmer(Fat.hip~Pregnant*Drought+AgeClass+(1|Lemur)+(1|Year),data=data1)
fat.null<-lmer(Fat.hip~1+(1|Lemur)+(1|Year),data=data1)

## calculate AIC to find best fit model
AIC(fat.m1,fat.m2,fat.m3,fat.m4,fat.m5,fat.m6,fat.m7,fat.null)

###### B. Body Mass Index (BMI) Models ######
bmi.m1<-lmer(BMI~Pregnant+(1|Lemur)+(1|Year),data=data1)
bmi.m2<-lmer(BMI~AgeClass+(1|Lemur)+(1|Year),data=data1)
bmi.m3<-lmer(BMI~Drought+(1|Lemur)+(1|Year),data=data1)
bmi.m4<-lmer(BMI~Pregnant+AgeClass+(1|Lemur)+(1|Year),data=data1)
bmi.m5<-lmer(BMI~Pregnant+AgeClass+Drought+(1|Lemur)+(1|Year),data=data1)
bmi.m6<-lmer(BMI~Pregnant+Drought+(1|Lemur)+(1|Year),data=data1)
bmi.m7<-lmer(BMI~Pregnant*Drought+AgeClass+(1|Lemur)+(1|Year),data=data1)
bmi.null<-lmer(BMI~1+(1|Lemur)+(1|Year),data=data1)

## calculate AIC to find best fit model
AIC(bmi.m1,bmi.m2,bmi.m3,bmi.m4,bmi.m5,bmi.m6,bmi.m7, bmi.null)


###### C. Body mass models ###### 
mass.m1<-lmer(Weight~Pregnant+(1|Lemur)+(1|Year),data=data1)
mass.m2<-lmer(Weight~AgeClass+(1|Lemur)+(1|Year),data=data1)
mass.m3<-lmer(Weight~Drought+(1|Lemur)+(1|Year),data=data1)
mass.m4<-lmer(Weight~Pregnant+AgeClass+(1|Lemur)+(1|Year),data=data1)
mass.m5<-lmer(Weight~Pregnant+AgeClass+Drought+(1|Lemur)+(1|Year),data=data1)
mass.m6<-lmer(Weight~Pregnant+Drought+(1|Lemur)+(1|Year),data=data1)
mass.m7<-lmer(Weight~Pregnant*Drought+AgeClass+(1|Lemur)+(1|Year),data=data1)
mass.null<-lmer(Weight~1+(1|Lemur)+(1|Year),data=data1)

## calculate AIC to find best fit model
AIC(mass.m1,mass.m2,mass.m3,mass.m4,mass.m5,mass.m6,mass.m7,mass.null)


#### II. MEASURES OF REPRODUCTIVE SUCCESS####

##### A. Reproductive Output #####
data3<-read.csv("Reproductive_Output_2024_deID.csv",header=T, stringsAsFactors = T,fileEncoding="UTF-8-BOM")

#Create subset dataset of adults only (data3B)
data3B<-subset(data3,AgeClass=="adult")

###### i. Does drought during conception year affect reproductive output? ######
#adults + subadults models (data3)
conception.m1<-glmer(Pregnant~Conception.Drought+AgeClass+(1|Female)+(1|Year),
                     data=data3, family=binomial(link = "logit"),na.action =na.exclude)
conception.null1<-glmer(Pregnant~AgeClass+(1|Female)+(1|Year),
                        data=data3, family=binomial(link = "logit"),na=na.exclude)
#LRT:
anova(conception.m1,conception.null1)

#adults only models (data3B)
conception.m2<-glmer(Pregnant~Conception.Drought+(1|Female)+(1|Year),
                     data=data3B, family=binomial(link = "logit"),na.action =na.exclude)
conception.null2<-glmer(Pregnant~1+(1|Female)+(1|Year),data=data3B, 
                        family=binomial(link = "logit"),na=na.exclude)
#LRT:
anova(conception.m2,conception.null2)

###### ii. Does drought in the year prior to conception affect reproductive output? ######
#remove any infants born before 2012 (year prior to conception = 2011)
data3_non<-subset(data3,Pre.ConceptionYr.Drought!="NA")
data3C<-subset(data3,Year>2011)
data3D<-subset(data3B,Year>2011)

#adults + subadults models (data3)
output.m1<-glmer(Pregnant~Pre.ConceptionYr.Drought+AgeClass+(1|Female)+(1|Year),
                 data=data3C, family=binomial(link = "logit"),na.action = na.exclude)
output.null1<-glmer(Pregnant~AgeClass+(1|Female)+(1|Year),data=data3C,
                    family=binomial(link = "logit"),na=na.exclude)
#LRT:
anova(output.m1,output.null1)

#adults only models (data3B)
output.m2<-glmer(Pregnant~Pre.ConceptionYr.Drought+(1|Female)+(1|Year),
                 data=data3D,control=glmerControl(optimizer="bobyqa"), family=binomial(link = "logit"),na.action = na.exclude)
output.null2<-glmer(Pregnant~1+(1|Female)+(1|Year),data=data3D, control=glmerControl(optimizer="bobyqa"),
                    family=binomial(link = "logit"),na=na.exclude)

#LRT:
anova(output.m2,output.null2)


##### B. Infant Survival #####
data2<-read.csv("Infant_Survival_2024_deID.csv",header=T, stringsAsFactors = T,fileEncoding="UTF-8-BOM")

######i. Does drought during weaning year affect infant survival? ######
s.m1<-glmer(Survival~Drought.WeanYr+(1|Year),data=data2, family=binomial(link = "logit"),na=na.exclude)
s.nullA<-glmer(Survival~1+(1|Year),data=data2, family=binomial(link = "logit"),na=na.exclude)
#LRT 
anova(s.m1,s.nullA)

######ii. Does drought during birth year affect infant survival? ######
#remove any infants born before 2011 (no rainfall data for 2010 wet season)
data2B<-subset(data2,Year>2010)

#Note: MomID had very little variance in s.nullB model (singular fit warning) but was in s.m2 model so still included.
s.m2<-glmer(Survival~Drought.BirthYr+(1|Year)+(1|MomID),data=data2B, family=binomial(link = "logit"),na=na.exclude)
s.nullB<-glmer(Survival~1+(1|Year)+(1|MomID),data=data2B, family=binomial(link = "logit"),na=na.exclude)
# LRT:
anova(s.m2,s.nullB)

######iii. Does drought in the year prior to conception affect infant survival? ######

#remove any infants born before 2012 (year prior to conception = 2011)
data2C<-subset(data2,Year>2011)

#Note: MomID had very little variance in s.m3 model (singular fit warning) but was in s.nullC model, so still included.
s.m3<-glmer(Survival~Drought.PreconceptionYr+(1|MomID)+(1|Year),data=data2C, family=binomial(link = "logit"),na.action=na.exclude)
s.nullC<-glmer(Survival~1+(1|MomID)+(1|Year),data=data2C, family=binomial(link = "logit"),na.action=na.exclude)
#LRT:
anova(s.m3,s.nullC)


####III. Behavior ####
scans<-read.csv("Instantaneous_Scan_2024_deID.csv",header=T, stringsAsFactors = T)

## GLMMs with zero-inflated negative binomial distribution
library(glmmTMB)

###### A. Total Activity Budget ######
# for each dependent variable, we first test whether the glmmTMB nbinom1 or nbinom2
# distribution was more appropriate using the null models and a LRT. We then used the 
# best fit distribution.

## Feed
#null models
feed.nb2<- glmmTMB(Feed.Forage~Sex+AgeClass+
                     offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                   data=scans,
                   ziformula=~1,
                   family=nbinom2)
feed.nb2B<- glmmTMB(Feed.Forage~Sex+AgeClass+
                      offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                    data=scans,
                    ziformula=~1,
                    family=nbinom1)
#LRT to test best distribution
anova(feed.nb2B,feed.nb2)

#drought model - #nbinom2
feed.nb1<- glmmTMB(Feed.Forage~Sex+Drought+AgeClass+
                     offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                   data=scans,
                   ziformula=~1,
                   family=nbinom2)
#LRT:
anova(feed.nb2,feed.nb1)

## Lick
#null models
li.nb2<- glmmTMB(Lick~1+AgeClass+Sex+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom1)
li.nb2B<- glmmTMB(Lick~1+AgeClass+Sex+
                    offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                  data=scans,
                  ziformula=~1,
                  family=nbinom2)
anova(li.nb2B,li.nb2)

#drought model - #nbinom1
li.nb1<- glmmTMB(Lick~Drought+AgeClass+Sex+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom1)
#LRT:
anova(li.nb1,li.nb2)

#months with licking - drought vs no drought
r1<-c(23,17) ## number of months with licking in typical and drought
r2<-c(585,221) ## number of total months for typical and drought

lick1<-rbind(r1,r2)
fisher.test(lick1,alternative="less")


##Rest
#null models
rest.nb2<- glmmTMB(Rest~Sex+AgeClass+
                     offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                   data=scans,
                   ziformula=~1,
                   family=nbinom2)
rest.nb2B<- glmmTMB(Rest~Sex+AgeClass+
                      offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                    data=scans,
                    ziformula=~1,
                    family=nbinom1)
anova(rest.nb2B,rest.nb2)

#drought model - #nbinom2
rest.nb1<- glmmTMB(Rest~Sex+Drought+AgeClass+
                     offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                   data=scans,
                   ziformula=~1,
                   family=nbinom2)
#LRT:
anova(rest.nb1,rest.nb2)

##Social (social + groom + play)
#null models
so.nb2<- glmmTMB(Social.Groom.Play~Sex+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom2)
so.nb2B<- glmmTMB(Social.Groom.Play~Sex+AgeClass+
                    offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                  data=scans,
                  ziformula=~1,
                  family=nbinom1)
anova(so.nb2,so.nb2B)

#drought model - #nbinom2
so.nb1<- glmmTMB(Social.Groom.Play~Sex+Drought+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom1)
anova(so.nb1,so.nb2B)

##Travel
#null models
tr.nb2<- glmmTMB(Travel~Sex+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom2)
tr.nb2B<- glmmTMB(Travel~Sex+AgeClass+
                    offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                  data=scans,
                  ziformula=~1,
                  family=nbinom1)
anova(tr.nb2B,tr.nb2)

#drought model - #nbinom2
tr.nb1<- glmmTMB(Travel~Sex+Drought+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom2)
#LRT:
anova(tr.nb1,tr.nb2)

###### B. Feeding Activity Budget ######
# for each dependent variable, we first tested whether the glmmTMB nbinom1 or nbinom2
# distribution was more appropriate using the null models and a LRT. We then used the 
# best fit distribution.

#subset data to only feeding scans
feed.only<-subset(scans,All.Feed>0)

##Flowers
#null models
fl.nb2 <- glmmTMB(Feed.Flowers~Sex+AgeClass+
                    offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                  data=feed.only,
                  ziformula=~1,
                  family=nbinom1)
fl.nb2B <- glmmTMB(Feed.Flowers~Sex+AgeClass+
                    offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                  data=feed.only,
                  ziformula=~1,
                  family=nbinom2)
anova(fl.nb2B,fl.nb2)
#drought models - #nbinom1
fl.nb1 <- glmmTMB(Feed.Flowers~Sex+Drought+AgeClass+
                    offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                  data=feed.only,
                  ziformula=~1,
                  family=nbinom1)
anova(fl.nb1,fl.nb2)


##Fruit
#null models
fr.nb2<- glmmTMB(Feed.Fruit~Sex+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom1)
fr.nb2B<- glmmTMB(Feed.Fruit~Sex+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom2)
anova(fr.nb2,fr.nb2B)
#drought model - nbinom2
fr.nb1<- glmmTMB(Feed.Fruit~Sex+Drought+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom2)
anova(fr.nb1,fr.nb2B)

fr.nb1<- glmmTMB(Feed.Fruit~Sex+Drought+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom2)
fr.nb1B<- glmmTMB(Feed.Fruit~Sex+Drought+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom2)
anova(fr.nb1B,fr.nb1)

## Mature leaves
#null models
ML.nb2<- glmmTMB(Feed.Mature.Leaves~Sex+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom1)
ML.nb2B<- glmmTMB(Feed.Mature.Leaves~Sex+AgeClass+
                    offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                  data=feed.only,
                  ziformula=~1,
                  family=nbinom2)
anova(ML.nb2B,ML.nb2)
#drought model - nbinom1
ML.nb1<- glmmTMB(Feed.Mature.Leaves~Sex+Drought+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom1)
anova(ML.nb1,ML.nb2)

##Young leaves
#null models
YL.nb2<- glmmTMB(Feed.Young.Leaves~Sex+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom1)
YL.nb2B<- glmmTMB(Feed.Young.Leaves~Sex+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom2)
anova(YL.nb2,YL.nb2B)
#drought model - nbinom2
YL.nb1<- glmmTMB(Feed.Young.Leaves~Sex+Drought+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom2)
anova(YL.nb1,YL.nb2B)

##Seeds
#null models
seeds.nb2<- glmmTMB(Feed.Seeds~Sex+AgeClass+
                      offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                    data=feed.only,
                    ziformula=~1,
                    family=nbinom1)
seeds.nb2B<- glmmTMB(Feed.Seeds~Sex+AgeClass+
                       offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                     data=feed.only,
                     ziformula=~1,
                     family=nbinom2)
anova(seeds.nb2,seeds.nb2B)

seeds.nb1<- glmmTMB(Feed.Seeds~Sex+Drought+AgeClass+
                      offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                    data=feed.only,
                    ziformula=~1,
                    family=nbinom2)
#LRT:
anova(seeds.nb1,seeds.nb2B)


##Sugar (Fruit+flowers)
sugar.nb2<- glmmTMB((Feed.Fruit+Feed.Flowers)~Sex+AgeClass+
                      offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                    data=feed.only,
                    ziformula=~1,
                    family=nbinom1)
sugar.nb2B<- glmmTMB((Feed.Fruit+Feed.Flowers)~Sex+AgeClass+
                       offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                     data=feed.only,
                     ziformula=~1,
                     family=nbinom2)
anova(sugar.nb2,sugar.nb2B)

sugar.nb1<- glmmTMB((Feed.Fruit+Feed.Flowers)~Sex+Drought+AgeClass+
                      offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Green.Year),
                    data=feed.only,
                    ziformula=~1,
                    family=nbinom2)
anova(sugar.nb2B,sugar.nb1)

#### IRR Effects Calculation#####
## Calculate effects + confidence intervals for significant drought effects:
## insert models into code:
#fruit (fr.nb1), flowers (fl.nb1), young leaves (YL.nb1), sugar (sugar.nb1)

exp(confint(sugar.nb1,method="Wald")) ## confidence interval