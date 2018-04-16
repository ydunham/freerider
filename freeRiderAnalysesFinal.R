## Yang et al. 2017 Free Rider Analyses 
## Fan Yang, Antonia Misch, You-jung Choi, and Yarrow Dunham
## Contact: yarrow.dunham@yale.edu
## This code should replicate all primary analyses in manuscript
## Created in RStudio 1.0.136, R version 3.3.2
## Update history
## - 7.14.17: created final file (YD)
## - 7.17.17: minor edits during error checking (YD)
## - 8.1.18: added aggregated analysis of all studies
## - 4.16.16: added Study 6 and updated figures

#### Setup ####

# required packages
library(tidyverse)   # 1.00
library(lme4)  # lmer / glmer  1.1-16
library(effsize) # cohens d calculator  0.7.1
library(optimx) # additional optimizers for glmer  2013.8.7


# read data: Studies 1-5
raw <- read.csv('Studies1-5Data.csv')

#read data: Study 6
Study6 <- read.csv('Study6Data.csv')


# subset to study datasets
Study1 <- subset(raw,Study==1) 
Study2 <- subset(raw,Study==2)
Study3 <- subset(raw,Study==3)
Study4 <- subset(raw,Study==4)
Study5 <- subset(raw,Study==5)

Study12 <- subset(raw,Study<3)

#### Study 1 ####

# convert to long format
longStudy1 <- Study1 %>%
  dplyr::select(ID,AgeCentered,10:13,22:25) %>% 
  gather(condition,response,Original_FR_Judgment:Original_Con_Preference, Comparison_FR_Judgment:Comparison_Con_Preference) %>% 
  mutate(intention=ifelse(substr(condition,1,2)=='Or','unwilling','unable')) %>%
  mutate (target=ifelse(grepl("FR",condition),"FreeRider","Contributor")) 

# sanity check: check balance
table(longStudy1$intention,longStudy1$target)

# fit maximal model
Mmax <- lmer(response ~ target*intention*AgeCentered + (1|ID),data=longStudy1)
summary(Mmax)
drop1(Mmax, test='Chisq')
# drop 3-way via LRT
M2way <- lmer(response ~ (target+intention+AgeCentered)^2 + (1|ID),data=longStudy1)
summary(M2way)
drop1(M2way, test='Chisq')
# drop target:age and intention:age
M2way2 <- lmer(response ~ target+intention+AgeCentered+target:intention+ (1|ID),data=longStudy1)
summary(M2way2)
drop1(M2way2, test='Chisq')

# use above as final model
options(contrasts = c("contr.SAS", "contr.SAS'"))  # relevel contrasts to unintentional and contributor, respectively
Study1final<-lmer(response ~ target+intention+AgeCentered+target:intention+(1|ID),data=longStudy1)
summary(Study1final) 
confint(Study1final) # output confidence intervals

## Compute effect sizes: d for various simple contrasts
# output means of free riders vs contributors
longStudy1 %>%
  group_by(target) %>%
  summarise(avg = mean(response))
# compute d
cohen.d(response ~ target, data=longStudy1) 

# intentional vs unintentional free riders
longStudy1 %>%
  filter(target=='FreeRider') %>%
  group_by(intention) %>%
  summarise(avg = mean(response))
# compute d
cohen.d(response ~ intention, data=filter(longStudy1,target=='FreeRider')) 

# untintentional free riders vs contributors
longStudy1 %>%
  filter(intention=='unable') %>%
  group_by(target) %>%
  summarise(avg = mean(response))
# compute d
cohen.d(response ~ target, data=filter(longStudy1,intention=='unable')) 

#### Study 2 ####

# convert to long format
longStudy2 <- Study2 %>%
  dplyr::select(ID,AgeCentered,10:13,22:25) %>% 
  gather(condition,response,Original_FR_Judgment:Original_Con_Preference, Comparison_FR_Judgment:Comparison_Con_Preference) %>% 
  mutate(intention=ifelse(substr(condition,1,2)=='Or','unwilling','unable')) %>%
  mutate (target=ifelse(grepl("FR",condition),"FreeRider","Contributor")) 

# sanity check: check balance
table(longStudy2$intention,longStudy2$target)

# fit maximal model
Mmax <- lmer(response ~ target*intention*AgeCentered + (1|ID),data=longStudy2)
summary(Mmax)
drop1(Mmax, test='Chisq')
# drop 3-way via LRT
M2way <- lmer(response ~ (target+intention+AgeCentered)^2 + (1|ID),data=longStudy2)
summary(M2way)
drop1(M2way, test='Chisq')
# drop intention:age
M2way2 <- lmer(response ~ target+intention+AgeCentered+target:intention+target:AgeCentered + (1|ID),data=longStudy2)
summary(M2way2)
drop1(M2way2, test='Chisq')

# use above as final model
options(contrasts = c("contr.SAS", "contr.SAS'"))  # relevel contrasts to unintentional and contributor, respectively
Study2final<-lmer(response ~ target+intention+AgeCentered+target:intention+target:AgeCentered + (1|ID),data=longStudy2)
summary(Study2final) 
confint(Study2final) # output confidence intervals

## Compute effect sizes: d for various simple contrasts
# output means of free riders vs contributors
longStudy2 %>%
  group_by(target) %>%
  summarise(avg = mean(response))
# compute d
cohen.d(response ~ target, data=longStudy2) 

# intentional vs unintentional free riders
longStudy2 %>%
  filter(target=='FreeRider') %>%
  group_by(intention) %>%
  summarise(avg = mean(response))
# compute d
cohen.d(response ~ intention, data=filter(longStudy2,target=='FreeRider')) 

#### Compare Studies 1 and 2 ####

longStudy12 <- Study12 %>%
  dplyr::select(ID,AgeinMonths,10:13,22:25) %>% 
  gather(condition,response,Original_FR_Judgment:Original_Con_Preference, Comparison_FR_Judgment:Comparison_Con_Preference) %>% 
  mutate(intention=ifelse(substr(condition,1,2)=='Or','unwilling','unable'),
         target=ifelse(grepl("FR",condition),"FreeRider","Contributor"),
         AgeCentered=AgeinMonths-mean(AgeinMonths))

# fit maximal model
Mmax <- lmer(response ~ target*intention*AgeCentered + (1|ID),data=longStudy12)
summary(Mmax)
drop1(Mmax, test='Chisq')
# use above as final model 
options(contrasts = c("contr.SAS", "contr.SAS'"))  # relevel contrasts to unintentional and contributor, respectively
Study12finalmodel <- lmer(response ~ target*intention*AgeCentered + (1|ID),data=longStudy12)
summary(Study12finalmodel)
confint(Study12finalmodel)

#### Study 3 ####

# convert to long format
longStudy3 <- Study3 %>%
  dplyr::select(ID,AgeCat,10:13,22:25) %>% 
  gather(condition,response,Original_FR_Judgment:Original_Con_Preference, Comparison_FR_Judgment:Comparison_Con_Preference) %>% 
  mutate(impact=ifelse(substr(condition,1,2)=='Or','impact','no impact')) %>%
  mutate (target=ifelse(grepl("FR",condition),"FreeRider","Contributor"),
          AgeCat=ifelse(AgeCat==1,-.5,.5)) 

# sanity check: check balance
table(longStudy3$impact,longStudy3$target)

# fit maximal model
Mmax <- lmer(response ~ target*impact*AgeCat + (1|ID),data=longStudy3)
summary(Mmax)
drop1(Mmax, test='Chisq')
# drop 3-way via LRT
M2way <- lmer(response ~ (target+impact+AgeCat)^2 + (1|ID),data=longStudy3)
summary(M2way)
drop1(M2way, test='Chisq')
# drop target:impact and impact:age
M2way2 <- lmer(response ~ target+impact+AgeCat+target:AgeCat+ (1|ID),data=longStudy3)
summary(M2way2)
drop1(M2way2, test='Chisq')

# use above as final model
options(contrasts = c("contr.SAS", "contr.SAS"))  # relevel contrasts to unintentional and contributor, respectively
Study3final <- lmer(response ~ target+impact+AgeCat+target:AgeCat+ (1|ID),data=longStudy3)
summary(Study3final) 
confint(Study3final) # output confidence intervals

# output means for free riders in the two impact conditions
longStudy3 %>%
  filter(target=='FreeRider') %>%
  group_by(impact) %>%
  summarise(avg = mean(response))

#### Study 4 ####

# convert to long format
longStudy4 <- Study4 %>%
  dplyr::select(ID,AgeCat,22:25) %>% 
  gather(condition,response,Comparison_FR_Judgment:Comparison_Con_Preference) %>% 
  mutate(conform=ifelse(substr(condition,1,2)=='Or','conform','nonconform')) %>%
  mutate (target=ifelse(grepl("FR",condition),"FreeRider","Contributor"),
          AgeCat=ifelse(AgeCat==1,-.5,.5)) 

# sanity check: should just have nonconform condition here
table(longStudy4$conform,longStudy4$target)

# fit maximal model
Mmax <- lmer(response ~ target*AgeCat + (1|ID),data=longStudy4)
summary(Mmax)
drop1(Mmax, test='Chisq')
# drop 3-way via LRT
M1way <- lmer(response ~ target+AgeCat + (1|ID),data=longStudy4)
summary(M1way)
drop1(M1way, test='Chisq')
# use above as final model
options(contrasts = c("contr.SAS", "contr.SAS"))  # relevel contrasts to unintentional and contributor, respectively
Study4final <- lmer(response ~ target + (1|ID),data=longStudy4)
summary(Study4final)
confint(Study4final)

# compare with Study 3, just free riders and just the original impact condition in Study 3
S3FRimpact <- filter(longStudy3,impact=='impact' & target=='FreeRider')[c(1,4)]
S3FRimpact$study <- 'three'
S4 <- filter(longStudy4,target=='FreeRider')[c(1,4)]
S4$study <- 'Four'
S34 <- rbind(S3FRimpact,S4)
mean(S3FRimpact$response)
mean(S4$response)
summary(lmer(response ~ study + (1|ID), data=S34))
confint(lmer(response ~ study + (1|ID), data=S34))


#### Study 5 ####

## start with evaluations data
longStudy5eval <- Study5 %>%
  dplyr::select(ID,AgeCat,10:13,22:25) %>% 
  gather(condition,response,Original_FR_Judgment:Original_Con_Preference, Comparison_FR_Judgment:Comparison_Con_Preference) %>% 
  mutate(outcome=ifelse(substr(condition,1,2)=='Or','Outcome','No Outcome')) %>%
  mutate (target=ifelse(grepl("FR",condition),"FreeRider","Contributor"),
          AgeCat=ifelse(AgeCat==1,-.5,.5))

# sanity check for condition balalnce
table(longStudy5eval$outcome,longStudy5eval$target)

# fit maximal model
Mmax <- lmer(response ~ target*outcome*AgeCat + (1|ID),data=longStudy5eval)
summary(Mmax)
drop1(Mmax, test='Chisq')
# drop 3-way via LRT
M2way <- lmer(response ~ (target+outcome+AgeCat)^2 + (1|ID),data=longStudy5eval)
summary(M2way)
drop1(M2way, test='Chisq')
# drop target:outcome and outcome:age
M2way2 <- lmer(response ~ target+outcome+AgeCat+target:AgeCat+ (1|ID),data=longStudy5eval)
summary(M2way2)
drop1(M2way2, test='Chisq')

#use above as final model
options(contrasts = c("contr.SAS", "contr.SAS"))  # relevel contrasts to unintentional and contributor, respectively
Study5final <- lmer(response ~ target+AgeCat+target:AgeCat+ (1|ID),data=longStudy5eval)
summary(Study5final)
confint(Study5final)

## Now move to punishment, costly and non-costly togeter
longStudy5pun <- Study5 %>%
  dplyr::select(ID,AgeCat,14:17,26:29) %>% 
  gather(condition,response,Original_FR_NoCostPunish:Original_Con_CostlyPunish, Comparison_FR_NoCostPunish:Comparison_Con_CostlyPunish) %>% 
  mutate(outcome=ifelse(substr(condition,1,2)=='Or','Outcome','No Outcome')) %>%
  mutate (cost=ifelse(grepl("No",condition),"NoCost","Costly")) %>%
  mutate (target=ifelse(grepl("FR",condition),"FreeRider","Contributor"),
          AgeCat=ifelse(AgeCat==1,-.5,.5))

# sanity check for condition balalnce
table(longStudy5pun$outcome,longStudy5pun$target,longStudy5pun$cost)

# fit maximal model

Mmax <- glmer(response ~ target*outcome*cost*AgeCat + (1|ID),family=binomial, data=longStudy5pun,
              control = glmerControl(optimizer = "optimx", calc.derivs = F,
                                     optCtrl = list(method = "nlminb")))
summary(Mmax)
drop1(Mmax,test='Chisq')

# drop 4-way
M3way <- glmer(response ~ (target+outcome+cost+AgeCat)^3 + (1|ID),family=binomial, data=longStudy5pun,
              control = glmerControl(optimizer = "optimx", calc.derivs = F,
                                     optCtrl = list(method = "nlminb")))
drop1(M3way, test='Chisq')

# drop all 3-ways
M2way <- glmer(response ~ (target+outcome+cost+AgeCat)^2 + (1|ID),family=binomial, data=longStudy5pun,
               control = glmerControl(optimizer = "optimx", calc.derivs = F,
                                      optCtrl = list(method = "nlminb")))
drop1(M2way, test='Chisq')

#  drop ns interactions
M2way2 <- glmer(response ~ target+outcome+cost+AgeCat + target:outcome  + (1|ID),family=binomial, data=longStudy5pun,
               control = glmerControl(optimizer = "optimx", calc.derivs = F,
                                      optCtrl = list(method = "nlminb")))
drop1(M2way2, test='Chisq')

# drop AgeCat to create final model
options(contrasts = c("contr.SAS", "contr.SAS"))  # relevel contrasts to unintentional and contributor, respectively
Study5finalPun <- glmer(response ~ target+outcome+cost + target:outcome +   (1|ID),family=binomial, data=longStudy5pun,
                control = glmerControl(optimizer = "optimx", calc.derivs = F,
                                       optCtrl = list(method = "nlminb")))
summary(Study5finalPun)
# run final model w/ default optimizers to make sure nothing odd about above
Study5finalPunDefaultOpt <- glmer(response ~ target+outcome+cost + target:outcome  +  (1|ID),family=binomial, data=longStudy5pun)
summary(Study5finalPunDefaultOpt)
exp(fixef(Study5finalPunDefaultOpt))  # output odds ratios for main effects (interpretations of interactions different)
confint(Study5finalPunDefaultOpt)

# what are the raw percentages for model effects
longStudy5pun %>%
  group_by(target) %>%
  summarise(avg = mean(response))  # free rider: 40%, contrib: 8%
longStudy5pun %>%
  group_by(outcome) %>%
  summarise(avg = mean(response))  # 24% v 23%; ns
longStudy5pun %>%
  group_by(cost) %>%
  summarise(avg = mean(response))  # 20% v 28%
longStudy5pun %>%
  group_by(target,outcome) %>%
  summarise(avg = mean(response))  
# Contrib: 13 v 3; FR: 36 v 44
# So contrib punished a little more in no outcome while FR punished a little more in outcome

## linear probability models to confirm we end up in the same place
M4wayLin <- lmer(response ~ (target+outcome+cost+AgeCat)^4 + (1|ID), data=longStudy5pun)
drop1(M4wayLin, test='Chisq')
M3wayLin <- lmer(response ~ (target+outcome+cost+AgeCat)^3 + (1|ID), data=longStudy5pun)
drop1(M3wayLin, test='Chisq')
M2wayLin <- lmer(response ~ (target+outcome+cost+AgeCat)^2 + (1|ID), data=longStudy5pun)
drop1(M2wayLin, test='Chisq')
M2wayLin2 <- lmer(response ~ target+outcome+cost + AgeCat + target:outcome +  (1|ID), data=longStudy5pun)
drop1(M2wayLin2, test='Chisq')
MlinFinal <- lmer(response ~ target+outcome+cost  + target:outcome +  (1|ID), data=longStudy5pun)
summary(MlinFinal)


## Now move to reward, who reward
longStudy5rew1 <- Study5 %>%
  dplyr::select(ID,AgeCat,18:19,30:31) %>% 
  gather(condition,response,Original_FR_RewardDecision:Original_Con_RewardDecision, Comparison_FR_RewardDecision:Comparison_Con_RewardDecision) %>% 
  mutate(outcome=ifelse(substr(condition,1,2)=='Or','Outcome','No Outcome')) %>%
  mutate (target=ifelse(grepl("FR",condition),"FreeRider","Contributor"),
          AgeCat=ifelse(AgeCat==1,-.5,.5))

# sanity check for condition balalnce
table(longStudy5rew1$outcome,longStudy5rew1$target)

Mmax <- glmer(response ~ target*outcome*AgeCat + (1|ID),family=binomial, data=longStudy5rew1,
              control = glmerControl(optimizer = "optimx", calc.derivs = F,
                                     optCtrl = list(method = "nlminb")))
drop1(Mmax,test='Chisq')
M2way <- glmer(response ~ (target+outcome+AgeCat)^2 + (1|ID),family=binomial, data=longStudy5rew1,
              control = glmerControl(optimizer = "optimx", calc.derivs = F,
                                     optCtrl = list(method = "nlminb")))
drop1(M2way,test='Chisq')
M1way <- glmer(response ~ target+outcome+AgeCat + (1|ID),family=binomial, data=longStudy5rew1)
drop1(M1way,test='Chisq')
Study5finalRew1 <- glmer(response ~ target + (1|ID),family=binomial, data=longStudy5rew1)
summary(Study5finalRew1)
exp(fixef(Study5finalRew1))
confint(Study5finalRew1)

# raw percentages
longStudy5rew1 %>%
  group_by(target) %>%
  summarise(avg = mean(response))

# now move to behavioral reward task
longStudy5rew2 <- Study5 %>%
  dplyr::select(ID,AgeCat,20:21,32:33) %>% 
  gather(condition,response,Original_FR_RewardGiven:Original_Con_RewardGiven, Comparison_FR_RewardGiven:Comparison_Con_RewardGiven) %>% 
  mutate(outcome=ifelse(substr(condition,1,2)=='Or','Outcome','No Outcome')) %>%
  mutate (target=ifelse(grepl("FR",condition),"FreeRider","Contributor"),
          AgeCat=ifelse(AgeCat==1,-.5,.5))

# start w/ maximal model
Mmax <-  lmer(response ~ target*outcome*AgeCat + (1|ID),data=longStudy5rew2)
drop1(Mmax,test='Chisq')
M2way <-  lmer(response ~ (target+outcome+AgeCat)^2 + (1|ID),data=longStudy5rew2)
drop1(M2way,test='Chisq')
M1way <-  lmer(response ~ target+outcome+AgeCat + (1|ID),data=longStudy5rew2)
drop1(M1way,test='Chisq')
Study5finalRew2 <- lmer(response ~ target + (1|ID),data=longStudy5rew2)
summary(Study5finalRew2)

# raw means
longStudy5rew2 %>%
  group_by(target) %>%
  summarise(avg = mean(response))


#### Aggregated across studies ####
# convert to long
allLong <- raw %>%
  dplyr::select(ID,AgeCentered,Study,AgeinMonths,AgeCat,10:13,22:25) %>%
  filter(AgeinMonths < 72 | AgeinMonths > 108) %>% 
  gather(condition,response,Original_FR_Judgment:Original_Con_Preference, Comparison_FR_Judgment:Comparison_Con_Preference) %>%
  filter(!is.na(response)) %>%
  filter(!(Study %in% c(1,2) & substr(condition,1,3) == 'Com')) %>%
  mutate (target=ifelse(grepl("FR",condition),"FreeRider","Contributor"),
          age=ifelse(AgeCat==1,'Young','Old'),
          study=as.factor(Study))

# number of unique subjects
length(unique(allLong$ID))

# Main model
options(contrasts = c("contr.SAS", "contr.SAS"))  # relevel default levels to contributor and young
M1 <- lmer(response ~ age*target + (1|ID) + (age + target|study), data=allLong)
summary(M1)
drop1(M1,test='Chisq')
summary(M1)

# get CIs
fixef(M1)["targetContributor"] + c(-1.96,1.96)*se.fixef(M1)["targetContributor"]
fixef(M1)["ageOld"] + c(-1.96,1.96)*se.fixef(M1)["ageOld"]
fixef(M1)["ageOld:targetContributor"] + c(-1.96,1.96)*se.fixef(M1)["ageOld:targetContributor"]


# means and effect sizes, by age group
allLong %>%
  group_by(age,target) %>%
  summarise(avg = mean(response))
# compute d
cohen.d(response ~ target, data=filter(allLong,age=='Young')) 
cohen.d(response ~ target, data=filter(allLong,age=='Old')) 

#### Study 6 ####

# who thought they were playing alonen versus with others
mean(Study6$Alone)#0.10

# Evaluation convert to long format, create contrast code for condition so intercept reflects mean effect
longStudy6 <- Study6 %>%
  dplyr::select(ID,Condition,Judgment,Preference) %>% 
  gather(measure,response,Judgment,Preference)  %>%
  mutate(Cond = ifelse(Condition == 'Impact',.5,-.5))

# sanity check: check balance
table(longStudy6$measure,longStudy6$Condition)

# fit maximal model
Mmax <- lmer(response ~ Cond*measure + (1|ID),data=longStudy6)
summary(Mmax)
drop1(Mmax,test='Chisq')

# drop 2-way:
M2way2 <- lmer(response ~ Cond+measure + (1|ID),data=longStudy6)
summary(M2way2)
drop1(M2way2,test='Chisq')

# final model
Mfinal <- lmer(response ~ Cond + (1|ID),data=longStudy6)
summary(Mfinal)
confint(Mfinal) # output confidence intervals

# output means for judgment and preference measures in the two conditions
longStudy6 %>%
  filter(measure=='Judgment') %>%
  group_by(Condition) %>%
  summarise(avg = mean(response))
longStudy6 %>%
  filter(measure=='Preference') %>%
  group_by(Condition) %>%
  summarise(avg = mean(response))

# Punishment: punish contributor=1, punish free rider=0 
#Contrast coding the condition variable###
Study6$Cond <- ifelse(Study6$Condition == 'Impact',.5,-.5)

Study6punish <- glm(Punish ~ Cond,family=binomial, data=Study6)
summary(Study6punish)
confint(Study6punish)
exp(coef(Study6punish)) #compute Odds Ratio


# Collaborate
Study6collaborate <- glm(Collaborate ~ Cond,family=binomial, data=Study6)
summary(Study6collaborate)
confint(Study6collaborate) 
exp(coef(Study6collaborate))


#### Supplementa analysis dropping children who beleived they played alone #####

# Evaluation: believers
Study6Believers<-subset(Study6,Alone==0)
longStudy6b <- Study6Believers %>%
  dplyr::select(ID,Condition,Judgment,Preference) %>% 
  gather(measure,response,Judgment,Preference)  %>%
  mutate(Cond = ifelse(Condition == 'Impact',.5,-.5))

# sanity check: check balance
table(longStudy6b$measure,longStudy6b$Condition)

# fit maximal model
Mmax <- lmer(response ~ Cond*measure + (1|ID),data=longStudy6b)
summary(Mmax)
drop1(Mmax,test='Chisq')

# drop 2-way:
M2way2 <- lmer(response ~ Cond+measure + (1|ID),data=longStudy6b)
summary(M2way2)
drop1(M2way2,test='Chisq')

# final model
Mfinal <- lmer(response ~ Cond + (1|ID),data=longStudy6b)
summary(Mfinal)
confint(Mfinal) # output confidence intervals

# output means for judgment and preference measures in the two conditions
longStudy6b %>%
  filter(measure=='Judgment') %>%
  group_by(Condition) %>%
  summarise(avg = mean(response))
longStudy6b %>%
  filter(measure=='Preference') %>%
  group_by(Condition) %>%
  summarise(avg = mean(response))

# Punishment: Believers
#Contrast coding the condition variable###
longStudy6b$Cond <- ifelse(longStudy6b$Condition == 'Impact',.5,-.5)

Study6punish <- glm(Punish ~ Cond,family=binomial, data=Study6Believers)
summary(Study6punish)
confint(Study6punish)
exp(coef(Study6punish)) #compute Odds Ratio


# Collaborate: Believers
Study6collaborate <- glm(Collaborate ~ Cond,family=binomial, data=Study6Believers)
summary(Study6collaborate)
confint(Study6collaborate) 
exp(coef(Study6collaborate))  #compute Odds Ratio

#### Paper Figures ####

# NOTE: This includes a subset of figures only
library(scales)

# Study1
substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
longStudy1Graph <- Study1 %>%
  dplyr::select(ID,10:13,22:25) %>% 
  gather(condition,response,Original_FR_Judgment:Original_Con_Preference, Comparison_FR_Judgment:Comparison_Con_Preference) %>% 
  mutate(Ttype=substr(condition,1,2)) %>% #Ttype is testing condition
  mutate (Ctype=ifelse(grepl("FR",condition),"FreeRider","Contributor")) #Ctype is Character/Target type

longStudy1Graph$Ttype<-factor(longStudy1Graph$Ttype,levels=c("Or","Co"))#Change the order for Ttype
longStudy1Graph$Ctype<-factor(longStudy1Graph$Ctype,levels=c("FreeRider","Contributor"))#change the order for Ctype

graph1 <- ggplot(longStudy1Graph,aes(x=Ctype, y = response, width=.65, fill = as.factor(Ttype))) +
  stat_summary(fun.y=mean, geom="bar", position=position_dodge()) + 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar",  aes(width=.15), position=position_dodge(.65)) 
graph1
graph1<-(graph1 +
           theme(legend.title=element_blank()) + 
           scale_x_discrete(breaks=c("Contributor", "FreeRider"),labels=c("Contributor", "Free Rider")) + 
           scale_y_continuous(limits=c(1,4),oob=squish,expand = c(0,0))+
           ylab("Rating (1 = most negative, 4 = most positive)") +
           xlab("Target") +
           theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
           theme(legend.title=element_blank()) +
           scale_fill_manual(values=c("#99CC99", "#FFCC99"),breaks=c("Co", "Or"),labels=c("Unintentional Free-Riding", "Intentional Free-Riding")) +
           geom_hline(yintercept = 2.5, linetype=2, color='black')+
           guides(fill = guide_legend(reverse = TRUE))+ #reverse the legend order
           ggtitle("a) 6-10-year-olds") + theme(plot.title=element_text(hjust = "0", face="bold"))+
           theme(legend.position="top") +
           theme(legend.direction="horizontal")+
           theme(plot.title = element_text(size = 12, face = "bold") , legend.text=element_text(size=9))+
           theme(axis.text=element_text(size=10,face="bold"), axis.title=element_text(size=10,face="bold")))
graph1

# Study2
substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
longStudy2Graph <- Study2 %>%
  dplyr::select(ID,10:13,22:25) %>% 
  gather(condition,response,Original_FR_Judgment:Original_Con_Preference, Comparison_FR_Judgment:Comparison_Con_Preference) %>% 
  mutate(Ttype=substr(condition,1,2)) %>%
  mutate (Ctype=ifelse(grepl("FR",condition),"FreeRider","Contributor")) 

longStudy2Graph$Ttype<-factor(longStudy2Graph$Ttype,levels=c("Or","Co"))#Change the order for Ttype
longStudy2Graph$Ctype<-factor(longStudy2Graph$Ctype,levels=c("FreeRider","Contributor"))#change the order for Ctype

reordergraph2 <- ggplot(longStudy2Graph,aes(x=Ctype, y = response, width=.65, fill = as.factor(Ttype))) +
  stat_summary(fun.y=mean, geom="bar", position=position_dodge()) + 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar",  aes(width=.15), position=position_dodge(.65)) 
reordergraph2

graph2<-(reordergraph2 +
           theme(legend.title=element_blank()) + 
           scale_x_discrete(breaks=c("Contributor", "FreeRider"),labels=c("Contributor", "Free Rider")) + 
           scale_y_continuous(limits=c(1,4),oob=squish,expand = c(0,0))+
           ylab("Rating (1 = most negative, 4 = most positive)") +
           xlab("Target") +
           theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
           theme(legend.title=element_blank()) +
           scale_fill_manual(values=c("#99CC99", "#FFCC99"),breaks=c("Co", "Or"),labels=c("Unintentional Free-Riding", "Intentional Free-Riding")) +
           geom_hline(yintercept = 2.5, linetype=2, color='black')+
           guides(fill = guide_legend(reverse = TRUE))+ #reverse the legend order
           ggtitle("b) 4-5-year-olds") + theme(plot.title=element_text(hjust = "0", face="bold"))+
           theme(legend.position="top") +
           theme(legend.direction="horizontal")+
           theme(plot.title = element_text(size = 12, face = "bold") , legend.text=element_text(size=9))+
           theme(axis.text=element_text(size=10,face="bold"), axis.title=element_text(size=10,face="bold")))
graph2


#save Study1 & 2 graphs into a single image############
library(grid)
library(gridExtra)
study12combined<-(grid.arrange(graph1,graph2,ncol=2,top=textGrob("Children's evaluations of free-riders and contributors in Study 1 and Study 2",gp=gpar(fontsize=12,fontface="bold"))))
study12combined


#### aggregated graph ####

allLong$target2<-factor(allLong$target,levels=c("FreeRider","Contributor"))#change the order for Ctype
allLong$age2<-factor(allLong$age,levels=c("Young","Old"))#change the order for Ctype

frame <- ggplot(allLong,aes(x=target2,y=response,fill=age2, width=.65)) +  
  stat_summary(fun.y=mean, geom="bar", size=4, position = position_dodge()) +
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=.15), position=position_dodge(.65))

graph7 <- frame +   theme(legend.title=element_blank()) + 
  scale_x_discrete(breaks=c("Contributor", "FreeRider"),labels=c("Contributor", "Free Rider")) +
  scale_y_continuous(limits=c(1,4.2),oob=squish,expand = c(0,0))+
  ylab("Rating (1 = most negative, 4 = most positive)") +
  xlab("Target") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values=c("#99CC99", "#FFCC99"),breaks=c("Young", "Old"),labels=c("4-5-yr-olds", "9-10-yr-olds")) +
  geom_hline(yintercept = 2.5, linetype=2, color='black') +
  theme(plot.title=element_text(hjust = 0.5, face="bold")) +
  theme(legend.position=c(0.5,0.98)) +
  theme(legend.direction="horizontal") +  ggtitle("Summary of aggregated data in Studies 1-5")+
  theme(plot.title = element_text(size = 12, face = "bold") , legend.text=element_text(size=9))+
  theme(axis.text=element_text(size=10,face="bold"), axis.title=element_text(size=10,face="bold")) 
graph7






