## Yang et al. 2017 Free Rider Analyses 
## Fan Yang, Antonia Misch, You-jung Choi, and Yarrow Dunham
## Contact: yarrow.dunham@yale.edu
## This code should replicate all primary analyses in manuscript
## Created in RStudio 1.0.136, R version 3.3.2
## Update history
## - 7.14.17: created final file (YD)
## - 7.17.17: minor edits during error checking (YD)

#### Setup ####

# required packages
library(tidyverse)   # 1.00
library(lme4)  # lmer / glmer  1.1-12
library(effsize) # cohens d calculator  0.7.0
library(optimx) # additional optimizers for glmer  2013.8.7


# read data
setwd("~/Documents/Active Projects/Fan Yang Projects/Free Riders/data")
raw <- read.csv('freeRiderAllData.csv')

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

# quick plot: shows main effect of cost across the facets, hint of interaction w/in facets
# though clearer if you drop the faceting
ggplot(longStudy5pun,aes(x=target,y=response,fill=outcome)) +
  stat_summary(fun.y=mean, geom="bar", position = position_dodge()) +
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=.25, position = position_dodge(.9)) +
  facet_grid(~cost)

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
