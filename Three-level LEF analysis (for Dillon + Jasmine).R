#LEF 3 level analysis
#Ben Brown, Dillon Browne Whole Family Lab

library(jtools)
library(tidyverse)
library(pander)
library(haven)
library(lme4)
library(dplyr)
library(ggplot2)
library(lmerTest)
library(sjPlot)
library(dplyr)
library(reshape2)
library(broom) 
library(stats) 

#####################
#
#     Prep Work
#
#####################

# Ben Brown's data file for creating a mixed level model to analyze classrooms

# Checking the working directory
getwd()

# Loading the data frame
dataLEF1 <- read_spss("/Users/benbrown/LEF_DB_03262020.sav")





# Deleting certain students who didn't have a classroom ID attached to them (Based on emails from Dillon, Shealyn)
dataLEF2 <- filter(dataLEF1, !(ID %in% c("CC102198","CC102669","CC103017", "CC103084", "CC103199","CC103208", "CC103442", "CC1022006", "CC102773", "CC103614", "CC104130")))

dataLEF2                               

# Checking to see if the filter works
any(dataLEF2==CC102198)







# Adjusting the time variable to be labelled as 0, 1, 2 rather than 1, 2, 3

dataLEF2 <- dataLEF2 %>% 
  mutate(Time.Adj = Time - 1)







# Changing the date variable to be a single integer
# Adding a new column with just the first date ->
dataLEF2$Date.Adj <- '2019-12-10'
# Finding the difference between each date and the first date
dataLEF2$diff_in_days <- difftime(dataLEF2$Date ,dataLEF2$Date.Adj , units = c("days"))

dataLEF2$diff_in_days






# Adding a new column of average IRS grouped by classroom (as per notes)
dataLEF2 <- dataLEF2 %>% 
  group_by(Classroom) %>% 
  mutate(IRS.CLASS.MEAN = mean(IRS_Total))

dataLEF2 <- dataLEF2 %>% 
  group_by(Classroom) %>% 
  mutate(IRS.CLASS.SD = sd(IRS_Total))

#Checking to see if average IRS by classroom variable works
colnames(dataLEF2)
dataLEF2$IRS.CLASS.MEAN







# Creating dummy variables for school type
dataLEF2$Dummy.All <- ifelse(dataLEF2$Classroom == 1 | dataLEF2$Classroom == 5 | dataLEF2$Classroom == 10 | dataLEF2$Classroom == 18 | dataLEF2$Classroom == 19, '0', #PRESCHOOL
                              ifelse(dataLEF2$Classroom == 2 | dataLEF2$Classroom == 6 | dataLEF2$Classroom == 7 | dataLEF2$Classroom == 11 | dataLEF2$Classroom == 12 | dataLEF2$Classroom == 13 | dataLEF2$Classroom == 14 | dataLEF2$Classroom == 20 | dataLEF2$Classroom == 21, '1', #FDK
                                        ifelse(dataLEF2$Classroom == 3 | dataLEF2$Classroom == 4 | dataLEF2$Classroom == 8 | dataLEF2$Classroom == 9 | dataLEF2$Classroom == 15 | dataLEF2$Classroom == 16 | dataLEF2$Classroom == 17 | dataLEF2$Classroom == 22, '2', 'something else'))) # School Age

dataLEF2$Dummy.PreSchool <- ifelse(dataLEF2$Classroom == 1 | dataLEF2$Classroom == 5 | dataLEF2$Classroom == 10 | dataLEF2$Classroom == 18 | dataLEF2$Classroom == 19, '1', '0') #PRESCHOOL

dataLEF2$Dummy.FDK <- ifelse(dataLEF2$Classroom == 2 | dataLEF2$Classroom == 6 | dataLEF2$Classroom == 7 | dataLEF2$Classroom == 11 | dataLEF2$Classroom == 12 | dataLEF2$Classroom == 13 | dataLEF2$Classroom == 14 | dataLEF2$Classroom == 20 | dataLEF2$Classroom == 21, '1', '0') #FDK
                                    
dataLEF2$Dummy.SchoolAge <- ifelse(dataLEF2$Classroom == 3 | dataLEF2$Classroom == 4 | dataLEF2$Classroom == 8 | dataLEF2$Classroom == 9 | dataLEF2$Classroom == 15 | dataLEF2$Classroom == 16 | dataLEF2$Classroom == 17 | dataLEF2$Classroom == 22, '1', '0') # School Age






# Adding a new column of average SDQ grouped by classroom (as per notes)
dataLEF2 <- dataLEF2 %>% 
  group_by(Classroom) %>% 
  mutate(SDQ.CLASS.MEAN = mean(SDQ_TotalD))

dataLEF2 <- dataLEF2 %>% 
  group_by(Classroom) %>% 
  mutate(SDQ.CLASS.SD = sd(SDQ_TotalD))





#Calculating the given % of boys in each classroom

dataLEF2 <- dataLEF2 %>%
  group_by(Classroom) %>%
  mutate(NumPat=n(),PROP.BOYS=sum(Gender==1)/n())





#Ordering variables
order(dataLEF2$ID, dataLEF2$Time, # Sequence of vectors of the same length
      decreasing = TRUE, # Whether to sort in increasing or decreasing order
      na.last = FALSE,     # Whether to put NA values at the beginning or at the end
      method = c("auto", "shell", "radix"))


#####################
#
#     Model 1: Unconditional growth
#
#####################

Model1 <- lmer(DESSAMini_Total ~ Time + 
                   (1 | Classroom:ID) + 
                   (1 | Classroom),
                 data=dataLEF2)

summary(Model1)
anova(Model1)
sjPlot::plot_model(Model1)


Model1.TEAF <- lmer(TEAF_Total ~ Time + 
                 (1 | Classroom:ID) + 
                 (1 | Classroom),
               data=dataLEF2)

summary(Model1.TEAF)
anova(Model1.TEAF)

#####################
#
#     Model 2: Random slopes
#
#####################

Model2 <- lmer(DESSAMini_Total ~ Time + SDQ.CLASS.MEAN + SDQ.CLASS.MEAN*Time + 
                 (Time | Classroom:ID) + 
                 (Time | Classroom),
               data=dataLEF2)

jtools::summ(Model2)
summary(Model2)
anova(Model2)

SDQ_TotalID


Model2.TEAF <- lmer(TEAF_Total ~ Time + SDQ.CLASS.MEAN + SDQ.CLASS.MEAN*Time +
                 (Time | Classroom:ID) + 
                 (Time | Classroom),
               data=dataLEF2)

summary(Model2.TEAF)
anova(Model2.TEAF)

# Comparing models #

anova(Model1, Model2)

lmerTest::anova(Model1, Model2)

stats::confint(Model1)



#####################
#
#     Model 3
#
#####################

Model3 <- lmer(DESSAMini_Total ~ Time + IRS.CLASS.SD + IRS.CLASS.MEAN + SDQ.CLASS.SD + SDQ.CLASS.MEAN + IRS.CLASS.SD*Time + IRS.CLASS.MEAN*Time + SDQ.CLASS.SD*Time + SDQ.CLASS.MEAN*Time + Gender + CCAge_Y +
                   (Time | Classroom:ID) + 
                   (Time | Classroom),
                 data=dataLEF2)

summary(Model3)
anova(Model3)



Model3.TEAF <- lmer(TEAF_Total ~ Time + IRS.CLASS.SD + IRS.CLASS.MEAN + SDQ.CLASS.SD + SDQ.CLASS.MEAN + Gender +
                 (Time | Classroom:ID) + 
                 (Time | Classroom),
               data=dataLEF2)

summary(Model3.TEAF)
anova(Model3.TEAF)

# Graph 1 #

formulas_from_lmer <- coef(Model3)$Classroom %>% select(intercept = 1, slope = 2)
formulas_from_lmer$Classroom <- row.names(formulas_from_lmer)
row.names(formulas_from_lmer) <- NULL

ggplot(dataLEF2, aes(x = Time, y = DESSAMini_Total, color = Classroom)) +
  geom_point(aes(shape = Classroom)) +
  geom_abline(data = formulas_from_lmer,
              mapping = aes(intercept = intercept, slope = slope, color = Classroom)) +
  geom_abline(aes(intercept = fixef(Model3)[[1]], slope = fixef(Model3)[[2]]),
              color = 'gray', size = 1.5, alpha = 0.6)


# Graph 2 #

subj_means_plot <- base_plot + 
  stat_summary(aes(group = Subject), fun.y = mean,  # means from raw data
               geom = "point", shape = 19, size = 4, color = "#fc8d62") +
  stat_summary(aes(group = Subject), fun.y = mean, 
               geom = "line", size = 1.2, color = "#fc8d62")
subj_means_plot



#####################
#
#     Model 4 - Model 3 with different predictor variables
#
#####################

Model4 <- lmer(DESSAMini_Total ~ Time + Dummy.PreSchool + Dummy.FDK + Dummy.SchoolAge + PROP.BOYS + Dummy.PreSchool*Time + Dummy.FDK*Time + Dummy.SchoolAge*Time + PROP.BOYS*Time + Gender +
                      (Time | Classroom:ID) + 
                      (Time | Classroom),
                    data=dataLEF2)

summary(Model4)
anova(Model4)

Model4.TEAF <- lmer(TEAF_Total ~ Time + Dummy.PreSchool + Dummy.FDK + Dummy.SchoolAge + PROP.BOYS + Gender +
                      (Time | Classroom:ID) + 
                      (Time | Classroom),
                    data=dataLEF2)

summary(Model4.TEAF)
anova(Model4.TEAF)


#####################
#
#     Model 5 - Same as model 4 but with the new date variable
#
#####################

Model5 <- lmer(DESSAMini_Total ~ diff_in_days + Dummy.PreSchool + Dummy.FDK + Dummy.SchoolAge + PROP.BOYS + Gender +
                 (diff_in_days | Classroom:ID) + 
                 (diff_in_days | Classroom),
               data=dataLEF2)

summary(Model5)
anova(Model5)

Model5.TEAF <- lmer(TEAF_Total ~ diff_in_days + Dummy.PreSchool + Dummy.FDK + Dummy.SchoolAge + PROP.BOYS + Gender +
                      (diff_in_days | Classroom:ID) + 
                      (diff_in_days | Classroom),
                    data=dataLEF2)

summary(Model5.TEAF)
anova(Model5.TEAF)

####################################################################################
####################################################################################
####################################################################################
#####
#####     August 10th, 2021
#####
####################################################################################
####################################################################################
####################################################################################

#####################
#
#     Model fit significance test & confidence intervals for random slopes
#
#####################

###   Significance test to determine change in model fit    ###

## Unconditional Growth Models

# For DESSA

FitModel1 <- lmer(DESSAMini_Total ~ SDQ_TotalD + 
                 (1 | Classroom:ID) + 
                 (1 | Classroom),
               data=dataLEF2)

summary(FitModel1)

# For TEAF

FitModel1.TEAF <- lmer(TEAF_Total ~ SDQ_TotalD + 
                      (1 | Classroom:ID) + 
                      (1 | Classroom),
                    data=dataLEF2)

summary(FitModel1.TEAF)




## Random Slopes Models

# For DESSA

FitModel2 <- lmer(DESSAMini_Total ~ SDQ_TotalD + 
                    (SDQ_TotalD | Classroom:ID) + 
                    (SDQ_TotalD | Classroom),
                  data=dataLEF2)

summary(FitModel2)

# For TEAF

FitModel2.TEAF <- lmer(TEAF_Total ~ SDQ_TotalD + 
                         (SDQ_TotalD | Classroom:ID) + 
                         (SDQ_TotalD | Classroom),
                       data=dataLEF2)

summary(FitModel2.TEAF)

## Significance test for comparison

# For dessa

anova(FitModel1, FitModel2)

# For TEAF

anova(FitModel1.TEAF, FitModel2.TEAF)

###   Confidence Intervals for Random Slopes    ###

# For dessa

stats::confint(FitModel2)

# For TEAF

stats::confint(FitModel2.TEAF)


#####################
#
#     Model 6 - RAW SDQ Model - August 10th, 2021
#
#####################

# Unconditional Growth #

Model6 <- lmer(DESSAMini_Total ~ SDQ_TotalD + 
                 (1 | Classroom:ID) + 
                 (1 | Classroom),
               data=dataLEF2)

jtools::summ(Model6)
summary(Model6)

Model6.TEAF <- lmer(DESSAMini_Total ~ SDQ_TotalD + 
                 (1 | Classroom:ID) + 
                 (1 | Classroom),
               data=dataLEF2)

jtools::summ(Model6.TEAF)
summary(Model6.TEAF)

# Random Slopes #

Model7 <- lmer(DESSAMini_Total ~ SDQ_TotalD + 
                 (SDQ_TotalD | Classroom:ID) + 
                 (SDQ_TotalD | Classroom),
               data=dataLEF2)

jtools::summ(Model7)
summary(Model7)

Model7.TEAF <- lmer(DESSAMini_Total ~ SDQ_TotalD + 
                 (SDQ_TotalD | Classroom:ID) + 
                 (SDQ_TotalD | Classroom),
               data=dataLEF2)

jtools::summ(Model7.TEAF)
summary(Model7.TEAF)

# Fixed effects #

Model8 <- lmer(DESSAMini_Total ~ SDQ_TotalD + Dummy.PreSchool + Dummy.FDK + Dummy.SchoolAge + PROP.BOYS + Gender + Time +
                 (SDQ_TotalD | Classroom:ID) + 
                 (SDQ_TotalD | Classroom),
               data=dataLEF2)

jtools::summ(Model8)
summary(Model8)

Model8.TEAF <- lmer(TEAF_Total ~ SDQ_TotalD + Dummy.PreSchool + Dummy.FDK + Dummy.SchoolAge + PROP.BOYS + Gender + Time +
                 (SDQ_TotalD | Classroom:ID) + 
                 (SDQ_TotalD | Classroom),
               data=dataLEF2)

jtools::summ(Model8.TEAF)
summary(Model8.TEAF)



#####################
#
#     Graphs
#
#####################

##

sapply(dataLEF2, class)

#
Model8 <- lmer(DESSAMini_Total ~ SDQ_TotalD + Dummy.PreSchool + Dummy.FDK + Dummy.SchoolAge + PROP.BOYS + Gender + Time +
                 (SDQ_TotalD | Classroom:ID) + 
                 (SDQ_TotalD | Classroom),
               data=dataLEF2)

dataLEF2$Classroom <- as.numeric(dataLEF2$Classroom)
#dataLEF2$DESSAMini_Total <- as.numeric(dataLEF2$DESSAMini_Total)
#dataLEF2$ID <- as.numeric(dataLEF2$ID)
#dataLEF2$SDQ_TotalD <- as.numeric(dataLEF2$SDQ_TotalD)
#dataLEF2$Dummy.PreSchool <- as.numeric(dataLEF2$Dummy.PreSchool)
#dataLEF2$Dummy.FDK <- as.numeric(dataLEF2$Dummy.FDK)
#dataLEF2$Dummy.SchoolAge <- as.numeric(dataLEF2$Dummy.SchoolAge)
#dataLEF2$PROP.BOYS <- as.numeric(dataLEF2$PROP.BOYS)

dataLEF2$Gender <- as.numeric(dataLEF2$Gender)
#dataLEF2$Time <- as.numeric(dataLEF2$Time)


## DESSA / Classroom ##

ggplot(data      = dataLEF2,
       aes(x     = Date,
           y     = DESSAMini_Total,
           col   = Classroom,
           group = Classroom))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  scale_color_gradientn(colours = rainbow(100))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  labs(title    = "Dessa Mini Score over time by classroom") +
  geom_smooth(method = 'lm', se = F, aes(group = 1), color = "black", size = 2)


## TEAF / Classroom ##

ggplot(data      = dataLEF2,
       aes(x     = Date,
           y     = TEAF_Total,
           col   = Classroom,
           group = Classroom))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  scale_color_gradientn(colours = rainbow(100))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  labs(title    = "TEAF Score over time by classroom") +
  geom_smooth(method = 'lm', se = F, aes(group = 1), color = "black", size = 2)
  


dataLEF2$fit <- predict(Model8)

ggplot(dataLEF2,aes(Time, DESSAMini_Total, group=interaction(Classroom), col=Classroom)) + 
  facet_grid(~Classroom) +
  geom_line(aes(y=fit), size=0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw()



