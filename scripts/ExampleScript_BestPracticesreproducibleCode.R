
#### EXAMPLE: DESCRIPTIVE ANALYSIS BEHAVIOR - PHYSICAL SCORE ####

#### Clear environment
rm(list = ls())

### change 1
### change 2


#### Working directory, packages, load data and define variables 

setwd("C:/Users/6259464/OneDrive - Universiteit Utrecht/Courses/BPRC")

Behavior_data <- read.csv("C:/Users/6259464/OneDrive - Universiteit Utrecht/Courses
                       /BPRC/ExampleData_BestPracticesreproducibleCode.csv", 
                       header=TRUE, sep=";", dec=",")
#View(Behavior_data)
str(Behavior_data)

library(ggplot2)
library(gridExtra)
library(pastecs)
library(Hmisc)
library(lintr)
lint("C:/Users/6259464/OneDrive - Universiteit Utrecht/Courses/BPRC/
     ExampleScript_BestPracticesreproducibleCode.R")


#?as.POSIXct
Behavior_data$Round <- as.factor(Behavior_data$Round)
Behavior_data$Day <- as.factor(Behavior_data$Day)
Behavior_data$LocationPen <- as.factor(Behavior_data$LocationPen)
Behavior_data$RoundPen <- as.factor(Behavior_data$RoundPen)
Behavior_data$EarID <- as.factor(Behavior_data$EarID)
Behavior_data$EarIDShort <- as.factor(Behavior_data$EarIDShort)
Behavior_data$SubjectID <- as.factor(Behavior_data$SubjectID)
Behavior_data$Physical <- as.ordered(Behavior_data$Physical)
str(Behavior_data)
attach(Behavior_data)


Behavior_data$Physical <- as.character(Behavior_data$Physical)
Behavior_data$Physical[Behavior_data$Physical == '1'] <- '0'
Behavior_data$Physical[Behavior_data$Physical == '2'] <- '0'
Behavior_data$Physical[Behavior_data$Physical == '3'] <- '1'
Behavior_data$Physical[Behavior_data$Physical == '4'] <- '1'
Behavior_data$Physical[Behavior_data$Physical == '5'] <- '1'
Behavior_data$Physical <- as.factor(Behavior_data$Physical)
str(Behavior_data)
#View(Behavior_data)

#### 1. Outliers ####

#Binomial data no outliers, check deviance residuals with final model

#plot(fitted.values(fit),residuals(fit, type = "deviance"))# Higher or lower than 2/-2 outlier --> see modern methods
#OR
#plot(predicted(fit),residuals(fit, type = "deviance"))

## Behavior2 ##

boxplot(Behavior_data$Behavior2Min1)
boxplot(Behavior_data$Behavior2Min2)

PhysicalBehavior2Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                             aes(x=Physical, y = Behavior2Min1, fill = Physical)) + geom_boxplot()
PhysicalBehavior2Min1# no apparent outliers

PhysicalBehavior2Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                             aes(x=Physical, y = Behavior2Min2, fill = Physical)) + geom_boxplot()
PhysicalBehavior2Min2# no apparent outliers

ClevelandBehavior2Min1 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior2Min1,x=seq(1,length(Behavior2Min1),1))) 
ClevelandBehavior2Min1# no apparent outliers

ClevelandBehavior2Min2 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior2Min2,x=seq(1,length(Behavior2Min2),1))) 
ClevelandBehavior2Min2# no apparent outliers

## Behavior1 ##

boxplot(Behavior_data$Behavior1Min1)
boxplot(Behavior_data$Behavior1Min2)

PhysicalBehavior1Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                               aes(x=Physical, y = Behavior1Min1, fill = Physical)) + geom_boxplot()
PhysicalBehavior1Min1# outliers?

PhysicalBehavior1Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                               aes(x=Physical, y = Behavior1Min2, fill = Physical)) + geom_boxplot()
PhysicalBehavior1Min2# outliers?

ClevelandBehavior1Min1 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior1Min1,x=seq(1,length(Behavior1Min1),1))) 
ClevelandBehavior1Min1# no apparent outliers

ClevelandBehavior1Min2 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior1Min2,x=seq(1,length(Behavior1Min2),1))) 
ClevelandBehavior1Min2# no apparent outliers

## Behavior5 ##

boxplot(Behavior_data$Behavior5Min1)
boxplot(Behavior_data$Behavior5Min2)

PhysicalBehavior5Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                              aes(x=Physical, y = Behavior5Min1, fill = Physical)) + geom_boxplot()
PhysicalBehavior5Min1# no apparent outliers

PhysicalBehavior5Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                              aes(x=Physical, y = Behavior5Min2, fill = Physical)) + geom_boxplot()
PhysicalBehavior5Min2# outliers?

ClevelandBehavior5Min1 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior5Min1,x=seq(1,length(Behavior5Min1),1))) 
ClevelandBehavior5Min1# no apparent outliers

ClevelandBehavior5Min2 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior5Min2,x=seq(1,length(Behavior5Min2),1))) 
ClevelandBehavior5Min2# no apparent outliers

## Behavior4 ##

boxplot(Behavior_data$Behavior4Min1)
boxplot(Behavior_data$Behavior4Min2)

PhysicalBehavior4Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                               aes(x=Physical, y = Behavior4Min1, fill = Physical)) + geom_boxplot()
PhysicalBehavior4Min1# no apparent outliers

PhysicalBehavior4Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                               aes(x=Physical, y = Behavior4Min2, fill = Physical)) + geom_boxplot()
PhysicalBehavior4Min2# no apparent outliers

ClevelandBehavior4Min1 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior4Min1,x=seq(1,length(Behavior4Min1),1))) 
ClevelandBehavior4Min1# no apparent outliers

ClevelandBehavior4Min2 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior4Min2,x=seq(1,length(Behavior4Min2),1))) 
ClevelandBehavior4Min2# no apparent outliers

## Behavior3 ##

boxplot(Behavior_data$Behavior3Min1)
boxplot(Behavior_data$Behavior3Min2)

PhysicalBehavior3Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                            aes(x=Physical, y = Behavior3Min1, fill = Physical)) + geom_boxplot()
PhysicalBehavior3Min1# no apparent outliers

PhysicalBehavior3Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                            aes(x=Physical, y = Behavior3Min2, fill = Physical)) + geom_boxplot()
PhysicalBehavior3Min2# no apparent outliers

ClevelandBehavior3Min1 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior3Min1,x=seq(1,length(Behavior3Min1),1))) 
ClevelandBehavior3Min1# no apparent outliers

ClevelandBehavior3Min2 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior3Min2,x=seq(1,length(Behavior3Min2),1))) 
ClevelandBehavior3Min2# outliers?


grid.arrange(ClevelandBehavior2Min1, ClevelandBehavior2Min2, ClevelandBehavior1Min1, ClevelandBehavior1Min2)
grid.arrange(ClevelandBehavior5Min1, ClevelandBehavior5Min2, ClevelandBehavior4Min1, ClevelandBehavior4Min2, ClevelandBehavior3Min1, ClevelandBehavior3Min2)

## Behavior6 ##

boxplot(Behavior_data$Behavior6Min1)
boxplot(Behavior_data$Behavior6Min2)

PhysicalBehavior6Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                            aes(x=Physical, y = Behavior6Min1, fill = Physical)) + geom_boxplot()
PhysicalBehavior6Min1# no apparent outliers

PhysicalBehavior6Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), 
                            aes(x=Physical, y = Behavior6Min2, fill = Physical)) + geom_boxplot()
PhysicalBehavior6Min2# outliers?

ClevelandBehavior6Min1 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior6Min1,x=seq(1,length(Behavior6Min1),1))) 
ClevelandBehavior6Min1# no apparent outliers

ClevelandBehavior6Min2 <- ggplot(Behavior_data) + geom_point(aes(y=Behavior6Min2,x=seq(1,length(Behavior6Min2),1))) 
ClevelandBehavior6Min2# no apparent outliers

#### 2. Homogeneity ####

#Binomial data --> Check with residuals of models during backward stepwise regression
#residuals(fit)
#fitted.values(fit)
#plot(fitted.values(fit),residuals(fit))


#### Behaviors (not response) ####

## Behavior3 (as Y)##

PlotPigPhysicalBehavior3Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior3Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalBehavior3Min1#Similar variance across fecal scores

PlotPigPhysicalRoundBehavior3Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior3Min1, x = Round)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalRoundBehavior3Min1#Similar variance across fecal scores and rounds

PlotPigPhysicalDayBehavior3Min1 <- ggplot(data=Behavior_data, aes(y = Behavior3Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior3Min1#Unequal variances across fecal scores across pens within rounds?


PlotPigPhysicalDayBehavior3Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior3Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior3Min2#Similar variances across fecal scores

PlotPigPhysicalDayBehavior3Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior3Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior3Min2#Similar variances across fecal scores and rounds

PlotPigPhysicalDayBehavior3Min2 <- ggplot(data=Behavior_data, aes(y = Behavior3Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior3Min2#Similar variances across fecal scores across pens, within rounds
#Unequal across rounds

## Behavior4 (as Y) ##

PlotPigPhysicalDayBehavior4Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior4Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min1#Similar variances across fecal scores

PlotPigPhysicalDayBehavior4Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior4Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min1#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior4Min1 <- ggplot(data=Behavior_data, aes(y = Behavior4Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min1#Unequal variances across fecal scores across pens within rounds?


PlotPigPhysicalDayBehavior4Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior4Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min2#Similar variances across fecal scores

PlotPigPhysicalDayBehavior4Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior4Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min2#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior4Min2 <- ggplot(data=Behavior_data, aes(y = Behavior4Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min2#Similar variances across fecal scores across pens, within rounds
#Unequal across rounds

## Behavior5 (as Y) ##

PlotPigPhysicalDayBehavior5Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior5Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min1#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior5Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior5Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min1#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior5Min1 <- ggplot(data=Behavior_data, aes(y = Behavior5Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min1#Similar variances across fecal scores across pens, within rounds
#Unequal across rounds


PlotPigPhysicalDayBehavior5Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior5Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min2#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior5Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior5Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min2#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior5Min2 <- ggplot(data=Behavior_data, aes(y = Behavior5Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min2#Unequal variances across fecal scores across pens within rounds?

## Behavior2 (as Y) ##

PlotPigPhysicalDayBehavior2Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior2Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior2 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior2Min1#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior2Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior2Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior2 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior2Min1#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior2Min1 <- ggplot(data=Behavior_data, aes(y = Behavior2Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior2 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior2Min1#Similar variances across fecal scores across pens, within ans across rounds


PlotPigPhysicalDayBehavior2Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior2Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior2 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior2Min2#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior2Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior2Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior2 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior2Min2#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior2Min2 <- ggplot(data=Behavior_data, aes(y = Behavior2Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior2 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior2Min2#Unequal variances across fecal scores across pens, within and across rounds

## Behavior1 (as Y) ##

PlotPigPhysicalDayBehavior1Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior1Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min1#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior1Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior1Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min1#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior1Min1 <- ggplot(data=Behavior_data, aes(y = Behavior1Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min1#Unequal variances across fecal scores across pens, within and across rounds


PlotPigPhysicalDayBehavior1Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior1Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min2#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior1Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior1Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min2#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior1Min2 <- ggplot(data=Behavior_data, aes(y = Behavior1Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min2#Unequal variances across fecal scores across pens, within and across rounds

## Behavior6 (as Y) ##

PlotPigPhysicalDayBehavior6Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior6Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min1#Similar variances across fecal scores

PlotPigPhysicalDayBehavior6Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior6Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min1#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior6Min1 <- ggplot(data=Behavior_data, aes(y = Behavior6Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min1#Similar variances across fecal scores across pens, within and across rounds


PlotPigPhysicalDayBehavior6Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior6Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min2#Similar variances across fecal scores

PlotPigPhysicalDayBehavior6Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior6Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min2#Similar variances across fecal scores across rounds

PlotPigPhysicalDayBehavior6Min2 <- ggplot(data=Behavior_data, aes(y = Behavior6Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min2#Similar variances across fecal scores across pens, within and across rounds



#### 3. Distribution ####

#Fecals scores binomial distribution

#### Behaviors (not response) ####

## Behavior1 ## 

hist(Behavior_data$Behavior1Min1)#Poisson, right skewed
qqnorm(Behavior_data$Behavior1Min1)# non-normally distributed residuals, right-skewed
abline(mean(Behavior_data$Behavior1Min1),sd(Behavior_data$Behavior1Min1))

hist(Behavior_data$Behavior1Min2)#Poisson, right skewed
qqnorm(Behavior_data$Behavior1Min2)# non-normally distributed residuals, right-skewed
abline(mean(Behavior_data$Behavior1Min2),sd(Behavior_data$Behavior1Min2))

## Behavior2 ##

hist(Behavior_data$Behavior2Min1)#Poisson, right skewed
qqnorm(Behavior_data$Behavior2Min1)# residuals ok, slightly right-skewed
abline(mean(Behavior_data$Behavior2Min1),sd(Behavior_data$Behavior2Min1))

hist(Behavior_data$Behavior2Min2)#Poisson
qqnorm(Behavior_data$Behavior2Min2)# non-normally distributed residuals, right-skewed
abline(mean(Behavior_data$Behavior2Min2),sd(Behavior_data$Behavior2Min2))

## Behavior3 ##

hist(Behavior_data$Behavior3Min1)#close to Normal, slightly left skewed
qqnorm(Behavior_data$Behavior3Min1)# residuals ok, slightly left-skewed
abline(mean(Behavior_data$Behavior3Min1),sd(Behavior_data$Behavior3Min1))

hist(Behavior_data$Behavior3Min2)#close to Normal, slightly left skewed
qqnorm(Behavior_data$Behavior3Min2)# residuals ok, slightly left-skewed
abline(mean(Behavior_data$Behavior3Min2),sd(Behavior_data$Behavior3Min2))

## Behavior5 ##

hist(Behavior_data$Behavior5Min1)#Poisson, right skewed
qqnorm(Behavior_data$Behavior5Min1)# non-normally distributed residuals, right skewed
abline(mean(Behavior_data$Behavior5Min1),sd(Behavior_data$Behavior5Min1))

hist(Behavior_data$Behavior5Min2)#Poisson, right skewed
qqnorm(Behavior_data$Behavior5Min2)# non-normally distributed residuals, right skewed
abline(mean(Behavior_data$Behavior5Min2),sd(Behavior_data$Behavior5Min2))

## Behavior4 ##

hist(Behavior_data$Behavior4Min1)#close to Normal
qqnorm(Behavior_data$Behavior4Min1)# residuals ok, light tailed or right skewed
abline(mean(Behavior_data$Behavior4Min1),sd(Behavior_data$Behavior4Min1))

hist(Behavior_data$Behavior4Min2)#close to Normal
qqnorm(Behavior_data$Behavior4Min2)# residuals ok, light tailed or right skewed
abline(mean(Behavior_data$Behavior4Min2),sd(Behavior_data$Behavior4Min2))

## Behavior6 ##

hist(Behavior_data$Behavior6Min1)#Poisson
qqnorm(Behavior_data$Behavior6Min1)# non-normally distributed residuals, right skewed
abline(mean(Behavior_data$Behavior6Min1),sd(Behavior_data$Behavior6Min1))

hist(Behavior_data$Behavior6Min2)#Poisson
qqnorm(Behavior_data$Behavior6Min2)# non-normally distributed residuals, right skewed
abline(mean(Behavior_data$Behavior6Min2),sd(Behavior_data$Behavior6Min2))



#### 4. Zero inflation ####

#Not relevant for binomial data

#### 5. Colinearity ####

## Visualization ##


PlotPigPhysicalDayBehavior3Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior3Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior3Min1

PlotPigPhysicalDayBehavior3Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior3Min1, x = Round)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior3Min1

PlotPigPhysicalDayBehavior3Min1 <- ggplot(data=Behavior_data, aes(y = Behavior3Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior3Min1


PlotPigPhysicalDayBehavior3Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior3Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior3Min2

PlotPigPhysicalDayBehavior3Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior3Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior3Min2

PlotPigPhysicalDayBehavior3Min2 <- ggplot(data=Behavior_data, aes(y = Behavior3Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior3 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior3Min2



PlotPigPhysicalDayBehavior4Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior4Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min1

PlotPigPhysicalDayBehavior4Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior4Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min1

PlotPigPhysicalDayBehavior4Min1 <- ggplot(data=Behavior_data, aes(y = Behavior4Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min1


PlotPigPhysicalDayBehavior4Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior4Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min2

PlotPigPhysicalDayBehavior4Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior4Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min2

PlotPigPhysicalDayBehavior4Min2 <- ggplot(data=Behavior_data, aes(y = Behavior4Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior4 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior4Min2



PlotPigPhysicalDayBehavior5Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior5Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min1

PlotPigPhysicalDayBehavior5Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior5Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min1

PlotPigPhysicalDayBehavior5Min1 <- ggplot(data=Behavior_data, aes(y = Behavior5Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min1


PlotPigPhysicalDayBehavior5Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior5Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min2

PlotPigPhysicalDayBehavior5Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior5Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min2

PlotPigPhysicalDayBehavior5Min2 <- ggplot(data=Behavior_data, aes(y = Behavior5Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior5 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior5Min2



PlotPigPhysicalDayBehavior2Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior2Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior2 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior2Min1

PlotPigPhysicalDayBehavior2Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior2Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior2 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior2Min1

PlotPigPhysicalDayBehavior2Min1 <- ggplot(data=Behavior_data, aes(y = Behavior2Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior2 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior2Min1


PlotPigPhysicalDayBehavior2Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior2Min2, x = Round, fill = Physical)) +
  xlab("Round") + ylab("Percentage Behavior2 (-2 days)") + 
  geom_boxplot(alpha = 0.2, show.legend = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels=scales::percent) +
  guides(fill=guide_legend(title="Physical")) + 
  scale_fill_manual(values = c("green", "red")) +
  ggtitle("Percentage Behavior2 min2 physical score per round") +
  theme(plot.title = element_text(hjust = 0.50))
PlotPigPhysicalDayBehavior2Min2

PlotPigPhysicalDayBehavior2Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior2Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior2 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior2Min2


PlotPigPhysicalDayBehavior1Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior1Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min1

PlotPigPhysicalDayBehavior1Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior1Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min1

PlotPigPhysicalDayBehavior1Min1 <- ggplot(data=Behavior_data, aes(y = Behavior1Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min1


PlotPigPhysicalDayBehavior1Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior1Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min2

PlotPigPhysicalDayBehavior1Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior1Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min2

PlotPigPhysicalDayBehavior1Min2 <- ggplot(data=Behavior_data, aes(y = Behavior1Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior1 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior1Min2



PlotPigPhysicalDayBehavior6Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior6Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min1

PlotPigPhysicalDayBehavior6Min1 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior6Min1, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min1

PlotPigPhysicalDayBehavior6Min1 <- ggplot(data=Behavior_data, aes(y = Behavior6Min1, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-1 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min1


PlotPigPhysicalDayBehavior6Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior6Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Physical), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min2

PlotPigPhysicalDayBehavior6Min2 <- ggplot(data=subset(Behavior_data, !is.na(Physical)), aes(y = Behavior6Min2, x = Physical)) +
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Round), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min2

PlotPigPhysicalDayBehavior6Min2 <- ggplot(data=Behavior_data, aes(y = Behavior6Min2, x = Physical, fill = Pen)) +
  geom_point(stat="identity", position = position_dodge(width = 0.75), size = 0.4, aes(color = Pen)) + 
  xlab("PhysicalScore") + ylab("Proportion Behavior6 (-2 days)") + 
  geom_boxplot(alpha = 0.2, aes(fill = Pen), show.legend = TRUE) +
  facet_wrap(Behavior_data$Round) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PlotPigPhysicalDayBehavior6Min2

