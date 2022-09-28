library(readxl)
library(ggplot2)
library(tidyverse)
library(emmeans)
library(MuMIn)

bats <- read_excel("Chris 2021-10-28 2009-2019 Data_2022-02_10 Perm Filters_Edited_Fixed_27Jun2022.xlsx", 
                   sheet = "Main Data Block_Edited")

BMRvBM.lm <- lm(O2ConsRateLowestBMR~AvgBodyMass,bats)

bats%>%
  ggplot(aes(x=AvgBodyMass,y=O2ConsRateLowestBMR,col=Colony))+geom_point()+geom_smooth(method="lm")

bats%>%
  ggplot(aes(x=AvgBodyMass,y=O2ConsRateLowestBMR,col=Colony))+geom_point()+geom_smooth()


bats %>% 
  ggplot(aes(Colony,AvgBodyMass))+geom_boxplot()

#group by P0+P1 and P2+P3, WS1 and WS2, colony

emmeans(XXX)


bats2 <- bats%>%
  mutate(logO2=log(O2ConsRateLowestBMR),logBM=log(AvgBodyMass))%>%
  select(logO2,Colony,logBM,WingScore,ReproStage2,AvgWbcMl)%>%
  na.omit()
  

bats2%>%
  ggplot(aes(logBM,logO2,col=Colony))+geom_point()+geom_smooth(method="lm")


#linear model

fit1 <- lm(logO2~logBM*Colony,bats)

fit2 <- lm(logO2~Colony,bats)
anova(fit2)

options(na.action="na.fail")
big.fit <- glm(logO2~logBM*WingScore*ReproStage2*AvgWbcMl,data=bats2)


big.dr <- big.fit%>%
  dredge()

get.models(big.dr,subset = weight>.1)