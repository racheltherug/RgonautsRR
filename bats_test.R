library(readxl)
library(ggplot2)
library(tidyverse)
library(emmeans)

bats <- read_excel("C:/Users/rugge/Desktop/Richardson Bat Lab/Chris 2021-10-28 2009-2019 Data_2022-02_10 Perm Filters_Edited_Fixed_27Jun2022.xlsx", 
                   sheet = "Main Data Block_Edited")

BMRvBM.lm <- lm(O2ConsRateLowestBMR~AvgBodyMass,bats)

bats%>%
  ggplot(aes(x=AvgBodyMass,y=O2ConsRateLowestBMR,col=Colony))+geom_point()

bats%>%
  ggplot(aes(x=AvgBodyMass,y=O2ConsRateLowestBMR,col=Colony))+geom_point()+geom_smooth()


#group by P0+P1 and P2+P3, WS1 and WS2, colony

emmeans(XXX)
