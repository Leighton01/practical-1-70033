library(tidyverse)
library(GGally)
library(ggplot2)
library(stringr)
library(dplyr)

# rm(list = ls())

####################################### Exploration & Summary :)
sum <- summary(dataT)

# Let's look @ matrix plot w/o factors
plotM <- ggpairs(dataT2[c("SalesSt","Temperature", "CPI", "Fuel_Price",
                         "Unemployment", "Year", "Week", "Holiday_Flag")])
plotM




ggpairs(dataT3[c("SalesSt","TempSt", "CPISt", "FuelSt",
                          "UnempSt")])




ggplot(dataT, aes(x = Week, y = Weekly_Sales)) +
  geom_smooth(se=TRUE)

ggplot(dataT, aes(x = Week, y = Weekly_Sales, color = Store, group = Store)) +
  geom_smooth(se=FALSE)



ggplot(dataT, aes(x = Temperature, y = Weekly_Sales)) +
  geom_smooth(se=FALSE)
ggplot(dataT, aes(x = CPI, y = Weekly_Sales)) +
  geom_smooth(se=FALSE)
ggplot(dataT, aes(x = Unemployment, y = Weekly_Sales)) +
  geom_smooth(se=FALSE)
ggplot(dataT, aes(x = Fuel_Price, y = Weekly_Sales)) +
  geom_smooth(se=FALSE)


# boxplot or histo..
par(mfrow=c(2,2))
hist(dataT$Temperature)
hist(dataT$Fuel_Price)
hist(dataT$Unemployment)
hist(dataT$CPI)





