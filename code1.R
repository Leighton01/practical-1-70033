# GOAL: precisely predicting sales

library(tidyverse)
library(GGally)
library(ggplot2)
library(stringr)
library(dplyr)
library(glmtoolbox)
library(corrplot)
# dataW: original data; dataN: only numerical; dataT: transformed data

dataW <- read_csv("ex1_walmart_sale.csv")

ls_hol <- as.Date(c("12-Feb-2010", "11-Feb-2011", "10-Feb-2012", "8-Feb-2013",
            "10-Sep-2010", "9-Sep-2011", "7-Sep-2012", "6-Sep-2013",
            "26-Nov-2010", "25-Nov-2011", "23-Nov-2012", "29-Nov-2013",
            "31-Dec-2010", "30-Dec-2011", "28-Dec-2012", "27-Dec-2013")
            ,"%d-%b-%Y")
ls_holwk <- as.integer(strftime(ls_hol, format = "%V"))

######################################## TRANSOFMRATIONS
dataT <- dataW

dataT$ID <- 1:nrow(dataT)
dataT$Year <- as.integer(str_sub(dataT$Date, 7, 10))
dataT$Week <- as.integer(strftime(dataT$Date, format = "%V"))
dataT$Date <- as.Date(dataT$Date, "%d-%m-%Y")
dataT <- mutate(dataT, Store = as.factor(Store))

dataT <- mutate(dataT, toHoli = NA)
dataT <- mutate(dataT, aftHoli = NA)
dataT <- mutate(dataT, near_Hol = NA)

for (i in 1:nrow(dataT)){
  dataT$wk_toHoli[i] <- as.integer(pre_wk_diff(dataT$Week[i], ls_holwk))
  dataT$wk_aftHoli[i] <- as.integer(post_wk_diff(dataT$Week[i], ls_holwk))

  # if (abs(dataT$wk_aftHoli[i]) < 2 | abs(dataT$wk_toHoli[i]) < 1){
  if (abs(dataT$wk_toHoli[i]) < 1){
    dataT$near_Hol[i] <- 1
  } else {
    dataT$near_Hol[i] <- 0}

  if (abs(dataT$Holiday_Flag[i]) == 1){
    dataT$near_Hol[i] <- 2
    }
}

dataT$near_Hol <- as.factor(dataT$near_Hol)

dataT <- mutate(group_by(dataT, Store),
                SalesStM = scale(Weekly_Sales, center = min(Weekly_Sales),
                                scale = max(Weekly_Sales)-min(Weekly_Sales)))


##################################Check correlation


dataCor <- subset(dataT,select = c(Weekly_Sales, SalesStM, Temperature,
                                   Fuel_Price,CPI, Unemployment,
                                   Year, Week))

plot.new()
gCor <- function() corrplot(cor(dataCor),
                 method = "ellipse",
                 type = "upper",
                 tl.col = "black",
                 tl.cex = 1,
                 addCoef.col = "black",
                 addCoefasPercent = TRUE)
gCor()

##################################Split data and model
# use 70% of dataset as training set and 30% as test set
train <- dataT %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(dataT, train, by = 'ID')


# GLM models

# All var + interaction
# fitG1 <- glm(Weekly_Sales ~ Store + Temperature * Unemployment *
#                Fuel_Price * near_Hol * CPI
#              , "gaussian", train)
# summary(fitG1)
# adjR2(fitG1)
#
#
# # Filter 1
# fitG2 <- glm(Weekly_Sales ~ Store + Temperature + Unemployment +
#              Fuel_Price + near_Hol + Temperature:Unemployment +
#              Unemployment:Fuel_Price + Temperature:Fuel_Price +
#              Temperature:CPI + CPI:Fuel_Price + Unemployment:CPI +
#              near_Hol:CPI + near_Hol:Temperature + near_Hol:Unemployment+
#              near_Hol:Fuel_Price +
#              , "gaussian", train)
#
#
# summary(fitG2)
# adjR2(fitG2)

fitG3 <- glm(Weekly_Sales ~ Store + Fuel_Price + Temperature:Unemployment+
               Fuel_Price:CPI + near_Hol:CPI
             , "gaussian", train)

summary(fitG3)
adjR2(fitG3)

predG3 <- predict(fitG3, newdata = test, type = "response")
predR2 <- cor(predG3, test$Weekly_Sales)^2


resG3 <- function() plot(fitG3$residuals)
resG3()

# yea maybe dont' show this box plot.. very similar trend :(

# ggplot(data = dataT, aes(x = near_Hol, y = Weekly_Sales))+
#   geom_boxplot()

boxhol <- ggplot(data = dataT, aes(x = near_Hol, y = SalesStM))+
  geom_boxplot()
# ggplot(data = dataT, aes(x = near_Hol, y = SalesStN))+
#   geom_boxplot()



# mean(dataT$Weekly_Sales[dataT$Holiday_Flag==1])
# mean(dataT$Weekly_Sales[dataT$Holiday_Flag==0])
#
# mean(dataT$SalesSt[dataT$Holiday_Flag==1])
# mean(dataT$SalesSt[dataT$Holiday_Flag==0])
#
# mean(dataT$Weekly_Sales[dataT$near_Hol == 2])
# mean(dataT$Weekly_Sales[dataT$near_Hol == 1])
# mean(dataT$Weekly_Sales[dataT$near_Hol == 0])
