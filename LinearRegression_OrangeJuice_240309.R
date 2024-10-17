# Title : LinearRegression_OrangeJuice
# Problem Statement: To evaluate sale performances for orange juice brands at 3 different price points
# By: mikportfolio, 240309 1220PM


# Install packages: tidyverse, dplyr
install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)

#  Import data set "oj"
Orange_Juice <- read.csv("D:\\2. Analytics Data Sets\\BDS Datasets\\oj.csv")
View(Orange_Juice)

#  Level brands
Orange_Juice$brand <- factor(Orange_Juice$brand)
levels(Orange_Juice$brand)
head(Orange_Juice)

#  Inspect all prices by brand
summarise(Orange_Juice %>% group_by(brand), 
          avg_price = mean(price))

#  Assign colours by brand
brandcol <- c("yellow","blue","red") #yellow, blue, and red from cheapest to most expensive


################
## Study 01   ##
################

#  Plot log price vs log sales
par(mfrow=c(1,2)) #  Create frame for visualizations - one row, two columns
plot(log(price) ~ brand, data=Orange_Juice, col=brandcol)
plot(log(sales) ~ log(price), data=Orange_Juice, col=brandcol[Orange_Juice$brand])


################
## Study 02   ##
################

# Create generalized linear model using glm function
reg = glm(log(sales) ~ log(price) + brand, data=Orange_Juice)
summary(reg)       ## coeff, tests, fit
coef(reg)          ## just coefficients

reg_interact <- glm(log(sales) ~ log(price)*brand, data = Orange_Juice)
coef(reg_interact)

beta <- coef(reg)
beta

################
## Study 03   ##
################

# Same price sensitivity - plot GLM from Study 2
plot(log(sales) ~ log(price), 
     data=Orange_Juice, 
     col=brandcol[Orange_Juice$brand], 
     cex=.5, 
     pch=20, 
     bty="n")
abline(a=beta[1], 
       b=beta[2], 
       col=brandcol[1], 
       lwd=2)
abline(a=beta[1]+ beta[3], 
       b=beta[2], 
       col=brandcol[2], 
       lwd=2)
abline(a=beta[1]+beta[4], 
       b=beta[2], 
       col=brandcol[3], 
       lwd=2)
legend("bottomleft", 
       bty="n", 
       lwd=2, 
       col=brandcol, 
       legend=levels(Orange_Juice$brand))


################
## Study 04   ##
################

# Price Sensitivity Interacts with Brand Indicator
reg_interact <- glm(log(sales) ~ log(price)*brand, data = Orange_Juice)
coef(reg_interact)
beta <- coef(reg_interact)
beta

plot(log(sales) ~ log(price), 
     data=Orange_Juice, 
     col=brandcol[Orange_Juice$brand], 
     cex=.1, 
     pch=20, 
     bty="n")
abline(a=beta[1], 
       b=beta[2], 
       col=brandcol[1], 
       lwd=2)
abline(a=beta[1]+beta[3], 
       b=beta[2]+beta[5], 
       col=brandcol[2], 
       lwd=2)
abline(a=beta[1]+beta[4], 
       b=beta[2]+beta[6], 
       col=brandcol[3], 
       lwd=2)
legend("bottomleft", 
       bty="n", 
       lwd=2, 
       col=brandcol, 
       legend=levels(Orange_Juice$brand))

################
## Study 05   ##
################

# Price Sensitivity Interacts with Brand Indicator and Feat
ojreg <- glm(log(sales) ~ log(price)*brand*feat, data=Orange_Juice)
coef(ojreg)
beta <- coef(ojreg)

# Without factoring feat 
plot(log(sales) ~ log(price), 
     data=Orange_Juice, 
     col=brandcol[Orange_Juice$brand], 
     cex=.1, 
     pch=20, 
     bty="n")
abline(a=beta[1], 
       b=beta[2],
       col=brandcol[1], 
       lwd=2) #Dominick's
abline(a=beta[1]+beta[3], 
       b=beta[2]+beta[6], 
       col=brandcol[2], 
       lwd=2) # Minute Maid
abline(a=beta[1]+beta[4], 
       b=beta[2]+beta[7], 
       col=brandcol[3], 
       lwd=2) # Tropicana
legend("bottomleft", 
       bty="n", 
       lwd=2, 
       col=brandcol, 
       legend=levels(Orange_Juice$brand))


# With factoring feat
plot(log(sales) ~ log(price), 
     data=Orange_Juice, 
     col=brandcol[Orange_Juice$brand], 
     cex=.1, 
     pch=20, 
     bty="n")
abline(a=beta[1], 
       b=beta[2] + beta[8],
       col=brandcol[1], 
       lwd=2) #Dominick's
abline(a=beta[1]+beta[3], 
       b=beta[2]+beta[6] + beta[8] + beta[11], 
       col=brandcol[2], 
       lwd=2) # Minute Maid
abline(a=beta[1]+beta[4], 
       b=beta[2]+beta[7] + beta[8]+ beta[12], 
       col=brandcol[3], 
       lwd=2) # Tropicana
legend("bottomleft", 
       bty="n", 
       lwd=2, 
       col=brandcol, 
       legend=levels(Orange_Juice$brand))

################
## Study 06   ##
################

# Elasticity Table
b <- coef(ojreg)
b["log(price)"] 
b["log(price)"] + b["log(price):brandminute.maid"]
b["log(price)"] + b["log(price):brandtropicana"]
b["log(price)"] + b["log(price):feat"]
b["log(price)"] + b["log(price):brandminute.maid"] + b["log(price):feat"] + b["log(price):brandminute.maid:feat"]
b["log(price)"] + b["log(price):brandtropicana"] + b["log(price):feat"] + b["log(price):brandtropicana:feat"]

salestable <- tapply(Orange_Juice$sales, Orange_Juice[,c("feat","brand")], sum)
mosaicplot(salestable,col=brandcol)
