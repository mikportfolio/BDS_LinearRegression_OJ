---
title: "Orange Juice Regression Model"
author: "Ahmad Mudrik"
date: "2024-06-03"
output:
  pdf_document: default
  html_document: default
---

# Orange Juice Regression Model

## Introduction
Data set showcases orange juice sales from Dominick's grocery stores. The data includes weekly prices, sales, and feat (Boolean indicator if brand was advertised in store or flyer) for 3 brands of orange juice: Tropicana, Minute Maid and Dominick's. This study aim to to evaluate the sale performances for orange juice brands at 3 different price points (cheap, mid, and expensive)

## Hypothesis 
1. Customers are more prone to buy mid-priced orange juice than the cheap cheap and expensive orange juices. 
2. The price of the product affects customer's purchasing behavior.

## Objective 
1. To evaluate prices of every brands using a scatter plot
plot a regression model of orange juice sales for 3 brands of orange juice


## Install packages, import and inspect data set

### Import Dataset

```{r}
Orange_Juice <- read.csv("D:\\2. Analytics Data Sets\\BDS Datasets\\oj.csv")
head(Orange_Juice)
```

- The data set includes weekly prices for 3 orange juice brands: Tropicana, Minute Maid, and Dominick's.

### Level brands

```{r}
Orange_Juice$brand <- factor(Orange_Juice$brand)
levels(Orange_Juice$brand)
head(Orange_Juice)
brandcol <- c("yellow","blue","red")
```

- Each brand occupies a price range: Dominick's (cheap), Minute Maid (mid), and Tropicana (expensive).
- Relationship is defined by assigning colors for each brands
- yellow, blue, and red from cheapest to most expensive


## Study 1: Brand Prices
```{r}
par(mfrow=c(1,2))
plot(log(price) ~ brand, data=Orange_Juice, col=brandcol)
plot(log(sales) ~ log(price), data=Orange_Juice, col=brandcol[Orange_Juice$brand])
```

### Findings from Study 1
- Each brand occupies a well-defined range
- Dominick's a cheap orange juice option, Minute Maid is mid, and Tropicana is the expensive option.
- log sales has a linear relationship with log price.
- Charging more means selling less. 


## Study 2: Regression Model
```{r}
reg <- glm(log(sales) ~ log(price) + brand, data=Orange_Juice)
summary(reg)       
coef(reg)          

beta <- coef(reg)
beta
```

### Findings from Study 2
**glm** (generalized linear model) was used, and log(sales) vs. log(price) was plotted, brand were assigned colors
-   used **model.matrix** to create model matrix that defines the numeric inputs x
-   used summary(reg) to access coefficient, test and fit
-   used coef(reg) to obtain coefficients
-   beta = -3.13 for the log price effect
-   sales drop by about 3.13% for every 1% price hike across all brands



## Study 3: Same Price Sensitivity

```{r}
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
```

### Findings from Study 3
-   3 lines shifted according to brand identity
-   At the same price, Tropicana (expensive) sells more than Minute Maid (mid), and Minute maid sells more than Dominick's (cheap)
-   This makes sense: Tropicana is a luxury product that is preferable at the same price.
-   All lines have the same slope
-   In economic terms, the model assumes consumers of the three bands have the same price sensitivity. 
- This is unrealistic; money is less of an issue for Tropicana consumers than it is for Dominick's consumer.
-   Next is to build this information into Study 4 by having **log(price)** interact with **brand**

## Study 4:: Price Sensitivity Interacts with Brand Indicator

```{r}
reg_interact <- glm(log(sales) ~ log(price)*brand,
                    data = Orange_Juice)
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

```


### Findings from Study 4
- Adding "`*'" inside the "glm" function interacts log price with brand, which influences the main slope
- log price for brand Tropicana, Minute Maid, and Dominick's is -2.7, -3.3 and -3.4, respectively.
- Tropicana brand are less price sensitive, meaning that the Tropicana core customers are less bothered by price changes

## Study 5:: Price Sensitivity Interacts with Brand Indicator and Feat

```{r}
ojreg <- glm(log(sales) ~ log(price)*brand*feat, data=Orange_Juice)
coef(ojreg)
beta <- coef(ojreg)
beta


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

```

### Findings from Study 5
- Note that feat is Boolean, indicating if product had been advertised or not. Adding feat into the "glm" equation evaluates the influence of advertising in the relationship between sales and prices.
- Featured products leads to price sensitivity
- Minute Maid elasticity dropped from -2.0 to -3.5 with ads
- Tropicana elasticity dropped from -2.0 to -3.6
- Dominick's elasticity dropped from -2.8 to -3.2

Marketing efforts can expand consumer base to include people who are more price sensitive,attracts consumers beyond brand loyalists. Therefore ad campaigns must be accompanied by **price cuts**.
Since featured products are often discounted, it could be that the demand curve is non-linear. At lower price points, the average customers is more price sensitive.

## Study 6: Elasticity Table
```{r}
b <- coef(ojreg)
b["log(price)"] 
b["log(price)"] + b["log(price):brandminute.maid"]
b["log(price)"] + b["log(price):brandtropicana"]

b["log(price)"] + b["log(price):feat"]
b["log(price)"] + b["log(price):brandminute.maid"] + b["log(price):feat"] + b["log(price):brandminute.maid:feat"]
b["log(price)"] + b["log(price):brandtropicana"] + b["log(price):feat"] + b["log(price):brandtropicana:feat"]

salestable <- tapply(Orange_Juice$sales, Orange_Juice[,c("feat","brand")], sum)
mosaicplot(salestable,col=brandcol)
```


### Findings from Study 6
- Mosaic plot of the amount of advertisement by brand 
- Minute Maid is featured more than Dominick's and Tropicana (Minute Maid < Dominick's < Tropicana)
- Minute Maid had similar price elasticity as Tropicana; it behaved like an expensive product
- This is due to contradicting effect between brands and advertisement. 


