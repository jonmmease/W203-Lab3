#####################################################################
# Program Name: Lab 3.R
# Analyst     : Stephen Holtz
#
#####################################################################

#####################################################################
# Setup

setwd("C:/Users/Holtz/Documents/Berkeley/203 Stats & R/Lab 3")
getwd()
library(car)
#####################################################################

mydata = read.csv("crime_v2.csv")
load("crime_v2.csv")
ls()
summary(mydata)
?hist
hist(mydata$crmrte, 15) # not really normal - long tail to right
hist(mydata$prbarr, 15)
hist(mydata$prbconv, 15)
hist(mydata$prbpris, 15)
hist(mydata$avgsen)
hist(mydata$polpc)
hist(mydata$density) #likely needs a logarithmic transformation
hist(mydata$taxpc) #likely needs a logarithmic transformation
hist(mydata$pctmin80) # Not normally distributed - need to think on transformation
hist(mydata$wcon, 10) # close to normal
hist(mydata$wtuc, 10) # close to normal
hist(mydata$wtrd, 10) # close to normal, some bias to the right?
hist(mydata$wfir) # close to normal
hist(mydata$wser, 10) # normally distributed <500, except one outlier... need to think on that data point.
mean(mydata$wser)
mean(mydata$wmfg)
hist(mydata$wmfg, 10) # close to normal
hist(mydata$wfed, 10) # close to normal, some bias to left
hist(mydata$wsta, 10) # close to normal
hist(mydata$wloc) # close to normal
hist(mydata$mix) #bias to the left - need to think about transformation
hist(mydata$pctymle) #bias to the left - need to think about transformation


scatterplotMatrix(~mydata$crmrte+mydata$pctymle)
scatterplotMatrix(~log(mydata$crmrte)+log10(mydata$pctymle)+log(mydata$density))


#####################################################################
# EDA

crime_rate_model_1 = lm(mydata$crmrte ~ mydata$prbarr)
plot(mydata$prbarr, mydata$crmrt, main = "Crime-Rate model 1")
abline(crime_rate_model_1)
summary(crime_rate_model_1) # R-squared = 0.1547

crime_rate_model_2 = lm(mydata$crmrte ~ mydata$prbconv)
plot(mydata$prbconv, mydata$crmrt)
abline(crime_rate_model_2)# this is rubbish

crime_rate_model_3 = lm(mydata$crmrte ~ mydata$prbpris)
plot(mydata$prbpris, mydata$crmrte)
abline(crime_rate_model_3)  
summary(crime_rate_model_3) # R-squared = 0.002208

crime_rate_model_4 = lm(mydata$crmrte ~ mydata$polpc)
plot(mydata$polpc, mydata$crmrte)
abline(crime_rate_model_4)
summary(crime_rate_model_4) # R-squared = 0.02886

mydata$density[79]
mydata$density[79] <- 
crime_rate_model_5 = lm(mydata$crmrte ~ mydata$density)
plot(mydata$density, mydata$crmrte)
abline(crime_rate_model_5)
summary(crime_rate_model_5) # R-squared = 0.5314

crime_rate_model_6 = lm(mydata$crmrte ~ mydata$avgsen)
plot(mydata$avgsen, mydata$crmrte)
abline(crime_rate_model_6)
crime_rate_model_6 = lm(mydata$
summary(crime_rate_model_6) # R-squared = 0.0007514

crime_rate_model_7 = lm(mydata$crmrte ~ mydata$pctmin80)
plot(mydata$pctmin80, mydata$crmrte)
abline(crime_rate_model_7)
summary(crime_rate_model_7) # R-squared = 0.03489

crime_rate_model_8 = lm(mydata$crmrte ~ mydata$wcon)
plot(mydata$wcon, mydata$crmrte)
abline(crime_rate_model_8)
summary(crime_rate_model_8) # R-squared = 0.1539

crime_rate_model_9 = lm(mydata$crmrte ~ mydata$wtuc)
plot(mydata$wtuc, mydata$crmrte)
abline(crime_rate_model_9)
summary(crime_rate_model_9) # R-squared = 0.0526

crime_rate_model_10 = lm(mydata$crmrte ~ mydata$wtrd)
plot(mydata$wtrd, mydata$crmrte)
abline(crime_rate_model_10)
summary(crime_rate_model_10) # R-squared = 0.1682

crime_rate_model_11 = lm(mydata$crmrte ~ mydata$wfir)
plot(mydata$wfir, mydata$crmrte)
abline(crime_rate_model_11)
summary(crime_rate_model_11) # R-squared = 0.1086

crime_rate_model_12 = lm(mydata$crmrte ~ mydata$wser)
plot(mydata$wser, mydata$crmrte)
abline(crime_rate_model_12)
summary(crime_rate_model_12) # R-squared = 0.002763

crime_rate_model_13 = lm(mydata$crmrte ~ mydata$wmfg)
plot(mydata$wmfg, mydata$crmrte)
abline(crime_rate_model_13)
summary(crime_rate_model_13) # R-squared = 0.1255

crime_rate_model_14 = lm(mydata$crmrte ~ mydata$wfed)
plot(mydata$wfed, mydata$crmrte)
abline(crime_rate_model_14)
summary(crime_rate_model_14) # R-squared = 0.02363

crime_rate_model_15 = lm(mydata$crmrte ~ mydata$wsta)
plot(mydata$wsta, mydata$crmrte)
abline(crime_rate_model_15)
summary(crime_rate_model_15) # R-squared = 0.01853

crime_rate_model_16 = lm(mydata$crmrte ~ mydata$wloc)
plot(mydata$wloc, mydata$crmrte)
abline(crime_rate_model_16)
summary(crime_rate_model_16) # R-squared = 0.1214

crime_rate_model_17 = lm(mydata$crmrte ~ mydata$mix)
plot(mydata$mix, mydata$crmrte)
abline(crime_rate_model_17)
summary(crime_rate_model_17) # R-squared = 0.01701

crime_rate_model_18 = lm(mydata$crmrte ~ mydata$pctymle)
plot(mydata$pctymle, mydata$crmrte)
abline(crime_rate_model_18)
summary(crime_rate_model_18) # R-squared = 0.08483

##############################################
# First Model Specification
# The variables of interest are:
# 1. Young male
# 2. Probability of arrest
# 3. Probability of conviction
# 4. Severity of punishment

First_Model_Specification = lm(mydata$crmrte ~ mydata$pctymle + mydata$prbarr + mydata$prbconv + mydata$prbpris + mydata$avgsen)
summary(crime_rate_model_19) R-squared = 0.5363

##############################################
# combined models

crime_rate_model_19 = lm(mydata$crmrte ~ mydata$density + mydata$wcon)
plot(mydata$density, mydata$crmrte)
abline(crime_rate_model_19)
summary(crime_rate_model_19) # R-squared = 0.5363

crime_rate_model_20 = lm(mydata$crmrte ~ mydata$density + mydata$prbarr + mydata$wcon + mydata$pctymle)
plot(mydata$density, mydata$crmrte)
abline(crime_rate_model_20)
summary(crime_rate_model_20) # R-squared = 0.6021

##############################################
# check co-linearity of wage terms
wage_con_tuc = lm(mydata$wcon ~ mydata$wtuc)
plot(mydata$wcon, mydata$wtuc)
plot(mydata$wcon, mydata$wtrd)
plot(mydata$wcon, mydata$wfir)
plot(mydata$wcon, mydata$wser)
plot(mydata$wcon, mydata$wmfg)
plot(mydata$wcon, mydata$wfed)
plot(mydata$wcon, mydata$wsta)
plot(mydata$wcon, mydata$wloc)
plot(mydata$wfed, mydata$wsta)