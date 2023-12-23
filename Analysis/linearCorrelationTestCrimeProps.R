library(tidycensus)
library(tigris)
library(crsuggest)
library(sf)
library(tidyverse)
library(ggplot2)
library(maps)
library(maptools)
library(sp) 

masterFrame <- read_csv("masterDF.csv")

#Linear Model Test (Inconclusive, only shows Coefficients)
lm(data=masterFrame, formula = masterFrame$P1Crimes + masterFrame$P2Crimes ~ masterFrame$Liquor + masterFrame$Marijuana + masterFrame$Tobacco)

#Plotting Crimes Against Niusance Properties
combinedPCrimes <- masterFrame$P1Crimes + masterFrame$P2Crimes
combinedNiusance <- masterFrame$Liquor + masterFrame$Marijuana + masterFrame$Tobacco
plot(combinedPCrimes ~ combinedNiusance, data=masterFrame)

#Getting Residuals for the correlation
#P1Crimes vs Niusance Properties
summary(lm(formula = masterFrame$P1Crimes ~ masterFrame$Liquor + masterFrame$Marijuana + masterFrame$Tobacco,data= masterFrame))
#P2Crimes vs Niusance Properties
summary(lm(formula = masterFrame$P2Crimes ~ masterFrame$Liquor + masterFrame$Marijuana + masterFrame$Tobacco,data= masterFrame))
#P1 and P2 Crimes Combined vs Niusance Properties
summary(lm(formula = masterFrame$P1Crimes + masterFrame$P2Crimes ~ masterFrame$Liquor + geoidframe$Marijuana + masterFrame$Tobacco,data=masterFrame))
summary(lm(formula = combinedNiusance ~ combinedPCrimes,data=masterFrame))

#Work with Dr. Fitzpatrick on 9/26
barplot(table(masterFrame$P1Crimes))
table(masterFrame$P1Crimes)
unique(masterFrame$P1Crimes)

summary(lm(formula = masterFrame$P1Crimes ~ masterFrame$Liquor * masterFrame$Marijuana * masterFrame$Tobacco,data= masterFrame))
summary(lm(formula = masterFrame$P2Crimes ~ masterFrame$Liquor * masterFrame$Marijuana * masterFrame$Tobacco,data= masterFrame))
summary(lm(formula = combinedPCrimes ~ masterFrame$Liquor * masterFrame$Marijuana * masterFrame$Tobacco,data= masterFrame))

plot(density(masterFrame$P1Crimes))
qqnorm(masterFrame$P1Crimes)
qqline(masterFrame$P1Crimes)
dev.new()
plot(density(masterFrame$P1Crimes))

plot(density(masterFrame$P2Crimes))
qqnorm(masterFrame$P2Crimes)
qqline(masterFrame$P2Crimes)
dev.new()
plot(density(masterFrame$P2Crimes))

plot(density(combinedPCrimes))
qqnorm(combinedPCrimes)
qqline(combinedPCrimes)
dev.new()
plot(density(combinedPCrimes))

#New work with TotalPopul variable from Jason Douglas
masterFramePop <- read_csv("masterDFwithJDPop.csv")
lm(data=masterFramePop, formula = masterFramePop$P1Crimes + masterFramePop$P2Crimes ~ masterFramePop$Liquor + masterFramePop$Marijuana + masterFramePop$Tobacco + masterFramePop$TotalPopul)

combinedPopCrimes <- masterFramePop$P1Crimes + masterFramePop$P2Crimes
combinedPopNiusance <- masterFramePop$Liquor + masterFramePop$Marijuana + masterFramePop$Tobacco + masterFramePop$TotalPopul
plot(combinedPopCrimes ~ combinedPopNiusance, data=masterFramePop)

summary(lm(formula = masterFramePop$P1Crimes ~ masterFramePop$Liquor * masterFramePop$Marijuana * masterFramePop$Tobacco * masterFramePop$TotalPopul,data= masterFramePop))
summary(lm(formula = masterFramePop$P2Crimes ~ masterFramePop$Liquor * masterFramePop$Marijuana * masterFramePop$Tobacco * masterFramePop$TotalPopul,data= masterFramePop))
summary(lm(formula = combinedPopCrimes ~ masterFramePop$Liquor * masterFramePop$Marijuana * masterFramePop$Tobacco * masterFramePop$TotalPopul,data= masterFramePop))

summary(lm(formula = masterFramePop$P1Crimes ~ masterFramePop$Liquor + masterFramePop$Marijuana + masterFramePop$Tobacco + masterFramePop$TotalPopul,data= masterFrame))
summary(lm(formula = masterFramePop$P2Crimes ~ masterFramePop$Liquor + masterFramePop$Marijuana + masterFramePop$Tobacco + masterFramePop$TotalPopul,data= masterFrame))
summary(lm(formula = combinedPopCrimes ~ masterFramePop$Liquor + masterFramePop$Marijuana + masterFramePop$Tobacco + masterFramePop$TotalPopul,data= masterFrame))