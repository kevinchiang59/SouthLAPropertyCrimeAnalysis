library(tidycensus)
library(tigris)
library(crsuggest)
library(sf)
library(tidyverse)
library(ggplot2)
library(maps)
library(maptools)
library(sp) 

#Change working directory to the one with JD's csv file
masterFramePop <- read_csv("masterDFwithJDPop.csv")

#Creating centered masterFramePop data frame
dfCC<-masterFramePop[complete.cases(masterFramePop),]
dfCC$LiquorC <- dfCC$Liquor-mean(dfCC$Liquor)
dfCC$MarijuanaC <- dfCC$Marijuana-mean(dfCC$Marijuana)
dfCC$TobaccoC <- dfCC$Tobacco-mean(dfCC$Tobacco)
dfCC$TotalPopulC <- dfCC$TotalPopul-mean(dfCC$TotalPopul)

#Creating masterFramePop data frame but with median replacement (4311) for NA elements in TotalPopul
dfMR <- masterFramePop
dfMR <- dfMR %>% mutate(across(TotalPopul, ~replace_na(., median(., na.rm=TRUE))))
dfMR$LiquorC <- dfMR$Liquor-mean(dfMR$Liquor)
dfMR$MarijuanaC <- dfMR$Marijuana-mean(dfMR$Marijuana)
dfMR$TobaccoC <- dfMR$Tobacco-mean(dfMR$Tobacco)
dfMR$TotalPopulC <- dfMR$TotalPopul-mean(dfMR$TotalPopul)

#Standardizing the two data frames
dfCC$LiquorS <- dfCC$LiquorC/sd(dfCC$LiquorC)
dfCC$MarijuanaS <- dfCC$MarijuanaC/sd(dfCC$MarijuanaC)
dfCC$TobaccoS <- dfCC$TobaccoC/sd(dfCC$TobaccoC)
dfCC$TotalPopulS <- dfCC$TotalPopulC/sd(dfCC$TotalPopulC)

dfMR$LiquorS <- dfMR$LiquorC/sd(dfMR$LiquorC)
dfMR$MarijuanaS <- dfMR$MarijuanaC/sd(dfMR$MarijuanaC)
dfMR$TobaccoS <- dfMR$TobaccoC/sd(dfMR$TobaccoC)
dfMR$TotalPopulS <- dfMR$TotalPopulC/sd(dfMR$TotalPopulC)

#6 Linear Model Tests
summary(lm(formula = P1Crimes ~ Liquor + Marijuana + Tobacco + TotalPopul, data = dfCC))
summary(lm(formula = P1Crimes ~ Liquor*TotalPopul + Marijuana*TotalPopul + Tobacco*TotalPopul, data = dfCC))
summary(lm(formula = P1Crimes ~ Liquor * Marijuana * Tobacco + TotalPopul, data = dfCC))
summary(lm(formula = P1Crimes ~ Liquor * Marijuana * Tobacco * TotalPopul, data = dfCC))
summary(lm(formula = P1Crimes ~ Liquor + Marijuana + Tobacco, data = dfCC))
summary(lm(formula = P1Crimes ~ Liquor * Marijuana * Tobacco, data = dfCC))

summary(lm(formula = P1Crimes ~ Liquor + Marijuana + Tobacco + TotalPopul, data = dfMR))
summary(lm(formula = P1Crimes ~ Liquor*TotalPopul + Marijuana*TotalPopul + Tobacco*TotalPopul, data = dfMR))
summary(lm(formula = P1Crimes ~ Liquor * Marijuana * Tobacco + TotalPopul, data = dfMR))
summary(lm(formula = P1Crimes ~ Liquor * Marijuana * Tobacco * TotalPopul, data = dfMR))
summary(lm(formula = P1Crimes ~ Liquor + Marijuana + Tobacco, data = dfMR))
summary(lm(formula = P1Crimes ~ Liquor * Marijuana * Tobacco, data = dfMR))

summary(lm(formula = P2Crimes ~ Liquor + Marijuana + Tobacco + TotalPopul, data = dfCC))
summary(lm(formula = P2Crimes ~ Liquor*TotalPopul + Marijuana*TotalPopul + Tobacco*TotalPopul, data = dfCC))
summary(lm(formula = P2Crimes ~ Liquor * Marijuana * Tobacco + TotalPopul, data = dfCC))
summary(lm(formula = P2Crimes ~ Liquor * Marijuana * Tobacco * TotalPopul, data = dfCC))
summary(lm(formula = P2Crimes ~ Liquor + Marijuana + Tobacco, data = dfCC))
summary(lm(formula = P2Crimes ~ Liquor * Marijuana * Tobacco, data = dfCC))

summary(lm(formula = P2Crimes ~ Liquor + Marijuana + Tobacco + TotalPopul, data = dfMR))
summary(lm(formula = P2Crimes ~ Liquor*TotalPopul + Marijuana*TotalPopul + Tobacco*TotalPopul, data = dfMR))
summary(lm(formula = P2Crimes ~ Liquor * Marijuana * Tobacco + TotalPopul, data = dfMR))
summary(lm(formula = P2Crimes ~ Liquor * Marijuana * Tobacco * TotalPopul, data = dfMR))
summary(lm(formula = P2Crimes ~ Liquor + Marijuana + Tobacco, data = dfMR))
summary(lm(formula = P2Crimes ~ Liquor * Marijuana * Tobacco, data = dfMR))

#6 Linear Model Tests with Centered Variables
summary(lm(formula = P1Crimes ~ LiquorC + MarijuanaC + TobaccoC + TotalPopulC, data = dfCC))
summary(lm(formula = P1Crimes ~ LiquorC*TotalPopulC + MarijuanaC*TotalPopulC + TobaccoC*TotalPopulC, data = dfCC))
summary(lm(formula = P1Crimes ~ LiquorC * MarijuanaC * TobaccoC + TotalPopulC, data = dfCC))
summary(lm(formula = P1Crimes ~ LiquorC * MarijuanaC * TobaccoC * TotalPopulC, data = dfCC))
summary(lm(formula = P1Crimes ~ LiquorC + MarijuanaC + TobaccoC, data = dfCC))
summary(lm(formula = P1Crimes ~ LiquorC * MarijuanaC * TobaccoC, data = dfCC))

summary(lm(formula = P1Crimes ~ LiquorC + MarijuanaC + TobaccoC + TotalPopulC, data = dfMR))
summary(lm(formula = P1Crimes ~ LiquorC*TotalPopulC + MarijuanaC*TotalPopulC + TobaccoC*TotalPopulC, data = dfMR))
summary(lm(formula = P1Crimes ~ LiquorC * MarijuanaC * TobaccoC + TotalPopulC, data = dfMR))
summary(lm(formula = P1Crimes ~ LiquorC * MarijuanaC * TobaccoC * TotalPopulC, data = dfMR))
summary(lm(formula = P1Crimes ~ LiquorC + MarijuanaC + TobaccoC, data = dfMR))
summary(lm(formula = P1Crimes ~ LiquorC * MarijuanaC * TobaccoC, data = dfMR))

summary(lm(formula = P2Crimes ~ LiquorC + MarijuanaC + TobaccoC + TotalPopulC, data = dfCC))
summary(lm(formula = P2Crimes ~ LiquorC*TotalPopulC + MarijuanaC*TotalPopulC + TobaccoC*TotalPopulC, data = dfCC))
summary(lm(formula = P2Crimes ~ LiquorC * MarijuanaC * TobaccoC + TotalPopulC, data = dfCC))
summary(lm(formula = P2Crimes ~ LiquorC * MarijuanaC * TobaccoC * TotalPopulC, data = dfCC))
summary(lm(formula = P2Crimes ~ LiquorC + MarijuanaC + TobaccoC, data = dfCC))
summary(lm(formula = P2Crimes ~ LiquorC * MarijuanaC * TobaccoC, data = dfCC))

summary(lm(formula = P2Crimes ~ LiquorC + MarijuanaC + TobaccoC + TotalPopulC, data = dfMR))
summary(lm(formula = P2Crimes ~ LiquorC*TotalPopulC + MarijuanaC*TotalPopulC + TobaccoC*TotalPopulC, data = dfMR))
summary(lm(formula = P2Crimes ~ LiquorC * MarijuanaC * TobaccoC + TotalPopulC, data = dfMR))
summary(lm(formula = P2Crimes ~ LiquorC * MarijuanaC * TobaccoC * TotalPopulC, data = dfMR))
summary(lm(formula = P2Crimes ~ LiquorC + MarijuanaC + TobaccoC, data = dfMR))
summary(lm(formula = P2Crimes ~ LiquorC * MarijuanaC * TobaccoC, data = dfMR))

#6 Linear Model Tests with Standardized Variables
summary(lm(formula = P1Crimes ~ LiquorS + MarijuanaS + TobaccoS + TotalPopulS, data = dfCC))
summary(lm(formula = P1Crimes ~ LiquorS*TotalPopulS + MarijuanaS*TotalPopulS + TobaccoS* TotalPopulS, data = dfCC))
summary(lm(formula = P1Crimes ~ LiquorS * MarijuanaS * TobaccoS + TotalPopulS, data = dfCC))
summary(lm(formula = P1Crimes ~ LiquorS * MarijuanaS * TobaccoS * TotalPopulS, data = dfCC))
summary(lm(formula = P1Crimes ~ LiquorS + MarijuanaS + TobaccoS, data = dfCC))
summary(lm(formula = P1Crimes ~ LiquorS * MarijuanaS * TobaccoS, data = dfCC))

summary(lm(formula = P1Crimes ~ LiquorS + MarijuanaS + TobaccoS + TotalPopulS, data = dfMR))
summary(lm(formula = P1Crimes ~ LiquorS*TotalPopulS + MarijuanaS*TotalPopulS + TobaccoS* TotalPopulS, data = dfMR))
summary(lm(formula = P1Crimes ~ LiquorS * MarijuanaS * TobaccoS + TotalPopulS, data = dfMR))
summary(lm(formula = P1Crimes ~ LiquorS * MarijuanaS * TobaccoS * TotalPopulS, data = dfMR))
summary(lm(formula = P1Crimes ~ LiquorS + MarijuanaS + TobaccoS, data = dfMR))
summary(lm(formula = P1Crimes ~ LiquorS * MarijuanaS * TobaccoS, data = dfMR))

summary(lm(formula = P2Crimes ~ LiquorS + MarijuanaS + TobaccoS + TotalPopulS, data = dfCC))
summary(lm(formula = P2Crimes ~ LiquorS*TotalPopulS + MarijuanaS*TotalPopulS + TobaccoS* TotalPopulS, data = dfCC))
summary(lm(formula = P2Crimes ~ LiquorS * MarijuanaS * TobaccoS + TotalPopulS, data = dfCC))
summary(lm(formula = P2Crimes ~ LiquorS * MarijuanaS * TobaccoS * TotalPopulS, data = dfCC))
summary(lm(formula = P2Crimes ~ LiquorS + MarijuanaS + TobaccoS, data = dfCC))
summary(lm(formula = P2Crimes ~ LiquorS * MarijuanaS * TobaccoS, data = dfCC))

summary(lm(formula = P2Crimes ~ LiquorS + MarijuanaS + TobaccoS + TotalPopulS, data = dfMR))
summary(lm(formula = P2Crimes ~ LiquorS*TotalPopulS + MarijuanaS*TotalPopulS + TobaccoS* TotalPopulS, data = dfMR))
summary(lm(formula = P2Crimes ~ LiquorS * MarijuanaS * TobaccoS + TotalPopulS, data = dfMR))
summary(lm(formula = P2Crimes ~ LiquorS * MarijuanaS * TobaccoS * TotalPopulS, data = dfMR))
summary(lm(formula = P2Crimes ~ LiquorS + MarijuanaS + TobaccoS, data = dfMR))
summary(lm(formula = P2Crimes ~ LiquorS * MarijuanaS * TobaccoS, data = dfMR))


