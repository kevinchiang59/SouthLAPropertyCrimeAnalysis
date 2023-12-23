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
jdFrame <- read_sf("sla-2019.shp")
jdTotPop <- cbind(as.numeric(jdFrame$GEOID),jdFrame$TotalPopul)
colnames(jdTotPop) <- c("GEOID","TotalPopul")

row_number = which(masterFrame$GEOID_short == jdTotPop$GEOID)
popNum = jdTotPop[row_number,2]

findTotalPopul <- function(inputGEOID){
	row_number = which(as.numeric(inputGEOID) == jdTotPop[,1])
	TotalPop = as.numeric(jdTotPop[row_number,2])
	
	myOutput <- list()
	myOutput$Pop <- TotalPop
	return(myOutput)
}
	
n = nrow(masterFrame)
masterFramePop <- matrix(0, nrow = n, ncol = 10)
for (x in 1:n){
	output[x] <- as.numeric(findTotalPopul(masterFramePop[x,1]))
	
	masterFramePop[x,1] <- masterFrame$GEOID_short[x]
	masterFramePop[x,2] <- masterFrame$Liquor[x]
	masterFramePop[x,3] <- masterFrame$Marijuana[x]
	masterFramePop[x,4] <- masterFrame$Tobacco[x]
	masterFramePop[x,5] <- masterFrame$PlacesofWorship[x]
	masterFramePop[x,6] <- masterFrame$Schools[x]
	masterFramePop[x,7] <- masterFrame$PublicParks[x]
	masterFramePop[x,8] <- masterFrame$P1Crimes[x]
	masterFramePop[x,9] <- masterFrame$P2Crimes[x]
	masterFramePop[x,10] <- output[x]
	}
colnames(masterFramePop) <- c("GEOID","Liquor","Marijuana","Tobacco","PlacesofWorship","Schools","PublicParks","P1Crimes","P2Crimes","TotalPopul")

write.csv(masterFramePop,"/Users/Kevin/Desktop/masterDFwithJDPop.csv", row.names = FALSE)
