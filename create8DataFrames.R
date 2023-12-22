library(crsuggest)
library(sf)
library(tidyverse)
readRenviron("~/.Renviron")
library(censusapi)
library(tidycensus)
library("tigris")
library("leaflet")


#load lac_population
lac_population <- get_acs(geography = "tract", variables = "B05006_150", state = "CA", county = "Los Angeles", year = 2020, geometry = TRUE)


#original function made to output long and short GEOID's with density 
find_GEOID_density <- function(latitude, longitude, dataframe){

	geo_number<- call_geolocator_latlon(latitude,longitude)
	short_geo_number = substring(geo_number,1,11)
	row_number = which(lac_population$GEOID == short_geo_number)
	pop_estimate = dataframe[row_number, 4]
	
	density <- pop_estimate
	
	myOutputs <- list()
	myOutputs$GEOID_true <- geo_number
	myOutputs$GEOID_short <- short_geo_number
	myOutputs$density<- density$estimate
	
	return(myOutputs)
}


#original function made to create a data frame matrix using find_GEOID_density
find_GEOID_density_matrix <- function(latitude, longitude, dataframe){
	n = length(latitude)
	myMatrix <- matrix(0,nrow = n, ncol = 5)
	for (x in 1:n){
	output <- find_GEOID_density(latitude[x],longitude[x],dataframe)
	myMatrix[x,1] <- latitude[x]
	myMatrix[x,2] <- longitude[x]
	myMatrix[x,3] <- output$GEOID_true
	myMatrix[x,4] <- output$GEOID_short
	myMatrix[x,5] <- output$density
		}
		myOutput <- as.data.frame(myMatrix)
		names(myOutput) <- c("latitude", "longitude", "GEOID_true", "GEOID_short", "density")
		return(myOutput)
	}


#load Crime Data
dataFolder <- "/Users/Kevin/Desktop/South LA Crime Analysis"

part1File <- paste(dataFolder,"2019PartICrime.csv",sep="/")
part1DF   <- read.csv(part1File);
latPart1  <- part1DF[,6];
lngPart1  <- part1DF[,7]; 

part2File <- paste(dataFolder,"2019PartIICrime.csv",sep="/")
part2DF   <- read.csv(part2File);
latPart2  <- part2DF[,6];
lngPart2  <- part2DF[,7]; 

liquorFile <- paste(dataFolder,"2019LiquorStores.csv",sep="/")
liquorDF   <- read.csv(liquorFile);
latLQ      <- liquorDF[,5];
lngLQ      <- liquorDF[,6]; 

mjFile    <- paste(dataFolder,"2019MarijuanaDispensaries.csv",sep="/")
mjDF      <- read.csv(mjFile);
latMJ     <- mjDF[,5];
lngMJ     <- mjDF[,6];

smokeFile <- paste(dataFolder,"2019TobaccoShops.csv",sep="/")
smokeDF   <- read.csv(smokeFile);
latTB     <- smokeDF[,5];
lngTB     <- smokeDF[,6]; 

wrshpFile <- paste(dataFolder,"2019PlacesOfWorship.csv",sep="/")
wrshpDF   <- read.csv(wrshpFile);
latPW     <- wrshpDF[,18];
lngPW     <- wrshpDF[,19];
 
pbprkFile <- paste(dataFolder,"2019PublicParksLatLng.csv",sep="/")
pbprkDF   <- read.csv(pbprkFile);
latPP     <- pbprkDF[,3];
lngPP     <- pbprkDF[,4]; 

schlFile <- paste(dataFolder,"2019Schools.csv",sep="/")
schlDF   <- read.csv(schlFile);
latSc    <- schlDF[,26];
lngSc    <- schlDF[,27]; 

plot(lngPart1,latPart1)
plot(lngPart2,latPart2)

part1DF$latR <- part1DF$latitude + rnorm(length(part1DF[,1]),0,0.002)
part1DF$lngR <- part1DF$longitude + rnorm(length(part1DF[,1]),0,0.002)

lqp <- cbind(lngLQ,latLQ)
mjp <- cbind(lngMJ,latMJ)
tbp <- cbind(lngTB,latTB)
npp <- rbind(lqp,tbp,mjp)
dev.new()
plot(npp[,1],npp[,2])

pwp <- cbind(lngPW,latPW)
ppp <- cbind(lngPP,latPP)
scp <- cbind(lngSc,latSc)
ptp <- rbind(pwp,ppp,scp)
dev.new()
plot(ptp[,1],ptp[,2])

part1crimes <- cbind(lngPart1,latPart1)
part2crimes <- cbind(lngPart2,latPart2)
combinedCrimes <- rbind(part1crimes,part2crimes)
plot(combinedCrimes)


#Create Data Frames
MJDataFrame <- find_GEOID_density_matrix(latMJ,lngMJ,lac_population)
write.csv(MJDataFrame,"/Users/Kevin/Desktop/MJDataFrame.csv", row.names = FALSE)

PPDataFrame <- find_GEOID_density_matrix(latPP,lngPP,lac_population)
write.csv(PPDataFrame,"/Users/Kevin/Desktop/PPDataFrame.csv", row.names = FALSE)

PWDataFrame <- find_GEOID_density_matrix(latPW,lngPW,lac_population)
write.csv(PWDataFrame,"/Users/Kevin/Desktop/PWDataFrame.csv", row.names = FALSE)

SCDataFrame <- find_GEOID_density_matrix(latSc,lngSc,lac_population)
write.csv(ScDataFrame,"/Users/Kevin/Desktop/SCDataFrame.csv", row.names = FALSE)

TBDataFrame <- find_GEOID_density_matrix(latTB,lngTB,lac_population)
write.csv(ScDataFrame,"/Users/Kevin/Desktop/TBDataFrame.csv", row.names = FALSE)

LQDataFrame <- find_GEOID_density_matrix(latLQ,lngLQ,lac_population)
write.csv(ScDataFrame,"/Users/Kevin/Desktop/LQDataFrame.csv", row.names = FALSE)

P1CrimeDataFrame <- find_GEOID_density_matrix(latPart1,lngPart1,lac_population)
write.csv(P1CrimeDataFrame,"/Users/Kevin/Desktop/P1CrimeDataFrame.csv", row.names = FALSE)

P2CrimeDataFrame <- find_GEOID_density_matrix(latPart2,lngPart2,lac_population)
write.csv(P2CrimeDataFrame,"/Users/Kevin/Desktop/P2CrimeDataFrame.csv", row.names = FALSE)