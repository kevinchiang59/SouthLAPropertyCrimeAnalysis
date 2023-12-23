library(dplyr)
library(geosphere)
library(readxl)
library("sf")
library("ggplot2")

#makePropertyAndCrimeDataDF
dataFolder <- "C:/Users/kchiang5/Desktop/South LA Crime Analysis"  

loadedDFs   <- list()

iType       <- 1
dataFile    <- paste(dataFolder,"2019LiquorStores.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,5];
longitude   <- dataDF[,6]; 
GEOID_short <- dataDF[,8];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

masterDF    <- loadedDFs[[iType]]

iType       <- 2
dataFile    <- paste(dataFolder,"2019TobaccoShops.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,5];
longitude   <- dataDF[,6]; 
GEOID_short <- dataDF[,8];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

masterDF    <- rbind(masterDF,loadedDFs[[iType]])

iType       <- 3
dataFile    <- paste(dataFolder,"2019MarijuanaDispensaries.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,5];
longitude   <- dataDF[,6]; 
GEOID_short <- dataDF[,8];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

masterDF    <- rbind(masterDF,loadedDFs[[iType]])

iType       <- 4
dataFile    <- paste(dataFolder,"2019PlacesOfWorship.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,18];
longitude   <- dataDF[,19]; 
GEOID_short <- dataDF[,21];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

masterDF    <- rbind(masterDF,loadedDFs[[iType]])

iType       <- 5 
dataFile    <- paste(dataFolder,"2019PublicParksLatLng.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,3];
longitude   <- dataDF[,4]; 
GEOID_short <- dataDF[,18];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

masterDF    <- rbind(masterDF,loadedDFs[[iType]])

iType       <- 6
dataFile    <- paste(dataFolder,"2019Schools.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,26];
longitude   <- dataDF[,27]; 
GEOID_short <- dataDF[,30];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

masterDF    <- rbind(masterDF,loadedDFs[[iType]])

iType       <- 7
dataFile    <- paste(dataFolder,"2019PartICrime.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,6];
longitude   <- dataDF[,7]; 
GEOID_short <- dataDF[,12];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

masterDF    <- rbind(masterDF,loadedDFs[[iType]])

iType       <- 8
dataFile    <- paste(dataFolder,"2019PartIICrime.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,6];
longitude   <- dataDF[,7]; 
GEOID_short <- dataDF[,12];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

masterDF    <- rbind(masterDF,loadedDFs[[iType]])

#addPop2MasterDF
JDShapeFile <- read_sf("C:/Users/kchiang5/Desktop/shp/sla-2019.shp")

masterDF$pop <- 0

pop <- JDShapeFile$TotalPopul
geo <- JDShapeFile$GEOID

for (ii in 1:length(pop)){

	indx <- which(as.numeric(masterDF$GEOID_short) == as.numeric(geo[ii]))
	
	masterDF$pop[indx] <- pop[ii]
	
}

#buildPopDF
popMat <- matrix(0,nrow=length(pop),ncol=10)
popDF  <- as.data.frame(popMat)

kk    <- length(popDF[1,])

for (ii in 1:length(pop)){

	for (jj in 1:8){

	indx 			   <- which( (as.numeric(masterDF$GEOID_short) == as.numeric(geo[ii]))&(masterDF$type==jj))
	nEvents            <- length(indx)
	
	popDF[ii,jj]       <- nEvents
	}
	
}

popDF[,9]  <- pop;
popDF[,10] <- geo;

names(popDF) <- c('Liquor','Tobacco','Marijuana','Worship','Parks','Schools','PartI','PartII','Population','GEOID')

#standardizeVariables
for (jj in 1:9){

	myVar 			    <- popDF[,jj]
    centered            <- myVar-mean(myVar)
	stdized             <- centered/sd(myVar)
	popDF[,jj+10]       <- stdized
	names(popDF)[jj+10] <- paste('std',names(popDF[jj]),sep="")
}


#ripleyFunction
P1Crimes <- dplyr::filter(masterDF, type %in% "7")
P1CrimeslatLong <- cbind(P1Crimes$latitude, P1Crimes$longitude)
P1CrimeslongLat <- cbind(P1Crimes$longitude, P1Crimes$latitude)

#Set up the centers as the radii of each nuisance property, filter out duplicates
nuisanceRaw <- unique(dplyr::filter(masterDF, type %in% c("1","2","3")))
nuisanceCenters <- cbind(nuisanceRaw$latitude, nuisanceRaw$longitude)
nuisanceCenterFlip <- cbind(nuisanceRaw$longitude, nuisanceRaw$latitude)
nuisanceCenter1 <- cbind(33.93775, -118.2826)
nuisanceCenter1Flip <- cbind(-118.2826, 33.93775)

#Set up the radii for the circle in K function
radii1 <- seq(0,1000,by=10)

#Ripley's function computation
ripleyPartial <- function(longLat,center,radii){
	counter = 0 * radii
	for (i in 1:length(radii)){
		#distHaversine outputs the shortest distance in meters between two points with lats and longs, r assumes a round planet with radii of 6378137 meters, 3.280839895 is the conversion value from meters to feet
		for (j in 1:nrow(longLat)){
			if((3.280839895*distHaversine(longLat[j,],center, r = 6378137)) <= radii[i]){
		counter[i] = counter[i] + 1
					}
				}
		}
		#ideally want to return the function as a xy table with x's of radii, and y's as the counter result at each radii
		return(counter)
		}

ripley <- function(longLat,center,radii){
	counter = matrix(0,nrow = nrow(center), ncol = length(radii))
	for (k in 1:nrow(center)){
		for (i in 1:length(radii)){
			for (j in 1:nrow(longLat)){
				if((3.280839895*distHaversine(longLat[j,],center[k,], r = 6378137)) <= radii[i]){
			counter[k,i] = counter[k,i] + 1
						}
					}
				}
			}
			return(counter)
		}
nuisanceRipleys <- ripley(P1CrimeslongLat,nuisanceCenterFlip,radii1)

protectiveRaw <- unique(dplyr::filter(masterDF, type %in% c("4","5","6")))
protectiveCenterFlip <- cbind(protectiveRaw$longitude,protectiveRaw$latitude)
protectiveRipleys <- ripley(P1CrimeslongLat,protectiveCenterFlip,radii1)



liquorRaw <- unique(dplyr::filter(masterDF, type %in% "1"))
liquorCenterFlip <- cbind(liquorRaw$longitude,liquorRaw$latitude)
tobaccoRaw <- unique(dplyr::filter(masterDF, type %in% "2"))
tobaccoCenterFlip <- cbind(tobaccoRaw$longitude,tobaccoRaw$latitude)
marijuanaRaw <- unique(dplyr::filter(masterDF, type %in% "3"))
marijuanaCenterFlip <- cbind(marijuanaRaw$longitude,marijuanaRaw$latitude)

worshipRaw <- unique(dplyr::filter(masterDF, type %in% "4"))
worshipCenterFlip <- cbind(liquorRaw$longitude,liquorRaw$latitude)
parksRaw <- unique(dplyr::filter(masterDF, type %in% "5"))
parksCenterFlip <- cbind(parksRaw$longitude,parksRaw$latitude)
schoolsRaw <- unique(dplyr::filter(masterDF, type %in% "6"))
schoolsCenterFlip <- cbind(schoolsRaw$longitude,schoolsRaw$latitude)

liquorRipleys <- ripley(P1CrimeslongLat,liquorCenterFlip,radii1)
tobaccoRipleys <- ripley(P1CrimeslongLat,tobaccoCenterFlip,radii1)
marijuanaRipleys <- ripley(P1CrimeslongLat,marijuanaCenterFlip,radii1)

worshipRipleys <- ripley(P1CrimeslongLat,worshipCenterFlip,radii1)
parksRipleys <- ripley(P1CrimeslongLat,parksCenterFlip,radii1)
schoolsRipleys <- ripley(P1CrimeslongLat,schoolsCenterFlip,radii1)



