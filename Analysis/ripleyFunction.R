library(dplyr)
library(geosphere)

source("makePropertyAndCrimeDataDF.R")
source("addPop2MasterDF.R")
source("standardizeVariables.R")
source("buildPopDF.R")


head(masterDF)

#Set up the Lats and Longs for Part 1 Crimes
P1Crimes <- dplyr::filter(masterDF, type %in% "7")
P1CrimeslatLong <- cbind(P1Crimes$latitude, P1Crimes$longitude)
P1CrimeslongLat <- cbind(P1Crimes$longitude, P1Crimes$latitude)

#Set up the centers as the radii of each nuisance property, filter out duplicates
nuisanceRaw <- unique(dplyr::filter(masterDF, type %in% c("1","2","3")))
nuisanceCenters <- cbind(nuisanceRaw$latitude, nuisanceRaw$longitude)
nuisanceCenterFlip <- cbind(nuisanceRaw$longitude, nuisanceRaw$latitude)
nuisanceCenter1 <- cbind(33.93775, s-118.2826)
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

#return Ripley's function output as a table
ripleyPartial(P1CrimeslongLat,nuisanceCenterFlip[75,],radii1)
ripleyPartial(P1CrimeslongLat,nuisanceCenterFlip[1,],radii1)

plot(P1CrimeslongLat[,1],P1CrimeslongLat[,2],xlim=c(-118.295,-118.285),ylim=c(33.95,33.96))
points(nuisanceCenterFlip[,1],nuisanceCenterFlip[,2],col="red"
ripleyPartial(P1CrimeslongLat,c(-118.292,33.9515),radii1)

ripley(P1CrimeslongLat,nuisanceCenterFlip[1:3,],radii1)

#scircle1(33.93775,-118.2826,1)