library(dplyr)
library(geosphere)

source("makePropertyAndCrimeDataDF.R")
source("addPop2MasterDF.R")
source("standardizeVariables.R")
source("buildPopDF.R")

#Create frame of random points inside of the South LA square inside of the tracts
randomSouthLA <- cbind(runif(50,-118.29,-118.255),runif(50,33.95,34))

#Set up the radii for the circle in K function
radii1 <- seq(0,1000,by=10)

#Set up the Lats and Longs for Part 1 Crimes
P1Crimes <- dplyr::filter(masterDF, type %in% "7")
P1CrimeslatLong <- cbind(P1Crimes$latitude, P1Crimes$longitude)
P1CrimeslongLat <- cbind(P1Crimes$longitude, P1Crimes$latitude)

#Ripley Function Compuatation
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
		
#Compute Ripleys
randomRipleys <- ripley(P1CrimeslongLat,randomSouthLA,radii1)