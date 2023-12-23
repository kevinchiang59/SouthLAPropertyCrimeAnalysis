library(dplyr)
library(geosphere)
library(spatstat)
library(maptools)
library(terra)
library(sf)

#same setup as ripleyFunction.R
source("makePropertyAndCrimeDataDF.R")
source("addPop2MasterDF.R")
source("standardizeVariables.R")
source("buildPopDF.R")

#Set up the Lats and Longs for Part 1 Crimes
P1Crimes <- dplyr::filter(masterDF, type %in% "7")
P1CrimeslatLong <- cbind(P1Crimes$latitude, P1Crimes$longitude)
P1CrimeslongLat <- cbind(P1Crimes$longitude, P1Crimes$latitude)

#confirming P1CrimeslongLat is of type data.matrix for later coercion to type ppp 
matrixP1CrimeslongLat <- data.matrix(P1CrimeslongLat)
head(matrixP1CrimeslongLat)

#create ppp data object by combining a data.matrix with owin(observation window)
#center is the coordinate for Row 1 of the nuisance properties
#circlular obervation window utilizes a matrix
#radius is 1000 feet * (conversion factor to meters)
circ_owin <- disc(center = c(-118.2826, 33.93775), radius = 1000*0.3048)
circ_owinFlip <- owin(center = c(33.93775, -118.2826), radius = 1000*0.3048)
ppp_obj <- as.ppp((matrixP1CrimeslongLat), W = circ_owin)
ppp_obj <- as.ppp((matrixP1CrimeslongLat), W = circ_owinFlip)

#plot a Ripley K function using the ppp_object
ripley_result <- Kest(ppp_obj, r = seq(0, 1000, by = 10), window = owin(center = c(-118.2826, 33.93775)))
ripley_result <- Kest(ppp_obj, r = seq(0, 1000, by = 10), window = owin(center = c(33.93775, -118.2826)))

#create shapefile boundary for corrected Ripleys
JDShapeFile <- read_sf("D:/LMU/Projects/TRDRP/Crime and Property Data for Ben/commondata/shp/sla-2019.shp")
#JDShapeFile <- read_sf("/Users/Kevin/Desktop/shp/sla-2019.shp")
#may need to replace read_sf with readShapePoly, but sf and terra packages may be hidden
boundary <- JDShapeFile

#combine circular boundary with South LA edge boundary
#intersect.owin doesn't recognize boundary from JDShapeFile as a window, will need to look into it
combinedBoundary <- intersect.owin(circ_owin,boundary)
new_owin <- owin(poly = polygon(combinedBoundary))

#create new ppp object using combined boundary
new_ppp_obj <- as.ppp((matrixP1CrimeslongLat), W = new_owin)
#fix the boundary
new_ppp_obj <- setcov((ppp_obj), correction = "border")

#new experimentation, successfully generates output
circ_owin <- disc(centre = c(-118.2826, 33.93775), radius = 0.00898315/2, mask = FALSE,"Spatial")
ppp_obj <- as.ppp((matrixP1CrimeslongLat), W = circ_owin)
ripley_result <- Kest(ppp_obj)
plot(ripley_result)

#comparison with ripley at same location
ripleyPartial <- function(longLat,center,radii){
	counter = 0 * radii
	for (i in 1:length(radii)){
				for (j in 1:nrow(longLat)){
			if((3.280839895*distHaversine(longLat[j,],center, r = 6378137)) <= radii[i]){
		counter[i] = counter[i] + 1
					}
				}
		}
		return(counter)
		}
testRipley <- ripleyPartial(P1CrimeslongLat,as.numeric(c(-118.2826, 33.95)),radii1)
testRipley <- rbind(radii1,testRipley)
transposeRipley <- t(testRipley)
plot(transposeRipley[,1], transposeRipley[,2],pch=".")
lines(transposeRipley[,1], transposeRipley[,2])

#tic()(code)toc() counts the time elapsed
tic()
testRipley <- ripleyPartial(P1CrimeslongLat,as.numeric(c(-118.2826, 33.95)),radii1)
testRipley <- rbind(radii1,testRipley)
transposeRipley <- t(testRipley)
plot(transposeRipley[,1], transposeRipley[,2],pch=".")
lines(transposeRipley[,1], transposeRipley[,2])
toc()

#combining multiple circ_owins
circ_owin1 <- disc(centre = c(-118.2826, 33.93775), radius = 0.00898315/2, mask = FALSE,"Spatial")
circ_owin2 <- disc(centre = c(-118.283, 33.935), radius = 0.00898315/2, mask = FALSE,"Spatial")


#1 is liquor
#2 is tobacco
#3 is marijuana


#cumulative tobacco ripley k function
circ_owin_list_tobacco <- list(circ_owin1, circ_owin2)
tobaccoCut <- dplyr::filter(masterDF, type %in% "2")
tobaccolatLong <- cbind(tobaccoCut$latitude, tobaccoCut$longitude)
for (i in 1:(nrow(tobaccolatLong))){
	circ_owin_list_tobacco[[i]] <- disc(centre = c(tobaccolatLong[i,2], tobaccolatLong[i,1]), radius = 0.00898315/2, mask = FALSE,"Spatial")
}

ppp_obj1 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_tobacco[[1]])
ppp_obj2 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_tobacco[[2]])
ppp_obj_list_tobacco <- list(ppp_obj1, ppp_obj2)
for (i in 1:length(circ_owin_list_tobacco)){
	ppp_obj_list_tobacco[[i]] <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_tobacco[[i]])
}

ripley_result1 <- Kest(ppp_obj_list[[1]])
ripley_result2 <- Kest(ppp_obj_list[[2]])
ripley_results_tobacco <- list(ripley_result1, ripley_result2)
for (i in 1:length(ppp_obj_list)){
	ripley_results_tobacco[[i]] <- Kest(ppp_obj_list_tobacco[[i]])
}

plot(ripley_results_tobacco[[1]], main="Tobacco Ripleys")
for (i in 2:length(ripley_results_tobacco)){
	plot(ripley_results_tobacco[[i]],add=TRUE)
}

#cumulative liquor ripley k function
circ_owin_list_liquor <- list(circ_owin1, circ_owin2)
liquorCut <- dplyr::filter(masterDF, type %in% "1")
liquorlatLong <- cbind(liquorCut$latitude, liquorCut$longitude)
for (i in 1:(nrow(liquorlatLong))){
	circ_owin_list_liquor[[i]] <- disc(centre = c(liquorlatLong[i,2], liquorlatLong[i,1]), radius = 0.00898315/2, mask = FALSE,"Spatial")
}

ppp_obj1 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_liquor[[1]])
ppp_obj2 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_liquor[[2]])
ppp_obj_list_liquor <- list(ppp_obj1, ppp_obj2)
for (i in 1:length(circ_owin_list_liquor)){
	ppp_obj_list_liquor[[i]] <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_liquor[[i]])
}

ripley_result1 <- Kest(ppp_obj_list[[1]])
ripley_result2 <- Kest(ppp_obj_list[[2]])
ripley_results_liquor <- list(ripley_result1, ripley_result2)
for (i in 1:length(ppp_obj_list_liquor)){
	ripley_results_liquor[[i]] <- Kest(ppp_obj_list_liquor[[i]])
}

plot(ripley_results_liquor[[1]], main="Liquor Ripleys")
for (i in 2:length(ripley_results_liquor)){
	plot(ripley_results_liquor[[i]],add=TRUE)
}

#cumulative marijuana ripley k function
circ_owin_list_marijuana <- list(circ_owin1, circ_owin2)
marijuanaCut <- dplyr::filter(masterDF, type %in% "3")
marijuanalatLong <- cbind(marijuanaCut$latitude, marijuanaCut$longitude)
for (i in 1:(nrow(marijuanalatLong))){
	circ_owin_list_marijuana[[i]] <- disc(centre = c(marijuanalatLong[i,2], marijuanalatLong[i,1]), radius = 0.00898315/2, mask = FALSE,"Spatial")
}

ppp_obj1 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_marijuana[[1]])
ppp_obj2 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_marijuana[[2]])
ppp_obj_list_marijuana <- list(ppp_obj1, ppp_obj2)
for (i in 1:length(circ_owin_list_marijuana)){
	ppp_obj_list_marijuana[[i]] <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_marijuana[[i]])
}

ripley_result1 <- Kest(ppp_obj_list[[1]])
ripley_result2 <- Kest(ppp_obj_list[[2]])
ripley_results_marijuana <- list(ripley_result1, ripley_result2)
for (i in 1:length(ppp_obj_list_marijuana)){
	ripley_results_marijuana[[i]] <- Kest(ppp_obj_list_marijuana[[i]])
}

plot(ripley_results_marijuana[[1]], main="Marijuana Ripleys")
for (i in 2:length(ripley_results_marijuana)){
	plot(ripley_results_marijuana[[i]],add=TRUE)
}

#cumulative schools ripley k function
circ_owin_list_schools <- list(circ_owin1, circ_owin2)
schoolsCut <- dplyr::filter(masterDF, type %in% "4")
schoolslatLong <- cbind(schoolsCut$latitude, schoolsCut$longitude)
for (i in 1:(nrow(schoolslatLong))){
	circ_owin_list_schools[[i]] <- disc(centre = c(schoolslatLong[i,2], schoolslatLong[i,1]), radius = 0.00898315/2, mask = FALSE,"Spatial")
}

ppp_obj1 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_schools[[1]])
ppp_obj2 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_schools[[2]])
ppp_obj_list_schools <- list(ppp_obj1, ppp_obj2)
for (i in 1:length(circ_owin_list_schools)){
	ppp_obj_list_schools[[i]] <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_schools[[i]])
}

ripley_result1 <- Kest(ppp_obj_list[[1]])
ripley_result2 <- Kest(ppp_obj_list[[2]])
ripley_results_schools <- list(ripley_result1, ripley_result2)
for (i in 1:length(ppp_obj_list_schools)){
	ripley_results_schools[[i]] <- Kest(ppp_obj_list_schools[[i]])
}

plot(ripley_results_schools[[1]], main="Schools Ripleys")
for (i in 2:length(ripley_results_schools)){
	plot(ripley_results_schools[[i]],add=TRUE)
}

#cumulative parks ripley k function
circ_owin_list_parks <- list(circ_owin1, circ_owin2)
parksCut <- dplyr::filter(masterDF, type %in% "5")
parkslatLong <- cbind(parksCut$latitude, parksCut$longitude)
for (i in 1:(nrow(parkslatLong))){
	circ_owin_list_parks[[i]] <- disc(centre = c(parkslatLong[i,2], parkslatLong[i,1]), radius = 0.00898315/2, mask = FALSE,"Spatial")
}

ppp_obj1 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_parks[[1]])
ppp_obj2 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_parks[[2]])
ppp_obj_list_parks <- list(ppp_obj1, ppp_obj2)
for (i in 1:length(circ_owin_list_parks)){
	ppp_obj_list_parks[[i]] <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_parks[[i]])
}

ripley_result1 <- Kest(ppp_obj_list[[1]])
ripley_result2 <- Kest(ppp_obj_list[[2]])
ripley_results_parks <- list(ripley_result1, ripley_result2)
for (i in 1:length(ppp_obj_list_parks)){
	ripley_results_parks[[i]] <- Kest(ppp_obj_list_parks[[i]])
}

plot(ripley_results_parks[[1]], main="Parks Ripleys")
for (i in 2:length(ripley_results_parks)){
	plot(ripley_results_parks[[i]],add=TRUE)
}

#cumulative worship ripley k function
circ_owin_list_worship <- list(circ_owin1, circ_owin2)
worshipCut <- dplyr::filter(masterDF, type %in% "6")
worshiplatLong <- cbind(worshipCut$latitude, worshipCut$longitude)
for (i in 1:(nrow(worshiplatLong))){
	circ_owin_list_worship[[i]] <- disc(centre = c(worshiplatLong[i,2], worshiplatLong[i,1]), radius = 0.00898315/2, mask = FALSE,"Spatial")
}

ppp_obj1 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_worship[[1]])
ppp_obj2 <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_worship[[2]])
ppp_obj_list_worship <- list(ppp_obj1, ppp_obj2)
for (i in 1:length(circ_owin_list_worship)){
	ppp_obj_list_worship[[i]] <- as.ppp((matrixP1CrimeslongLat), W = circ_owin_list_worship[[i]])
}

ripley_result1 <- Kest(ppp_obj_list[[1]])
ripley_result2 <- Kest(ppp_obj_list[[2]])
ripley_results_worship <- list(ripley_result1, ripley_result2)
for (i in 1:length(ppp_obj_list_worship)){
	ripley_results_worship[[i]] <- Kest(ppp_obj_list_worship[[i]])
}

plot(ripley_results_worship[[1]], main="Worship Ripleys")
for (i in 2:length(ripley_results_worship)){
	plot(ripley_results_worship[[i]],add=TRUE)
}

#plot experimentation on 4/4
xxx <- ripley_result1$iso
class(xxx)
dim(xxx)
length(xxx)
plot(xxx,type="l",lty=3)
plot(ripley_results[[1]], main="Tobacco Ripleys")
ripleytest <- Kest(ppp_obj_list[[1]],r=seq(0,0.0025,by=0.0001))
yyy <- ripleytest$iso
dev.new()
plot(seq(0,0.0025,by=0.0001),yyy)
plot(seq(0,0.0025,by=0.0001),yyy,type = "l")

#filtered isotropic cumulative tobacco ripley 
ntobacco <- length(ripley_results_tobacco)
r <- seq(0,0.0025,by=0.00001)
nr <- length(r)
ripleyKtobacco <- as.data.frame(matrix(0,nrow=ntobacco,ncol=nr))
for (i in 1:ntobacco){
	k <- Kest(ppp_obj_list_tobacco[[i]], r=r)
	ripleyKtobacco[i,] <- k$iso
	}
	plot(r,ripleyKtobacco[1,],type = "l", main="Isotropic Tobacco Ripleys")
for (i in 2: ntobacco){
	lines(r,ripleyKtobacco[i,])
	}
#create mean red line
rkm <- colMeans(ripleyKtobacco)
lines(r,rkm,col="red")
	
#filtered isotropic cumulative liquor ripley 
nliquor <- length(ripley_results_liquor)
r <- seq(0,0.0025,by=0.00001)
nr <- length(r)
ripleyKliquor <- as.data.frame(matrix(0,nrow=nliquor,ncol=nr))
for (i in 1: nliquor){
	k <- Kest(ppp_obj_list_liquor[[i]], r=r)
	ripleyKliquor[i,] <- k$iso
	}
	plot(r,ripleyKliquor[1,],type = "l", main="Isotropic Liquor Ripleys")
for (i in 2: nliquor){
	lines(r,ripleyKliquor[i,])
	}
#create mean red line
rkm <- colMeans(ripleyKliquor)
lines(r,rkm,col="red")	

#filtered isotropic cumulative marijuana ripley 
nmarijuana <- length(ripley_results_marijuana)
r <- seq(0,0.0025,by=0.00001)
nr <- length(r)
ripleyKmarijuana <- as.data.frame(matrix(0,nrow=nmarijuana,ncol=nr))
for (i in 1: nmarijuana){
	k <- Kest(ppp_obj_list_marijuana[[i]], r=r)
	ripleyKmarijuana[i,] <- k$iso
	}
	plot(r, ripleyKmarijuana[1,],type = "l", main="Isotropic Marijuana Ripleys")
for (i in 2: nmarijuana){
	lines(r,ripleyKmarijuana[i,])
	}
#create mean red line
rkm <- colMeans(ripleyKmarijuana)
lines(r,rkm,col="red")

#filtered isotropic cumulative schools ripley 
nschools <- length(ripley_results_schools)
r <- seq(0,0.0025,by=0.00001)
nr <- length(r)
ripleyKschools <- as.data.frame(matrix(0,nrow=nschools,ncol=nr))
for (i in 1:nschools){
	k <- Kest(ppp_obj_list_marijuana[[i]], r=r)
	ripleyKschools[i,] <- k$iso
	}
	plot(r, ripleyKschools[1,],type = "l", main="Isotropic Schools Ripleys")
for (i in 2:nschools){
	lines(r, ripleyKschools[i,])
	}
#create mean red line
rkm <- colMeans(ripleyKschools)
lines(r,rkm,col="red")	

#filtered isotropic cumulative parks ripley 
nparks <- length(ripley_results_parks)
r <- seq(0,0.0025,by=0.00001)
nr <- length(r)
ripleyKparks <- as.data.frame(matrix(0,nrow=nparks,ncol=nr))
for (i in 1:nparks){
	k <- Kest(ppp_obj_list_parks[[i]], r=r)
	ripleyKparks[i,] <- k$iso
	}
	plot(r, ripleyKparks[1,],type = "l", main="Isotropic Parks Ripleys")
for (i in 2:nparks){
	lines(r, ripleyKparks[i,])
	}
#create mean red line
ripleyKparks <- ripleyKparks[-13,]
rkm <- colMeans(ripleyKparks)
lines(r,rkm,col="red")	
#had to omit the 13th row due to all of them appearaing as NaN's, potential error

#filtered isotropic cumulative worship ripley 
nworship <- length(ripley_results_worship)
r <- seq(0,0.0025,by=0.00001)
nr <- length(r)
ripleyKworship <- as.data.frame(matrix(0,nrow= nworship,ncol=nr))
for (i in 1: nworship){
	k <- Kest(ppp_obj_list_worship[[i]], r=r)
	ripleyKworship[i,] <- k$iso
	}
	plot(r, ripleyKworship[1,],type = "l", main="Isotropic Worship Ripleys")
for (i in 2: nworship){
	lines(r, ripleyKworship[i,])
	}
#create mean red line
rkm <- colMeans(ripleyKworship)
lines(r,rkm,col="red")	




	