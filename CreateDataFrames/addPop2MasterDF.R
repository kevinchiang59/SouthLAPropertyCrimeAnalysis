library("sf")
library("ggplot2")

JDShapeFile <- read_sf("D:/LMU/Projects/TRDRP/Crime and Property Data for Ben/commondata/shp/sla-2019.shp")

masterDF$pop <- 0

pop <- JDShapeFile$TotalPopul
geo <- JDShapeFile$GEOID

for (ii in 1:length(pop)){

	indx <- which(as.numeric(masterDF$GEOID_short) == as.numeric(geo[ii]))
	
	masterDF$pop[indx] <- pop[ii]
	
}

