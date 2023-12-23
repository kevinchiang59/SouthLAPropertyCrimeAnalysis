popLatDF           <- popDF
popLatDF$latitude  <- 0
popLatDF$longitude <- 0

for (ii in 1:length(pop)){

	indx 			   <- which( (as.numeric(masterDF$GEOID_short) == as.numeric(geo[ii])))
	myLat              <- mean(masterDF$latitude[indx])
	myLng              <- mean(masterDF$longitude[indx])	
	
	popLatDF$latitude[ii]  <- myLat
	popLatDF$longitude[ii] <- myLng
}


GWRBandwidth <- gwr.sel(data=popLatDF,PartI ~ Liquor + Tobacco + Marijuana + Population,coords=as.matrix(cbind(popLatDF$latitude,popLatDF$longitude)),adapt=TRUE)

beta <- ggwr(data=popLatDF,PartI ~ Liquor + Tobacco + Marijuana + Population,coords=as.matrix(cbind(popLatDF$latitude,popLatDF$longitude)),adapt=GWRBandwidth)
beta
