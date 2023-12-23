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

