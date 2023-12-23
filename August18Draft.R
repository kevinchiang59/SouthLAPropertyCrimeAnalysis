#loading frames and variables
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

LQDataFrame <- read_csv("LQDataFrame.csv")
MJDataFrame <- read_csv("MJDataFrame.csv")
PPDataFrame <- read_csv("PPDataFrame.csv")
PWDataFrame <- read_csv("PWDataFrame.csv")
SCDataFrame <- read_csv("SCDataFrame.csv")
TBDataFrame <- read_csv("TBDataFrame.csv")
P1CrimeDataFrame <- read_csv("P1CrimeDataFrame.csv")
P2CrimeDataFrame <- read_csv("P2CrimeDataFrame.csv")

combinedFrames <- rbind(LQDataFrame,MJDataFrame,P1CrimeDataFrame,P2CrimeDataFrame,PPDataFrame,PWDataFrame,SCDataFrame,TBDataFrame)

#making dataframe starts here
combinedFrames$GEOID_short <- as.numeric(combinedFrames$GEOID_short)
shortID <- unique(combinedFrames$GEOID_short)

createMasterFrame <- function(shortIDs){
	n = length(shortIDs)
	myMatrix <- matrix(0,nrow = n, ncol = 9)
	for (x in 1:n){
	myMatrix[x,1] <- shortIDs[x]
	myMatrix[x,2] <- length(which(as.numeric(LQDataFrame$GEOID_short) == shortIDs[x]))
	myMatrix[x,3] <- length(which(as.numeric(MJDataFrame$GEOID_short) == shortIDs[x]))
	myMatrix[x,4] <- length(which(as.numeric(TBDataFrame$GEOID_short) == shortIDs[x]))
	myMatrix[x,5] <- length(which(as.numeric(PWDataFrame$GEOID_short) == shortIDs[x]))
	myMatrix[x,6] <- length(which(as.numeric(SCDataFrame$GEOID_short) == shortIDs[x]))
	myMatrix[x,7] <- length(which(as.numeric(PPDataFrame$GEOID_short) == shortIDs[x]))
	myMatrix[x,8] <- length(which(as.numeric(P1CrimeDataFrame$GEOID_short) == shortIDs[x]))
	myMatrix[x,9] <- length(which(as.numeric(P2CrimeDataFrame$GEOID_short) == shortIDs[x]))
		}
		myOutput = as.data.frame(myMatrix)
		names(myOutput) <- c("GEOID_short","Liquor", "Marijuana", "Tobacco", "Places of Worship", "Schools", "Public Parks", "P1 Crimes", "P2Crimes")
		return(myOutput)
	}

