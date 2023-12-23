#loading files
library(readxl)
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

lqp <- cbind(lngLQ,latLQ)
mjp <- cbind(lngMJ,latMJ)
tbp <- cbind(lngTB,latTB)
npp <- rbind(lqp,tbp,mjp)

pwp <- cbind(lngPW,latPW)
ppp <- cbind(lngPP,latPP)
scp <- cbind(lngSc,latSc)
ptp <- rbind(pwp,ppp,scp)

part1crimes <- cbind(lngPart1,latPart1)
part2crimes <- cbind(lngPart2,latPart2)
combinedCrimes <- rbind(part1crimes,part2crimes)

LQDataFrame <- read.csv("LQDataFrame.csv")
MJDataFrame <- read.csv("MJDataFrame.csv")
PPDataFrame <- read.csv("PPDataFrame.csv")
PWDataFrame <- read.csv("PWDataFrame.csv")
SCDataFrame <- read.csv("SCDataFrame.csv")
TBDataFrame <- read.csv("TBDataFrame.csv")
P1CrimeDataFrame <- read.csv("P1CrimeDataFrame.csv")
P2CrimeDataFrame <- read.csv("P2CrimeDataFrame.csv")

combinedFrame <- rbind(LQDataFrame,MJDataFrame,P1CrimeDataFrame,P2CrimeDataFrame,PPDataFrame,PWDataFrame,SCDataFrame,TBDataFrame)


#Creating 8 New Frames with crimePropNum column 
crimePropNum <- 0
for (x in 1:nrow(LQDataFrame)){
	crimePropNum[x] <- 1
	}
newLQDataFrame <- cbind(LQDataFrame,crimePropNum)
write.csv(newLQDataFrame,"/Users/Kevin/Desktop/newLQDataFrame.csv", row.names = FALSE)

crimePropNum <- 0
for (x in 1:nrow(MJDataFrame)){
	crimePropNum[x] <- 2
	}
newMJDataFrame <- cbind(MJDataFrame,crimePropNum)
write.csv(newMJDataFrame,"/Users/Kevin/Desktop/newMJDataFrame.csv", row.names = FALSE)

crimePropNum <- 0
for (x in 1:nrow(TBDataFrame)){
	crimePropNum[x] <- 3
	}
newTBDataFrame <- cbind(TBDataFrame,crimePropNum)
write.csv(newTBDataFrame,"/Users/Kevin/Desktop/newTBDataFrame.csv", row.names = FALSE)

crimePropNum <- 0
for (x in 1:nrow(PWDataFrame)){
	crimePropNum[x] <- 4
	}
newPWDataFrame <- cbind(PWDataFrame,crimePropNum)
write.csv(newPWDataFrame,"/Users/Kevin/Desktop/newPWDataFrame.csv", row.names = FALSE)

crimePropNum <- 0
for (x in 1:nrow(SCDataFrame)){
	crimePropNum[x] <- 5
	}
newSCDataFrame <- cbind(SCDataFrame,crimePropNum)
write.csv(newSCDataFrame,"/Users/Kevin/Desktop/newSCDataFrame.csv", row.names = FALSE)

crimePropNum <- 0
for (x in 1:nrow(PPDataFrame)){
	crimePropNum[x] <- 6
	}
newPPDataFrame <- cbind(PPDataFrame,crimePropNum)
write.csv(newLQDataFrame,"/Users/Kevin/Desktop/newPPDataFrame.csv", row.names = FALSE)

crimePropNum <- 0
for (x in 1:nrow(P1CrimeDataFrame)){
	crimePropNum[x] <- 7
	}
newP1CrimeDataFrame <- cbind(P1CrimeDataFrame,crimePropNum)
write.csv(newP1CrimeDataFrame,"/Users/Kevin/Desktop/newP1CrimeDataFrame.csv", row.names = FALSE)

crimePropNum <- 0
for (x in 1:nrow(P2CrimeDataFrame)){
	crimePropNum[x] <- 8
	}
newP2CrimeDataFrame <- cbind(P2CrimeDataFrame,crimePropNum)
write.csv(newP2CrimeDataFrame,"/Users/Kevin/Desktop/newP2CrimeDataFrame.csv", row.names = FALSE)

#New Combined Frame
newCombinedFrame <-rbind(newLQDataFrame,newMJDataFrame,newTBDataFrame,newPWDataFrame,newSCDataFrame,newPPDataFrame,newP1CrimeDataFrame,newP2CrimeDataFrame)

write.csv(newCombinedFrame,"/Users/Kevin/Desktop/newCombinedFrame.csv", row.names = FALSE)
