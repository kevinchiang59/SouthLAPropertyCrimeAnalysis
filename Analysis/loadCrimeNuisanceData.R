dataFolder <- "D:/LMU/Projects/TRDRP/Crime and Property Data for Ben"

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
latSc    <- schlDF[,3];
lngSc    <- schlDF[,4]; 

plot(lngPart1,latPart1)
