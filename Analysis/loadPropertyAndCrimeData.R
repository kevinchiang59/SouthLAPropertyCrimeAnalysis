library(readxl)
#
# Creates a list of 8 data frames, each extracted directly from csv data file
# frame contain latitude, longitude, GEOID_short (10 digit), and type
# type codes  1 = liquor stire
#             2 = smoke shop
#             3 = marijuana dispensary
#             4 = places of worship
#             5 = public parks
#             6 = schools
#             7 = Part I crimes
#             8 = Part II crimes
#
# warning: code is specific to the 8 data files
#          lots of *hard-coded* array indices and file names
#
dataFolder  <- "D:/LMU/Projects/TRDRP/Crime and Property Data for Ben"

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

iType       <- 2
dataFile    <- paste(dataFolder,"2019TobaccoShops.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,5];
longitude   <- dataDF[,6]; 
GEOID_short <- dataDF[,8];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))


iType       <- 3
dataFile    <- paste(dataFolder,"2019MarijuanaDispensaries.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,5];
longitude   <- dataDF[,6]; 
GEOID_short <- dataDF[,8];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

iType       <- 4
dataFile    <- paste(dataFolder,"2019PlacesOfWorship.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,18];
longitude   <- dataDF[,19]; 
GEOID_short <- dataDF[,21];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

iType       <- 5 
dataFile    <- paste(dataFolder,"2019PublicParksLatLng.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,3];
longitude   <- dataDF[,4]; 
GEOID_short <- dataDF[,18];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

iType       <- 6
dataFile    <- paste(dataFolder,"2019Schools.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,26];
longitude   <- dataDF[,27]; 
GEOID_short <- dataDF[,30];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

iType       <- 7
dataFile    <- paste(dataFolder,"2019PartICrime.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,6];
longitude   <- dataDF[,7]; 
GEOID_short <- dataDF[,12];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))

iType       <- 8
dataFile    <- paste(dataFolder,"2019PartIICrime.csv",sep="/")
dataDF      <- read.csv(dataFile);
latitude    <- dataDF[,6];
longitude   <- dataDF[,7]; 
GEOID_short <- dataDF[,12];
type        <- rep(iType,length(dataDF[,1]))

loadedDFs[[iType]] <- data.frame(latitude=latitude,longitude=longitude,GEOID_short=GEOID_short,type=type)
rm(list=c("latitude","longitude","GEOID_short","type"))
