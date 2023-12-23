library(dplyr)

tobaccoRipleys <- read.csv("tobaccoRipleys.csv")
transposeTobacco <- t(tobaccoRipleys)
liquorRipleys <- read.csv("liquorRipleys.csv")
transposeLiquor <- t(liquorRipleys)
marijuanaRipleys <- read.csv("marijuanaRipleys.csv")
transposeMarijuana <- t(marijuanaRipleys)

schoolsRipleys <- read.csv("schoolsRipleys.csv")
transposeSchools <- t(schoolsRipleys)
parksRipleys <- read.csv("parksRipleys.csv")
transposeParks <- t(parksRipleys)
worshipRipleys <- read.csv("worshipRipleys.csv")
transposeWorship <- t(worshipRipleys)

#create master frames of ripleys
id <- rep(1,times=ncol(transposeTobacco))
idTobaccoRipleys <- cbind(tobaccoRipleys,id)
newTobaccoRipleys <- idTobaccoRipleys[-1,]

id <- rep(2,times=ncol(transposeLiquor))
idLiquorRipleys <- cbind(liquorRipleys,id)
newLiquorRipleys <- idLiquorRipleys[-1,]

id <- rep(3,times=ncol(transposeMarijuana))
idMarijuanaRipleys <- cbind(marijuanaRipleys,id)
newMarijuanaRipleys <- idMarijuanaRipleys[-1,]

combinedNuisanceRipleys <- bind_rows(newTobaccoRipleys, newLiquorRipleys, newMarijuanaRipleys)

id <- rep(4,times=ncol(transposeSchools))
idSchoolsRipleys <- cbind(schoolsRipleys,id)
newSchoolsRipleys <- idSchoolsRipleys[-1,]

id <- rep(5,times=ncol(transposeParks))
idParksRipleys <- cbind(parksRipleys,id)
newParksRipleys <- idParksRipleys[-1,]

id <- rep(6,times=ncol(transposeWorship))
idWorshipRipleys <- cbind(worshipRipleys,id)
newWorshipRipleys <- idWorshipRipleys[-1,]

combinedProtectiveRipleys <- bind_rows(newSchoolsRipleys, newParksRipleys, newWorshipRipleys)

combinedRipleys <- bind_rows(combinedNuisanceRipleys, combinedProtectiveRipleys)

#central tendencies of combinedRipleys
boxplot(data=combinedRipleys,combinedRipleys[,26]~as.factor(id))
boxplot(data=combinedRipleys,combinedRipleys[,51]~as.factor(id))
boxplot(data=combinedRipleys,combinedRipleys[,76]~as.factor(id))
boxplot(data=combinedRipleys,combinedRipleys[,101]~as.factor(id))

#individual central tendencies
median(transposeTobacco[26, 2:ncol(transposeTobacco)])
median(transposeTobacco[51, 2:ncol(transposeTobacco)])
median(transposeTobacco[76, 2:ncol(transposeTobacco)])
median(transposeTobacco[101, 2:ncol(transposeTobacco)])

median(transposeLiquor[26, 2:ncol(transposeLiquor)])
median(transposeLiquor[51, 2:ncol(transposeLiquor)])
median(transposeLiquor[76, 2:ncol(transposeLiquor)])
median(transposeLiquor[101, 2:ncol(transposeLiquor)])

median(transposeMarijuana[26, 2:ncol(transposeMarijuana)])
median(transposeMarijuana[51, 2:ncol(transposeMarijuana)])
median(transposeMarijuana[76, 2:ncol(transposeMarijuana)])
median(transposeMarijuana[101, 2:ncol(transposeMarijuana)])

median(transposeSchools[26, 2:ncol(transposeSchools)])
median(transposeSchools[51, 2:ncol(transposeSchools)])
median(transposeSchools[76, 2:ncol(transposeSchools)])
median(transposeSchools[101, 2:ncol(transposeSchools)])

median(transposeParks[26, 2:ncol(transposeParks)])
median(transposeParks[51, 2:ncol(transposeParks)])
median(transposeParks[76, 2:ncol(transposeParks)])
median(transposeParks[101, 2:ncol(transposeParks)])

median(transposeWorship[26, 2:ncol(transposeWorship)])
median(transposeWorship[51, 2:ncol(transposeWorship)])
median(transposeWorship[76, 2:ncol(transposeWorship)])
median(transposeWorship[101, 2:ncol(transposeWorship)])