#loading Ripley files and transposing them for easier plotting
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

#plotting ripleys and overlapping lines
plot(transposeTobacco[,1],transposeTobacco[,2],pch=".",ylim=c(0,250))
for (i in 2:ncol(transposeTobacco)){
 	lines(transposeTobacco[,1],transposeTobacco[,i])
}

plot(transposeLiquor[,1], transposeLiquor[,2],pch=".",ylim=c(0,250))
for (i in 2:ncol(transposeLiquor)){
 	lines(transposeLiquor[,1], transposeLiquor[,i])
}

plot(transposeMarijuana[,1], transposeMarijuana[,2],pch=".",ylim=c(0,250))
for (i in 2:ncol(transposeMarijuana)){
 	lines(transposeMarijuana[,1], transposeMarijuana[,i])
}

plot(transposeSchools[,1], transposeSchools[,2],pch=".",ylim=c(0,250))
for (i in 2:ncol(transposeSchools)){
 	lines(transposeSchools[,1], transposeSchools[,i])
}

plot(transposeParks[,1], transposeParks[,2],pch=".",ylim=c(0,250))
for (i in 2:ncol(transposeParks)){
 	lines(transposeParks[,1], transposeParks[,i])
}

plot(transposeWorship[,1], transposeWorship[,2],pch=".",ylim=c(0,250))
for (i in 2:ncol(transposeWorship)){
 	lines(transposeWorship[,1], transposeWorship[,i])
}


