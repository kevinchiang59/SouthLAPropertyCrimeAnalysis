for (jj in 1:9){

	myVar 			    <- popDF[,jj]
    centered            <- myVar-mean(myVar)
	stdized             <- centered/sd(myVar)
	popDF[,jj+10]       <- stdized
	names(popDF)[jj+10] <- paste('std',names(popDF[jj]),sep="")
}

