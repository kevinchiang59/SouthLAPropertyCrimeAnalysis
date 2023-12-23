source("correctedRipley.R")

#Liquor Bootstrap with plot
nl <- nrow(ripleyKliquor)
for (b in 1:10000){
indx <- sample(seq(1,nl),nl,replace=TRUE)
rb <- ripleyKliquor
}
 
for (j in 1:nl){
rb[j,] <- ripleyKliquor[indx[j],]
}
 
rb <- ripleyKliquor[indx,]
rbm<- colMeans(rb)

quantileLiquor <- apply(rb,2,function(x)quantile(x,probs = c(.025,.975)))
plot(quantileLiquor[1,],ylim=c(0,4e-5), type="l", main = "Mean Bootstrapped Liquor Ripley Plot (red) with 95% CI (black)")
lines(quantileLiquor[2,])
rlm <- colMeans(ripleyKliquor)
lines(rlm,col="red")
dev.new()

 
#Tobacco Bootstrap
nl <- nrow(ripleyKtobacco)
for (b in 1:10000){
indx <- sample(seq(1,nl),nl,replace=TRUE)
rb <- ripleyKtobacco
}
 
for (j in 1:nl){
rb[j,] <- ripleyKtobacco[indx[j],]
}
 
rb <- ripleyKtobacco[indx,]
rbm<- colMeans(rb)

quantileTobacco <- apply(rb,2,function(x)quantile(x,probs = c(.025,.975)))
plot(quantileTobacco[1,],ylim=c(0,4e-5), type="l", main = "Mean Bootstrapped Tobacco Ripley Plot (red) with 95% CI (black)")
lines(quantileTobacco[2,])
rlm <- colMeans(ripleyKtobacco)
lines(rlm,col="red")
dev.new()


#Marijuana Bootstrap
nl <- nrow(ripleyKmarijuana)
for (b in 1:10000){
indx <- sample(seq(1,nl),nl,replace=TRUE)
rb <- ripleyKmarijuana
}
 
for (j in 1:nl){
rb[j,] <- ripleyKmarijuana[indx[j],]
}
 
rb <- ripleyKmarijuana[indx,]
rbm<- colMeans(rb)
 
quantileMarijuana <- apply(rb,2,function(x)quantile(x,probs = c(.025,.975)))
plot(quantileMarijuana[1,],ylim=c(0,4e-5), type="l", main = "Mean Bootstrapped Marijuana Ripley Plot (red) with 95% CI (black)")
lines(quantileMarijuana[2,])
rlm <- colMeans(ripleyKmarijuana)
lines(rlm,col="red")
dev.new()

 
#Schools Bootstrap
nl <- nrow(ripleyKschools)
for (b in 1:10000){
indx <- sample(seq(1,nl),nl,replace=TRUE)
rb <- ripleyKschools
}
 
for (j in 1:nl){
rb[j,] <- ripleyKschools[indx[j],]
}
 
rb <- ripleyKschools[indx,]
rbm<- colMeans(rb)
 
quantileSchools <- apply(rb,2,function(x)quantile(x,probs = c(.025,.975)))
plot(quantileSchools[1,],ylim=c(0,4e-5), type="l", main = "Mean Bootstrapped Schools Ripley Plot (red) with 95% CI (black)")
lines(quantileSchools[2,])
rlm <- colMeans(ripleyKschools)
lines(rlm,col="red")
dev.new()

 
#Worship Bootstrap
nl <- nrow(ripleyKworship)
for (b in 1:10000){
indx <- sample(seq(1,nl),nl,replace=TRUE)
rb <- ripleyKworship
}
 
for (j in 1:nl){
rb[j,] <- ripleyKworship[indx[j],]
}
 
rb <- ripleyKworship[indx,]
rbm<- colMeans(rb)
 
quantileWorship <- apply(rb,2,function(x)quantile(x,probs = c(.025,.975)))
plot(quantileWorship[1,],ylim=c(0,4e-5), type="l", main = "Mean Bootstrapped Worship Ripley Plot (red) with 95% CI (black)")
lines(quantileWorship[2,])
rlm <- colMeans(ripleyKworship)
lines(rlm,col="red")
dev.new()


#Parks Bootstrap
nl <- nrow(ripleyKparks)
for (b in 1:10000){
indx <- sample(seq(1,nl),nl,replace=TRUE)
rb <- ripleyKworship
}
 
for (j in 1:nl){
rb[j,] <- ripleyKworship[indx[j],]
}
 
rb <- ripleyKworship[indx,]
rbm<- colMeans(rb)

quantileParks <- apply(rb,2,function(x)quantile(x,probs = c(.025,.975)))
plot(quantileParks[1,],ylim=c(0,4e-5), type="l", main = "Mean Bootstrapped Parks Ripley Plot (red) with 95% CI (black)")
lines(quantileParks[2,])
rlm <- colMeans(ripleyKparks)
lines(rlm,col="red")

 
 