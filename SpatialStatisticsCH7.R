library("spatial")
library("splancs")
library("spatstat")
library("spatialkernel")
library(maptools)

#Experimenting with spatial methods learned in the "Spatial Analysis with R" textbook for use in cleaned South LA Data later on

#Guide code I found (https://github.com/r-spatial/asdar-book.org/blob/master/docs/book2ed/sppa_mod.R)

#7.2 Packages
data(japanesepines)
summary(japanesepines)

spjpines <- as(japanesepines, "SpatialPoints")
summary(spjpines)

spjpines1 <- elide(spjpines, scale = TRUE, unitsq = TRUE)
summary(spjpines1)

pppjap <- as(spjpines1, "ppp")
summary(pppjap)


#could not load the code below due to SPATSTAT package not being loadable
#library(rgdal)
#spasthma <- readOGR(".", "spasthma")
#spbdry <- readOGR(".", "spbdry")
#spsrc <- readOGR(".", "spsrc")
#sproads <- readOGR(".", "sproads")
 
#7.3.2 G-Function: Distance to the Nearest Event
r <- seq(0, sqrt(2)/6, by = 0.005)
spjpines <- as(japanesepines, "SpatialPoints")
envpines <- envelope(as(spjpines, "ppp"), fun = Gest, r = r,
 nrank = 2, nsim = 99)

spred <- as(redwoodfull, "SpatialPoints")
envred <- envelope(as(spred, "ppp"), fun = Gest, r = r,
 nrank = 2, nsim = 99)

spcells <- as(cells, "SpatialPoints")
envcells <- envelope(as(spcells, "ppp"), fun = Gest,
 r = r, nrank = 2, nsim = 99)

Gresults <- rbind(envjap, envred, envcells)
Gresults <- cbind(Gresults, DATASET = rep(c("JAPANESE",
    "REDWOOD", "CELLS"), each = length(r)))
plot(Gresults$theo,Gresults$obs)
    
#7.3.3 F-Function: Distance from a point to the nearest event
#needed to modify r since the function was giving an error for the original value for "by"
r <- seq(0, sqrt(2)/6, by = 0.00195)
Fenvjap <- envelope(as(spjpines1, "ppp"), fun = Fest, r = r, nrank = 2, nsim = 99)
Fenvred <- envelope(as(spred, "ppp"), fun = Fest, r = r, nrank = 2, nsim = 99)
Fenvcells <- envelope(as(spcells, "ppp"), fun = Fest, r = r, nrank = 2, nsim = 99)
Fresults <- rbind(Fenvjap, Fenvred, Fenvcells)
Fresults <- cbind(Fresults, DATASET = rep(c("JAPANESE",
+     "REDWOOD", "CELLS"), each = length(r)))
plot(Fresults$theo,Fresults$obs)
#plots only show function lines and no data that it is fitting for

#7.4.3 Intensity Estimation
library(splancs)
mserw <- mse2d(as.points(coordinates(spred)), as.points(list(x = c(0, + 1, 1, 0), y = c(0, 0, 1, 1))), 100, 0.15)
bw <- mserw$h[which.min(mserw$mse)]
#not sure how plot of MSE vs Bandwidth was generated
poly <- as.points(list(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0)))
sG <- Sobj_SpatialGrid(spred, maxDim = 100)$SG
grd <- slot(sG, "grid")
summary(grd)

k0 <- spkernel2d(spred, poly, h0 = bw, grd)
k1 <- spkernel2d(spred, poly, h0 = 0.05, grd)
k2 <- spkernel2d(spred, poly, h0 = 0.1, grd)
k3 <- spkernel2d(spred, poly, h0 = 0.15, grd)
df <- data.frame(k0 = k0, k1 = k1, k2 = k2, k3 = k3)
kernels <- SpatialGridDataFrame(grd, data = df)
summary(kernels)
xy <- list(x = coordinates(kernels)[, 1], y = coordinates(kernels)[, 2])
k4 <- density(as(spred, "ppp"), 0.5 * bw, dimyx = c(100, 100), xy = xy)
kernels$k4 <- as(k4, "SpatialGridDataFrame")$v
k5 <- density(as(spred, "ppp"), 0.5 * 0.05, dimyx = c(100,
100), xy = xy)
kernels$k5 <- as(k5, "SpatialGridDataFrame")$v
k6 <- density(as(spred, "ppp"), 0.5 * 0.1, dimyx = c(100,
100), xy = xy)
kernels$k6 <- as(k6, "SpatialGridDataFrame")$v
k7 <- density(as(spred, "ppp"), 0.5 * 0.15, dimyx = c(100,
100), xy = xy)
kernels$k7 <- as(k7, "SpatialGridDataFrame")$v
summary(kernels)

#7.4.4 Likelihood of Inhomogenous Poisson Process
L <- function(alphabeta, x) {
	l <- apply(x, 1, loglambda, alpha = alphabeta[1], beta = alphabeta[-1]) 
	l <- sum(l)
	intL <- adapt(2, c(0, 0), c(1, 1), functn = function(x, alpha, beta) {exp(loglambda(x, alpha, beta))}, alpha = alphabeta[1], beta = alphabeta[-1]) 
	l <- l - intL$value 
	return(l)
	}
#could not load the code below due to "adapt" package not unavailable for my version
#data(lansing)
#x <- as.points(lansing[lansing$marks == "maple", ])
#optbeta <- optim(par = c(log(514), 0, 0, 0, 0, 0), fn = L, control = list(maxit = 1000, fnscale = -1), x = x)
#lmaple <- lansing[lansing$marks == "maple", ]
#ppm(Q = lmaple, trend = ~x + y + I(x^2) + I(y^2) + I(x * + y))

#7.4.5 Second Order Properties
Kenvjap <- envelope(as(spjpines1, "ppp"), fun = Kest, r = r, nrank = 2, nsim = 99)
Kenvred <- envelope(as(spred, "ppp"), fun = Kest, r = r, nrank = 2, nsim = 99)
Kenvcells <- envelope(as(spcells, "ppp"), fun = Kest, r = r, nrank = 2, nsim = 99)
Kresults <- rbind(Kenvjap, Kenvred, Kenvcells)
Kresults <- cbind(Kresults, DATASET = rep(c("JAPANESE", "REDWOOD", "CELLS"), each = length(r)))
#Will need to look into visualizing the results correctly by plots

#7.5.1 Case Control Studies: Spatial Variation of the Relative Risk
#Could not load the code below due to "spatialkernel" package not unavailable for my version (spbdry not found)
#sG <- Sobj_SpatialGrid(spbdry, maxDim = 50)$SG
#gt <- slot(sG, "grid")
#pbdry <- slot(slot(slot(spbdry, "polygons")[[1]], "Polygons")[[1]], "coords")

#Need to figure out where "spasthma" data is coming from, see above link to github
#library(splancs)
#cases <- spasthma[spasthma$Asthma == "case", ]
#ncases <- nrow(cases)
#controls <- spasthma[spasthma$Asthma == "control", ]
#ncontrols <- nrow(controls)
#kcases <- spkernel2d(cases, pbdry, h0 = bwasthma, gt)
#kcontrols <- spkernel2d(controls, pbdry, h0 = bwasthma, gt)