library(crsuggest)
library(sf)
library(tidyverse)
readRenviron("~/.Renviron")
library(censusapi)
library(tidycensus)
library("tigris")
library("leaflet")

#Number 1
lac_population <- get_acs(geography = "tract", variables = "B05006_150", state = "CA", county = "Los Angeles", year = 2020, geometry = TRUE)

ggplot(data = lac_population, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) + 
  labs(title = "  LA tracts by Population, 2020",
       caption = "Data source: 2020 1-year ACS, US Census Bureau",
       fill = "Density") + 
  theme_void()

#Number 2 
find_GEOID_density <- function(latitude, longitude, dataframe){

	#lac$INTPTLAT
	#lac$INTPTLON
	#lac$GEOID
	geo_number<- call_geolocator_latlon(latitude,longitude)
	short_geo_number = substring(geo_number,1,11)
	row_number = which(lac_population$GEOID == short_geo_number)
	pop_estimate = dataframe[row_number, 4]
	
	density <- pop_estimate
	
	myOutputs <- list()
	myOutputs$GEOID_true <- geo_number
	myOutputs$GEOID_short <- short_geo_number
	myOutputs$density<- density$estimate
	
	#change this so it only shows estimate up 
	return(myOutputs)
}

#test line
find_GEOID_density(34.2206887,-118.2400494,lac_population)

#ignore function below, was testing chunks
find_geoID <- function(latitude, longitude, dataframe){
	geo_number<- call_geolocator_latlon(latitude,longitude)
	short_geo_number = substring(geo_number,1,11)
	row_number = which(lac_population$GEOID == short_geo_number)
	return(row_number)
}

#is there a function in tidycensus that can return the GEOID of a lat-long
#list as output variable

#Number 3-4
ggplot(lac) + geom_sf() + xlim(118.4,118.2) + ylim(33.88,34.04)

south_square = ggplot(data = lac_population, aes(fill = estimate)) + 
   geom_sf() + xlim(118.4,118.2) + ylim(33.88,34.04) +
   scale_fill_distiller(palette = "Blues", 
                        direction = 1) + 
   labs(title = "  LA tracts by Population, 2020",
        caption = "Data source: 2020 1-year ACS, US Census Bureau",
        fill = "Density") + 
   theme_void()
south_square 

#Number 5 (unsuccessful plotting, may be due to multiple data layers or structure of data not matching, please see link in email for a potential way)
south_square + geom_point(aes(lat = 34.017, lon = -118.2888), color = "red", size = 2) 

df <- data.frame (INTPTLAT = c("34.017","34.3"), INTPTLON = c("-118.2888","-118.5"))
ggplot(lac) + geom_sf() + geom_point(data = df, aes(x = INTPTLAT, y = INTPTLON), color = "red", shape = 1)

#(closest code)
ggplot() + geom_sf(data = lac) + geom_sf(data = st_sfc(st_point(c(34.017,-118.2888)), crs = 4269), color = 'red') + theme_bw()

figure out 5.2 correctly 

for (x in 1:lqp_length){
	out <- find_GEOID_density(latLQ[x],lngLQ[x],lac_population)
	print(out)
}

find_GEOID_density_matrix <- function(latitude, longitude, dataframe){
	n = length(latitude)
	myMatrix <- matrix(0,nrow = n, ncol = 5)
	for (x in 1:n){
	output <- find_GEOID_density(latitude[x],longitude[x],dataframe)
	myMatrix[x,1] <- latitude[x]
	myMatrix[x,2] <- longitude[x]
	myMatrix[x,3] <- output$GEOID_true
	myMatrix[x,4] <- output$GEOID_short
	myMatrix[x,5] <- output$density
		}
		myOutput <- as.data.frame(myMatrix)
		names(myOutput) <- c("latitude", "longitude", "GEOID_true", "GEOID_short", "density")
		return(myOutput)
	}
	
	
add_Matrix <- function(dataframe,){
	
	append(dataframe, values, after = )
}
