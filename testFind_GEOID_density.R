library(crsuggest)
library(sf)
library(tidyverse)
readRenviron("~/.Renviron")
library(censusapi)
library(tidycensus)
library("tigris")
library("leaflet")

lac_population <- get_acs(geography = "tract", variables = "B05006_150", state = "CA", county = "Los Angeles", year = 2020, geometry = TRUE)


find_GEOID_density <- function(latitude, longitude, dataframe){
 
	geo_number<- call_geolocator_latlon(latitude,longitude)
	short_geo_number = substring(geo_number,1,11)
	row_number = which(lac_population$GEOID == short_geo_number)
	pop_estimate = dataframe[row_number, 4]
	
	density <- pop_estimate
	
	myOutputs <- list()
	myOutputs$GEOID_true <- geo_number
	myOutputs$GEOID_short <- short_geo_number
	myOutputs$density<- density$estimate
	
	return(myOutputs)
}

find_GEOID_density(34.2206887,-118.2400494,lac_population)