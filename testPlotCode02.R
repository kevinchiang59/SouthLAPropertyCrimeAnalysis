library(crsuggest)
library(sf)
library(tidyverse)
readRenviron("~/.Renviron")
library(censusapi)
library(tidycensus)
library("tigris")
library("leaflet")

myKey <- "f70a66fe4632924551ad3339d6d38a3b9576b6b7"

lac<-tracts(state="CA",county="Los Angeles")

leaflet(lac) %>% addTiles() %>% addPolygons(popup = ~NAME)

#census_api_key(myKey, install = TRUE)
census_api_key(myKey)

# lac_population <- get_acs(geography = "tract",variables = "B05006_150",state = "CA",year = 2020,geometry = TRUE)
# plot(lac_population)
lac_population <- get_acs(geography = "tract", variables = "B05006_150", state = "CA", county = "Los Angeles", year = 2020, geometry = TRUE)

dev.new()
ggplot(data = lac_population, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) + 
  labs(title = "  LA tracts by Population, 2020",
       caption = "Data source: 2020 1-year ACS, US Census Bureau",
       fill = "Density") + 
  theme_void()
  
#library(sf)
ca_counties <- counties("CA", cb = TRUE)
st_crs(ca_counties)
#install.packages("crsuggest")
#library(crsuggest)

#install.packages("tidyverse")
#library(tidyverse)
dev.new()
ggplot(ca_counties) + 
    geom_sf() + 
    coord_sf(crs = 4269, datum = 4269)

dev.new()
ggplot(lac) + 
   geom_sf()
#I believe crs and datum can manipulate the area plotted, just need the right numbers*

#dplyr filter()


#UPDATED CODE AFTER MEETING*

#Number 1*
lac_population <- get_acs(geography = "tract", variables = "B05006_150", state = "CA", county = "Los Angeles", year = 2020, geometry = TRUE)

dev.new()
ggplot(data = lac_population, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) + 
  labs(title = "  LA tracts by Population, 2020",
       caption = "Data source: 2020 1-year ACS, US Census Bureau",
       fill = "Density") + 
  theme_void()

#Number 2 (need code in {brackets} that will make the function read the data frame)*
find_geometry <- function(INTPTLAT,INTPTLON){
	return(geometry)
}
find_density_tractnum <-function(NAME){
	return(B05006_150)
}

#Number 3-4*
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

#Number 5 (unsuccessful plotting, may be due to multiple data layers or structure of data not matching, please see link in email for a potential way)*
south_square + geom_point(aes(lat = 34.017, lon = -118.2888), color = "red", size = 2) 

df <- data.frame (INTPTLAT = c("34.017","34.3"), INTPTLON = c("-118.2888","-118.5"))
ggplot(lac) + geom_sf() + geom_point(data = df, aes(x = INTPTLAT, y = INTPTLON), color = "red", shape = 1)

#(closest code)
# ggplot() + geom_sf(data = lac) + geom_sf(data = st_sfc(st_point(c(-118.2888,34.017)), crs = 4269), color = 'red') + xlim(118.4,118.2) + ylim(33.88,34.04) +theme_bw()

ggplot() + geom_point(data = part1DF,aes(x=longitude,y=latitude), color = 'red') + geom_sf(data = lac) + xlim(118.33,118.21) + ylim(33.9,34.05) +theme_bw()

south_square = ggplot(data = lac_population, aes(fill = estimate)) + 
   geom_sf() + xlim(-118.33,-118.21) + ylim(33.9,34.05) + 
   #geom_point(data = part1DF,aes(x=longitude,y=latitude), color = 'red') + 
   scale_fill_distiller(palette = "Blues", 
                        direction = 1) + 
   labs(title = "  LA tracts by Population, 2020",
        caption = "Data source: 2020 1-year ACS, US Census Bureau",
        fill = "Density") + 
   geom_point(data = part1DF,aes(x=lngR,y=latR),inherit.aes = FALSE, color = 'red') + 
   geom_point(data = liquorDF,aes(x=longitude,y=latitude),inherit.aes = FALSE, color = 'blue') + 
   geom_point(data = smokeDF,aes(x=longitude,y=latitude),inherit.aes = FALSE, color = 'yellow') + 
   geom_point(data = mjDF,aes(x=longitude,y=latitude),inherit.aes = FALSE, color = 'green') + 
   theme_void()
south_square 

south_square = ggplot(data = lac_population, aes(fill = estimate)) + 
   geom_sf() + xlim(-118.33,-118.21) + ylim(33.9,34.05) + 
   #geom_point(data = part1DF,aes(x=longitude,y=latitude), color = 'red') + 
   scale_fill_distiller(palette = "Blues", 
                        direction = 1) + 
   labs(title = "  LA tracts by Population, 2020",
        caption = "Data source: 2020 1-year ACS, US Census Bureau",
        fill = "Density") + 
   geom_point(data = part1DF,aes(x=longitude,y=latitude),inherit.aes = FALSE, color = 'red') + 
   theme_void()
south_square 


south_square = ggplot(data = p, aes(fill = TotalPopul)) + 
   geom_sf() + xlim(118.4,118.2) + ylim(33.88,34.04) +
   scale_fill_distiller(palette = "Blues", 
                        direction = 1) + 
   labs(title = "  LA tracts by Population, 2020",
        caption = "Data source: 2020 1-year ACS, US Census Bureau",
        fill = "Density") + 
   theme_void()
south_square 



















