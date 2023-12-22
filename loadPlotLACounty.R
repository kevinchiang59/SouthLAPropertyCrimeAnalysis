library(crsuggest)
library(sf)
library(tidyverse)
readRenviron("~/.Renviron")
library(censusapi)
library(tidycensus)
library("tigris")
library("leaflet")

#loading and plotting all of LA County

lac_population <- get_acs(geography = "tract", variables = "B05006_150", state = "CA", county = "Los Angeles", year = 2020, geometry = TRUE)

ggplot(data = lac_population, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) + 
  labs(title = "  LA tracts by Population, 2020",
       caption = "Data source: 2020 1-year ACS, US Census Bureau",
       fill = "Density") + 
  theme_void() 