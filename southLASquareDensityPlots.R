library(crsuggest)
library(sf)
library(tidyverse)
readRenviron("~/.Renviron")
library(censusapi)
library(tidycensus)
library("tigris")
library("leaflet")

lac_population <- get_acs(geography = "tract", variables = "B05006_150", state = "CA", county = "Los Angeles", year = 2020, geometry = TRUE)

ggplot(lac_population) + geom_sf() + xlim(118.4,118.2) + ylim(33.88,34.04)

dev.new()

south_square = ggplot(data = lac_population, aes(fill = estimate)) + 
   geom_sf() + xlim(118.4,118.2) + ylim(33.88,34.04) +
   scale_fill_distiller(palette = "Blues", 
                        direction = 1) + 
   labs(title = "  LA tracts by Population, 2020",
        caption = "Data source: 2020 1-year ACS, US Census Bureau",
        fill = "Density") + 
   theme_void()
south_square 
