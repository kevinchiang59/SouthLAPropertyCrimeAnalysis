library("sf")
library("ggplot2")
JDShapeFile <- read_sf("D:/LMU/Projects/TRDRP/Crime and Property Data for Ben/commondata/shp/sla-2019.shp")

#source("E:\\zdrive\\LMU\\Projects\\TRDRP\\src\\loadPropertyAndCrimeData.R")
source("D:\\LMU\\Projects\\TRDRP\\src\\loadPropertyAndCrimeData.R")
P2Crimecoord <- rbind(loadedDFs[[1]]$longitude,loadedDFs[[1]]$latitude)

south_square = ggplot(data = JDShapeFile, aes(fill = TotalPopul)) + 
   geom_sf() + xlim(118.33,118.225) + ylim(33.905,34.025) + 
   scale_fill_distiller(palette = "Blues", 
                        direction = 1) + 
   labs(title = "  LA tracts by Population, 2019",
        caption = "Data source: Jason Douglas",
        fill = "Pop Density") + 
		theme_bw() + geom_sf(data = st_sfc(st_zm(st_multipoint(t(P2Crimecoord))), crs = 4269), color = 'red', size = 1,inherit.aes = FALSE)
		
south_square 