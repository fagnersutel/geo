#https://stackoverflow.com/questions/32622174/plotting-population-density-map-in-r-with-geom-point
library(ggplot2)
library(ggthemes)
library(scales)
library(ggmap)
library(MASS)
library(sp)
library(viridis)

pop <- read.csv("PopulationDensity1.csv", header=TRUE, stringsAsFactors=FALSE)
pop
#pop$LAT<- gsub(" ", ".", pop$LAT)
#pop$LONG <- gsub(" ", ".", pop$LONG)
# get density polygons
dens <- contourLines(
    kde2d(pop$LONG, pop$LAT, 
          lims=c(expand_range(range(pop$LONG), add=0.5),
                 expand_range(range(pop$LAT), add=0.5))))

# this will be the color aesthetic mapping
pop$Density <- 0

# density levels go from lowest to highest, so iterate over the
# list of polygons (which are in level order), figure out which
# points are in that polygon and assign the level to them

for (i in 1:length(dens)) {
    tmp <- point.in.polygon(pop$LONG, pop$LAT, dens[[i]]$x, dens[[i]]$y)
    pop$Density[which(tmp==1)] <- dens[[i]]$level
}

mapabase <- get_map(location="Porto Alegre", zoom=12, maptype="roadmap")
#mapabase <- get_map(location="@-51.140987,-30.114981", zoom=2, maptype="roadmap")

names(pop) <- c("x", "CODE", "LAT", "LONG", "POPULACAO", "DENSIDADE")

gg <- ggmap(mapabase, extent="normal")
gg <- gg + geom_point(data=pop, aes(x=LONG, y=LAT, color=DENSIDADE, size=POPULACAO))
gg <- gg + scale_color_viridis()
gg <- gg + theme_map()
gg <- gg +  theme_bw()
gg



library(ggplot2)
library(OpenStreetMap)
lat <- c(-30.324634,-29.945876)
lon <- c(-51.299088,-50.889990)
map <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),zoom=10,'osm')
mapLatLon <- openproj(map)
g2 <- autoplot(mapLatLon)
g2 <- g2 + geom_point(data=pop, aes(x=LONG, y=LAT, color=DENSIDADE, size=POPULACAO))
g2 <- g2 + scale_color_viridis()
g2 <- g2 + theme_map()
g2 <- g2 +  theme_bw()
g2





library(ggplot2)
library(ggthemes)
library(scales)
library(ggmap)
library(MASS)
library(sp)
library(viridis)

pop <- read.csv("~/Dropbox/PopulationDensity.csv", header=TRUE, stringsAsFactors=FALSE)

# get density polygons
dens <- contourLines(
    kde2d(pop$LONG, pop$LAT, 
          lims=c(expand_range(range(pop$LONG), add=0.5),
                 expand_range(range(pop$LAT), add=0.5))))

# this will be the color aesthetic mapping
pop$Density <- 0

# density levels go from lowest to highest, so iterate over the
# list of polygons (which are in level order), figure out which
# points are in that polygon and assign the level to them

for (i in 1:length(dens)) {
    tmp <- point.in.polygon(pop$LONG, pop$LAT, dens[[i]]$x, dens[[i]]$y)
    pop$Density[which(tmp==1)] <- dens[[i]]$level
}

mapabase <- get_map(location="Canada", zoom=3, maptype="terrain")

gg <- ggmap(mapabase, extent="normal")
gg <- gg + geom_point(data=pop, aes(x=LONG, y=LAT, color=Density))
gg <- gg + scale_color_viridis()
gg <- gg + theme_map()
gg <- gg + theme(legend.position="none")
gg

