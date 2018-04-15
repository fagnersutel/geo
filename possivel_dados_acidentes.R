## required packages
#install.packages("mapview")
library(mapview)
library(sp)

## import 'meuse' dataset
data("meuse")

coordinates(meuse) <- ~ x + y
proj4string(meuse) <- CRS("+init=epsg:28992")

## create viewer (background layer 'OpenTopoMap')
mapview(meuse)
TESTE <- as.data.frame(meuse)
  TESTE