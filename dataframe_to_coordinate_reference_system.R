library(mapview)
library(raster)
library(RColorBrewer)
library(sp)
# https://environmentalinformatics-marburg.github.io/mapview/advanced/advanced.html
pal <- colorRampPalette(brewer.pal(9, "BrBG"))

kili_data <- system.file("extdata", "kiliNDVI.tif", package = "mapview")
kili_data
kiliNDVI <- stack(kili_data)

mapview(kiliNDVI[[1]], col.regions = pal(100), at = seq(-0.2, 1, 0.2), legend = TRUE)

breweries91
mapview(breweries91, zcol = "founded", at = seq(1400, 2200, 200), legend = TRUE)

mapview(breweries91, map.types = c("CartoDB.DarkMatter", "OpenStreetMap.DE"), color = "grey40")


data(meuse)
head(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")

mapview(meuse, zcol = c("lead", "landuse", "elev"), 
        layer.name = c("Concentration of lead", "Lanuse type", "elevacao"))

head(parquimetros)
coordinates(parquimetros) <- ~V1+V2
proj4string(parquimetros) <- CRS("+init=epsg:4326")
names(parquimetros)
mapview(parquimetros, zcol = c("V5", "V3", "V7"), 
        layer.name = c("Bairro", "Vagas", "Situacao"))
