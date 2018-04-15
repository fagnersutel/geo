library(ggplot2)
library(OpenStreetMap)
library(ggmap)

nm = c("osm", "maptoolkit-topo",
       "waze", "mapquest", "mapquest-aerial",
       "bing", "stamen-toner", "stamen-terrain",
       "stamen-watercolor", "osm-german", "osm-wanderreitkarte",
       "mapbox", "esri", "esri-topo",
       "nps", "apple-iphoto", "skobbler",
       "opencyclemap", "osm-transport",
       "osm-public-transport", "osm-bbike", "osm-bbike-german")
par(mfrow=c(1,1))
for(i in 1:length(nm)){
    print(nm[i])
    map = openmap(c(lat= 38.05025395161289,   lon= -123.03314208984375),
                  c(lat= 36.36822190085111,   lon= -120.69580078125),
                  minNumTiles=9,type=nm[i])
    plot(map)
    title(nm[i],cex.main=4)
}


subscr<-data.frame(lon=c(-30.0517,-30.0507, -30.0301144, -30.0283829, -30.0457871),
                   lat=c(-51.1885,-51.2244,-51.1333589, -51.2032779, -51.2054645), 
                   pop=c(58,100, 150, 80, 110))
coordinates(subscr)<-~lat+lon
proj4string(subscr)<-CRS("+init=epsg:4326")
lat <- c(-30.324634,-29.945876)
lon <- c(-51.299088,-50.889990)
map <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),zoom=10,'osm')
plot(map)
points(spTransform(subscr,osm()))


tmp <- as.data.frame(spTransform(subscr, osm()))
symbols(y = tmp$lon, x = tmp$lat, circles = tmp$pop, add = TRUE,
        inches = 0.2, bg = "darkgreen")




map <- get_map(location = c(lon[1], lat[2], lon[2], lat[1]),
               maptype = "roadmap", source = "google", zoom = 11)
p <- ggmap(map) +
    geom_point(data = as.data.frame(subscr), aes(x = lat, y = lon, size=pop),
               colour = "darkgreen") +
    theme_bw()
print(p)



gcodes = geocode(iconv("ufrgs", to = "UTF-8"))
map <- openmap(c(lat = gcodes$lat + 0.005, lon = gcodes$lon - 0.01), c(lat = gcodes$lat - 
                                                                           0.005, lon = gcodes$lon + 0.005), type = "osm")
mapLatLon <- openproj(map)
g <- autoplot(mapLatLon)

g <- g + geom_point(data = gcodes, aes(lon, lat), size = 50, color = "red", 
                    fill = "pink", alpha = 0.3, shape = 20)
g + labs(title = "UFRGS") + theme(text = element_text(family = "Heiti TC Medium", 
                                                    size = 16))
