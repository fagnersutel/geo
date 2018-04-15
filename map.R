#https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
library(ggmap)
qmap(location = "boston university")
qmap(location = "boston university", zoom = 14)
qmap(location = "boston university", zoom = 14, source = "osm")

mydata = read.csv("vehicle-accidents.csv")
mydata$State <- as.character(mydata$State)
mydata$MV.Number = as.numeric(mydata$MV.Number)
mydata = mydata[mydata$State != "Alaska", ]
mydata = mydata[mydata$State != "Hawaii", ]

for (i in 1:nrow(mydata)) {
    latlon = geocode(mydata[i,1])
    mydata$lon[i] = as.numeric(latlon[1])
    mydata$lat[i] = as.numeric(latlon[2])
}

mv_num_collisions = data.frame(mydata$MV.Number, mydata$lon, mydata$lat)
colnames(mv_num_collisions) = c('collisions','lon','lat')

usa_center = as.numeric(geocode("United States"))
USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="normal")
circle_scale_amt = 0.01
USAMap + 
    geom_point(aes(x=lon, y=lat), data=mv_num_collisions, col="orange", alpha=0.4, size=mv_num_collisions$collisions*circle_scale_amt) +  
    scale_size_continuous(range=range(mv_num_collisions$collisions))

##########################################

mydata = read.csv("docs/data.csv", header = T, sep = ";")
names(mydata) <- c("lat", "lon", "MV.Number")
mydata
mydata$lat <- gsub(" ", ".", mydata$lat)
mydata$lon <- gsub(" ", ".", mydata$lon)
mydata


mv_num_collisions = data.frame(mydata$MV.Number, mydata$lon, mydata$lat)
mv_num_collisions
colnames(mv_num_collisions) = c('collisions','lon','lat')
usa_center = as.numeric(geocode("UFRGS"))

USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=12), extent="normal")
circle_scale_amt = 0.2
mv_num_collisions
mv_num_collisions$lon<-as.numeric(as.character(mv_num_collisions$lon))
mv_num_collisions$lat<-as.numeric(as.character(mv_num_collisions$lat))
mv_num_collisions

USAMap + 
    geom_point(aes(x=lon, y=lat), data=mv_num_collisions, col="red", alpha=0.4, size=mv_num_collisions$collisions*circle_scale_amt) +  
    scale_size_continuous(range=range(mv_num_collisions$collisions))


##########################################

mydata = read.csv("docs/data.csv", header = T, sep = ";")
names(mydata) <- c("lat", "lon", "MV.Number", "Classe")
mydata
mydata$lat <- gsub(" ", ".", mydata$lat)
mydata$lon <- gsub(" ", ".", mydata$lon)
mydata


mv_num_collisions = data.frame(mydata$MV.Number, mydata$lon, mydata$lat, mydata$Classe)
mv_num_collisions
colnames(mv_num_collisions) = c('collisions','lon','lat', 'class')
usa_center = as.numeric(geocode("UFRGS"))

USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=12), extent="normal")
circle_scale_amt = 0.2
mv_num_collisions
mv_num_collisions$lon<-as.numeric(as.character(mv_num_collisions$lon))
mv_num_collisions$lat<-as.numeric(as.character(mv_num_collisions$lat))
mv_num_collisions

USAMap + 
    geom_point(aes(x=lon, y=lat), data=mv_num_collisions, col="red", alpha=0.4, size=mv_num_collisions$collisions*circle_scale_amt) +  
    scale_size_continuous(range=range(mv_num_collisions$collisions)) 

