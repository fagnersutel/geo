setwd("~/OneDrive/r-files/geo/")
library(sp)
# this code just creates a sample SpatialPointsDataFrame 
x <- c(-10,-10,10,10,-10)
y <- c(-10,10,10,-10,-10)
df.1 <- data.frame(x,y,id=47, order=1:5,hole=F,piece=1,group=47.1,box_id=1)
coordinates(df.1)=c("x","y")
x <- c(+15+3*cos(2*pi/5*(0:5)))
y <- c(-15+3*sin(2*pi/5*(0:5)))
df.2 <- data.frame(x,y,id=48, order=1:6,hole=F,piece=1,group=48.1,box_id=2)
coordinates(df.2)=c("x","y")
x <- c(-15+2*cos(2*pi/8*(0:8)))
y <- c(+15+2*sin(2*pi/8*(0:8)))
df.3.1 <- data.frame(x,y,id=20, order=1:9,hole=F,piece=1,group=20.1,box_id=3)
coordinates(df.3.1)=c("x","y")
x <- c(0+2*cos(2*pi/8*(0:8)))
y <- c(+15+2*sin(2*pi/8*(0:8)))
df.3.2 <- data.frame(x,y,id=20, order=1:9,hole=F,piece=1,group=20.2,box_id=3)
coordinates(df.3.2)=c("x","y")
x <- c(+15+2*cos(2*pi/8*(0:8)))
y <- c(+15+2*sin(2*pi/8*(0:8)))
df.3.3 <- data.frame(x,y,id=20, order=1:9,hole=F,piece=1,group=20.3,box_id=3)
coordinates(df.3.3)=c("x","y")

df <- rbind(df.1,df.2,df.3.1,df.3.2,df.3.3)
df
class(df)
data <- data.frame(box_id=unique(df$box_id),row.names=unique(df$id))
data

points2polygons <- function(df,data) {
  get.grpPoly <- function(group,ID,df) {
    Polygon(coordinates(df[df$id==ID & df$group==group,]))
  }
  get.spPoly  <- function(ID,df) {
    Polygons(lapply(unique(df[df$id==ID,]$group),get.grpPoly,ID,df),ID)
  }
  spPolygons  <- SpatialPolygons(lapply(unique(df$id),get.spPoly,df))
  SpatialPolygonsDataFrame(spPolygons,match.ID=T,data=data)
}
spDF <- points2polygons(df,data)
spDF
plot(spDF,col=spDF$box_id+1)

library(rgdal)
writeOGR(spDF,dsn=".",layer="myShapefile", driver="ESRI Shapefile")

rgdal::writeOGR(obj = spDF,
                dsn = "myShapefile.json",
                layer = "myShapefile",
                driver = "GeoJSON",
                overwrite_layer = TRUE)


dados = parquimetros[parquimetros$"cluster" == 113,  ]
dados
names(dados) = c("log", "lat", "lon", "v3", "v4", "bairro", "id", "v7", "group", "box_id")
dados
xy_final <- data.frame()
for (i in ch1){
  polying = dados[i,]
  xy_final = rbind(xy_final, polying)
  print(xy[1, ])
}
xy_final
dados = xy_final
coordinates(dados)=c("lat","lon")
df = dados
data <- data.frame(box_id=unique(df$box_id),row.names=unique(df$id))
data

points2polygons <- function(df,data) {
  get.grpPoly <- function(group,ID,df) {
    Polygon(coordinates(df[df$id==ID & df$group==group,]))
  }
  get.spPoly  <- function(ID,df) {
    Polygons(lapply(unique(df[df$id==ID,]$group),get.grpPoly,ID,df),ID)
  }
  spPolygons  <- SpatialPolygons(lapply(unique(df$id),get.spPoly,df))
  SpatialPolygonsDataFrame(spPolygons,match.ID=T,data=data)
}
spDF <- points2polygons(df,data)
spDF
plot(spDF,col=spDF$box_id+1)


library(rgdal)
rgdal::writeOGR(obj = spDF,
                dsn = "myParq.json",
                layer = "myParq.json",
                driver = "GeoJSON",
                overwrite_layer = TRUE)
