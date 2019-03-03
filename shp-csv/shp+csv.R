#http://neondataskills.org/R/csv-to-shapefile-R/
# load packages
library(rgdal)  # for vector work; sp package should always load with rgdal. 
library (raster)   # for metadata/attributes- vectors or rasters

# set working directory to data folder
setwd("~/OneDrive/r-files/geo/shp-csv/")
# Read the .csv file
plot.locations_HARV <- 
    read.csv("NEON-DS-Site-Layout-Files/HARV/HARV_PlotLocations.csv", stringsAsFactors = FALSE)

# look at the data structure
str(plot.locations_HARV)
# view column names
names(plot.locations_HARV)
# view first 6 rows of the X and Y columns
head(plot.locations_HARV$easting)
head(plot.locations_HARV$northing)
head(plot.locations_HARV[,1])
head(plot.locations_HARV[,2])
# view first 6 rows of the X and Y columns
head(plot.locations_HARV$geodeticDa)
head(plot.locations_HARV$utmZone)

# Import the line shapefile
lines_HARV <- readOGR( "NEON-DS-Site-Layout-Files/HARV/", "HARV_roads")
## OGR data source with driver: ESRI Shapefile 
## Source: "NEON-DS-Site-Layout-Files/HARV/", layer: "HARV_roads"
## with 13 features
## It has 15 fields
# view CRS
crs(lines_HARV)
## CRS arguments:
##  +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84
## +towgs84=0,0,0

# view extent
extent(lines_HARV)

# create crs object
utm18nCRS <- crs(lines_HARV)
utm18nCRS
class(utm18nCRS)

# note that the easting and northing columns are in columns 1 and 2
plot.locationsSp_HARV <- SpatialPointsDataFrame(plot.locations_HARV[,1:2],
                                                plot.locations_HARV,    #the R object to convert
                                                proj4string = utm18nCRS)   # assign a CRS 

# look at CRS
crs(plot.locationsSp_HARV)

# first, convert the data.frame to spdf
r <- SpatialPointsDataFrame(plot.locations_HARV[,1:2],
                            plot.locations_HARV)

# second, assign the CRS in one of two ways
r <- crs("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs 
         +ellps=WGS84 +towgs84=0,0,0" )
# or

crs(r) <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# plot spatial object
plot(plot.locationsSp_HARV, 
     main="Map of Plot Locations")

# create boundary object 
aoiBoundary_HARV <- readOGR("NEON-DS-Site-Layout-Files/HARV/",
                            "HarClip_UTMZ18")

# plot Boundary
plot(aoiBoundary_HARV,
     main="AOI Boundary\nNEON Harvard Forest Field Site")

# add plot locations
plot(plot.locationsSp_HARV, 
     pch=8, add=TRUE)

# no plots added, why? CRS?
# view CRS of each
crs(aoiBoundary_HARV)

## CRS arguments:
##  +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84
## +towgs84=0,0,0

crs(plot.locationsSp_HARV)

## CRS arguments:
##  +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84
## +towgs84=0,0,


# view extent of each
extent(aoiBoundary_HARV)

## class       : Extent 
## xmin        : 732128 
## xmax        : 732251.1 
## ymin        : 4713209 
## ymax        : 4713359

extent(plot.locationsSp_HARV)

## class       : Extent 
## xmin        : 731405.3 
## xmax        : 732275.3 
## ymin        : 4712845 
## ymax        : 4713846

# add extra space to right of plot area; 
# par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(extent(plot.locationsSp_HARV),
     col="purple", 
     xlab="easting",
     ylab="northing", lwd=8,
     main="Extent Boundary of Plot Locations \nCompared to the AOI Spatial Object",
     ylim=c(4712400,4714000)) # extent the y axis to make room for the legend

plot(extent(aoiBoundary_HARV), 
     add=TRUE,
     lwd=6,
     col="springgreen")

legend("bottomright",
       #inset=c(-0.5,0),
       legend=c("Layer One Extent", "Layer Two Extent"),
       bty="n", 
       col=c("purple","springgreen"),
       cex=.8,
       lty=c(1,1),
       lwd=6)


plotLoc.extent <- extent(plot.locationsSp_HARV)
plotLoc.extent

## class       : Extent 
## xmin        : 731405.3 
## xmax        : 732275.3 
## ymin        : 4712845 
## ymax        : 4713846

# grab the x and y min and max values from the spatial plot locations layer
xmin <- plotLoc.extent@xmin
xmax <- plotLoc.extent@xmax
ymin <- plotLoc.extent@ymin
ymax <- plotLoc.extent@ymax

# adjust the plot extent using x and ylim
plot(aoiBoundary_HARV,
     main="NEON Harvard Forest Field Site\nModified Extent",
     border="darkgreen",
     xlim=c(xmin,xmax),
     ylim=c(ymin,ymax))

plot(plot.locationsSp_HARV, 
     pch=8,
     col="purple",
     add=TRUE)

# add a legend
legend("bottomright", 
       legend=c("Plots", "AOI Boundary"),
       pch=c(8,NA),
       lty=c(NA,1),
       bty="n", 
       col=c("purple","darkgreen"),
       cex=.8)


# write a shapefile
writeOGR(plot.locationsSp_HARV, getwd(),
         "PlotLocations_HARV", driver="ESRI Shapefile")

