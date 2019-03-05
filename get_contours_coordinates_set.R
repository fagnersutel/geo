#https://cran.r-project.org/web/packages/contoureR/contoureR.pdf

library(contoureR)
library(ggplot2)
set.seed(1)
x = runif(100)
y = runif(100)

x_coord <- c(16.48438,  24.74609, 17.49512,  22.59277, 16.48438)
x_coord
y_coord <- c(59.736328125, 55.0341796875, 55.1220703125, 61.142578125, 59.736328125)
xy <- cbind(x_coord, y_coord)
xy <- as.data.frame(xy)
xy
xy$aa = "aa"
xy$bb = "bb"
xy$cc = "cc"
xy
class(xy)
ch1 = convexHullAM_Indexes(xy$x,xy$y, includeColinear=FALSE,zeroBased = FALSE)
ch1
ggplot(data.frame(xy$x_coord,xy$y_coord),aes(xy$x_coord,xy$y_coord)) +
  geom_point() +
  geom_path(data=data.frame(x,y)[ch1,],colour="red")
ch1

x <- x_coord
y <- y_coord
ch = convexHullAM_Indexes(x,y,includeColinear=FALSE,zeroBased = FALSE)
ggplot(data.frame(x,y),aes(x,y)) +
  geom_point() +
  geom_path(data=data.frame(x,y)[ch,],colour="red")
ch











library(sp)

Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
plot(SpP, col = 1:3, pbg="white")


Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,5,3,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2), 1:2)
plot(SpP, col = 1:3, pbg="white")



data(wrld_simpl)
plot(wrld_simpl, col='white', bg='grey9', border=NA)

limx <- c(-6,1)
limy <- c(49,60)
plot(wrld_simpl, xlim=limx, ylim=limy, col='white', bg='grey9', border=NA)

# Apply RDP algorithm with distance variable = 0.4 degrees
wrld_RDP <- gSimplify(wrld_simpl, 0.4, topologyPreserve=TRUE)

plot(wrld_RDP, xlim=limx, ylim=limy, col='white', bg='grey9', border=NA)




x_coord <- c(16.48438,  17.49512,  24.74609, 22.59277, 16.48438)
y_coord <- c(59.736328125, 55.1220703125, 55.0341796875, 61.142578125, 59.736328125)

x_coorda <- c(16.48438,  24.74609, 17.49512,  22.59277, 16.48438)
y_coorda <- c(59.736328125, 55.0341796875, 55.1220703125, 61.142578125, 59.736328125)


xym <- cbind(x_coord, y_coord)
xym

p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
plot(sps)













lng <- c(-51.74768, -51.74735, -51.74768, -51.74735)
lat <- c(-0.1838690, -0.1840984, -0.1840993, -0.1838682)
data <- data.frame(lng, lat)
area <- geosphere::areaPolygon(x = data)
area





# Contour Lines for a Function, Constrained to a limited domain
# Example of the provision of non-regular data
library(contoureR)
library(ggplot2)
a  = -2; b = +2; n  = 150
x  = runif(n*n,a,b)
y  = runif(n*n,a,b)
df = data.frame(x,y)
df$z   = with(df,-x*y*exp(-x^2-y^2))
df.sub = subset(df,x^2 + y^2 < 2)
df.cnt = getContourLines(df.sub,nlevels=20)
ggplot(data=df.cnt,aes(x,y,group=Group,colour=z)) + geom_path() + theme_bw()


df$z = with(df,-x*y*exp(-x^2-y^2))
df.sub = subset(df,x^2 + y^2 < 2)
df.cnt = getContourLines(df.sub,nlevels=3)
ggplot(data=df.cnt,aes(x,y,group=Group,colour=z)) + geom_path() + theme_bw()


n = 100
x = runif(n)
y = runif(n)
df = expand.grid(x,y);
colnames(df) = c("x","y")
df$z = df$x^2 + df$y^2
dm = getDelaunayMesh(df$x,df$y)
res = contourWalker(dm,as.matrix(df),levels=pretty(df$z,n=20))
res = data.frame(res); colnames(res) = c('LID','GID','PID','x','y','z')
res$Group = interaction(res$LID,res$GID)
library(ggplot2)
ggplot(res,aes(x,y,group=Group,colour=z)) + geom_path()



x = runif(100)
y = runif(100)
ch = convexHullAM_Indexes(x,y,includeColinear=FALSE,zeroBased = FALSE)
ggplot(data.frame(x,y),aes(x,y)) +
  geom_point() +
  geom_path(data=data.frame(x,y)[ch,],colour="red")




data(volcano)
x = 1:nrow(volcano)
y = 1:ncol(volcano)
z = expand.grid(x=x,y=y); z$z = apply(z,1,function(xx){ volcano[ xx[1],xx[2] ]} )
df = getContourLines(z)
ggplot(df,aes(x,y,group=Group,colour=z)) + geom_path()
# Contour Lines for a Function
library(ggplot2)
a = -2; b = 2; n = 75
x = y = seq(a,b,by=diff(c(a,b))/(n+1))
df = expand.grid(x=x,y=y)
df$z = with(df,-x*y*exp(-x^2-y^2))
df.cnt = getContourLines(df)
ggplot(data=df.cnt,aes(x,y,group=Group,colour=z)) + geom_path()



set.seed(1)
x = runif(100)
y = runif(100)
ch = getConvexHull(x,y)
#To demonstrate, Lets view the hull
library(ggplot2)
df = data.frame(x,y)
ggplot(data=df,aes(x,y)) +
  geom_path(data=df[ch,]) +
  geom_point()

par(mfrow=c(2,2))

set.seed(1)
x = runif(100)
y = runif(100)
dm = getDelaunayMesh(x,y)
#To demonstrate, Lets view the mesh
library(ggplot2)
library(reshape)
df = as.data.frame(dm); df$id = 1:nrow(df); df = melt(df,id="id")
df = cbind(df,data.frame(x,y)[df$value,])
ggplot(data=df,aes(x,y,group=id)) +
  geom_polygon(aes(fill=id),color="gray")




set.seed(1)
x = runif(100)
y = runif(100)
op = orderPoints(x,y)
#To demonstrate, Lets view the points in order
library(ggplot2)
df = data.frame(x,y)
df = df[op,];
df$id = 1:nrow(df)
ggplot(data=df,aes(x,y,colour=id)) +
  geom_path() + geom_point() +
  scale_colour_gradient(low="green",high="red")
