require(spatialEco)
require(sp)
data(meuse)
meuse
coordinates(meuse) = ~x+y
meuse


sr1=Polygons(list(Polygon(cbind(c(180114, 180553, 181127, 181477, 181294, 181007, 180409,
                                  180162, 180114), c(332349, 332057, 332342, 333250, 333558, 333676,
                                                     332618, 332413, 332349)))),'1')
sr2=Polygons(list(Polygon(cbind(c(180042, 180545, 180553, 180314, 179955, 179142, 179437,
                                  179524, 179979, 180042), c(332373, 332026, 331426, 330889, 330683,
                                                             331133, 331623, 332152, 332357, 332373)))),'2')
sr3=Polygons(list(Polygon(cbind(c(179110, 179907, 180433, 180712, 180752, 180329, 179875,
                                  179668, 179572, 179269, 178879, 178600, 178544, 179046, 179110),
                                c(331086, 330620, 330494, 330265, 330075, 330233, 330336, 330004,
                                  329783, 329665, 329720, 329933, 330478, 331062, 331086)))),'3')
sr4=Polygons(list(Polygon(cbind(c(180304, 180403,179632,179420,180304),
                                c(332791, 333204, 333635, 333058, 332791)))),'4')
sr=SpatialPolygons(list(sr1,sr2,sr3,sr4))
srdf=SpatialPolygonsDataFrame(sr, data.frame(row.names=c('1','2','3','4'), PIDS=1:4, y=runif(4)))

head(srdf@data)  # polygons
head(meuse@data) # points
plot(srdf)
points(meuse, pch=20)

