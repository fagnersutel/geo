gctorture (FALSE)
rm(list = ls(all.names = TRUE))
library(leaflet.extras)
library(apcluster)
setwd("~/OneDrive/r-files/AffinityPropagationClustering/")
getwd()
load(file = "apres2.rda")
load(file = "x2-13000.rda")

head(x2)
dim(x2)
summary(apres)

plot(apres, x2)

predict.apcluster <- function(s, exemplars, newdata)
{
  simMat <- s(rbind(exemplars, newdata), sel=(1:nrow(newdata)) + nrow(exemplars))[1:nrow(exemplars), ]
  unname(apply(simMat, 2, which.max))
}


resultado <- list()


parq = read.csv('tipoSinal.csv', header = FALSE, sep = ",")
head(parq)
parq = parq[parq$V1 < 1006 , ]
parq$V5 = as.integer(parq$V5)
parq$V7 = NULL
parq$V8 = NULL
parq$V9 = NULL
parq$V10 = NULL
parq$V11 = NULL
parq$V12 = NULL
parq$V13 = NULL
parq$V14 = NULL
parq$V15 = NULL
parq$V16 = NULL
parq$V17 = NULL
parq$V18 = NULL
parq$V19 = NULL
parq$V20 = NULL
parq$V1 = NULL


parquiativo = parq[parq$V23 == "Parquimetro Ativo", ]
parq$V23 = as.character(parq$V23)
dim(parquiativo)
head(parquiativo)
parquiretirado = parq[parq$V23 == "Parquimetro Retirado", ]
dim(parquiretirado)
head(parq)

parquimetros = rbind(parquiativo, parquiretirado)
dim(parquimetros)
head(parquimetros)

areas = unique(parquimetros$V21)
print(areas)

parquimetros$V3 <- as.numeric(as.character(parquimetros$V3))
parquimetros$V4 <- as.numeric(as.character(parquimetros$V4))
parquimetros <- parquimetros[parquimetros$V3 < 0, ]
parquimetros <- subset(parquimetros, !is.na(V3))
head(parquimetros)

dim(parquimetros)

parquimetros$cluster = 0
head(parquimetros)
parquimetros = parquimetros[1:length(parquimetros$V3),]
head(parquimetros)
names(parquimetros) = c("V0", "V1", "V2", "V3", "V4", "V5", "V6", "V7")
head(parquimetros)

aa= length(parquimetros$V3)/10
aa = aa+1
aa

resultado = predict.apcluster(negDistMat(r=2), x2[apres@exemplars, ],  parquimetros[1:210, 2:3])
parquimetros$cluster[1:210] = resultado
length(resultado)
head(parquimetros)

dados = parquimetros
head(dados)

meucluster <- function(cluster) {
  dadosc = dados[dados$cluster == cluster,]
  tamanho = length(dados$cluster[dados$cluster==cluster])
  leaflet(dadosc) %>%
    addTiles(group="OSM") %>% 
    addCircles(~V1, ~V2, weight = 0.1, radius=8, color= 'blue',
               stroke = TRUE, fillOpacity = 0.8) %>% 
    addLegend("topright", colors= "blue", labels=paste("com", tamanho, "Parquímetros", sep = " "), title=paste("Cluster nº", cluster, sep = " "))
}


clusters_encontrados = unique(dados$cluster)
clusters_encontrados = sort(clusters_encontrados)
length(clusters_encontrados)
clusters_encontrados

pal <- colorFactor(
  palette = 'Dark2',
  domain = parquimetros$cluster
)

leaflet(parquimetros) %>%
  addTiles(group="Mapa") %>% 
  addCircles(group="Parquimetros", ~V1, ~V2, weight = 0.1, radius=30, color=~pal(cluster),
             stroke = TRUE, fillOpacity = 0.8, popup=~paste("Cluster: ", cluster,  sep = " ")) %>% 
  addLegend(group="Legenda", "topright", colors= "", labels=paste("Classificados em meio a ", summary(apres)[1], "Clusters"), title="Parquímetros em Porto Alegre") %>% 
  addLayersControl(overlayGroups = c("Mapa", "Parquimetros", "Legenda"),
                   options = layersControlOptions(collapsed = TRUE))


parquimetros
poly = data.frame()
for (i in clusters_encontrados){
  temp = parquimetros[parquimetros$"cluster" == i,  ]
  ch1 = convexHullAM_Indexes(temp[,2],temp[,3], includeColinear=FALSE,zeroBased = FALSE)
  print(i)
  print(ch1)
  for (ii in ch1) {
    polying = temp[ii,]
    poly = rbind(poly, polying)
  }
  
}
poly

dados = poly
names(dados) = c("log", "lat", "lon", "v3", "v4", "group", "id1", "v7", "id", "box_id")
dados$id = (dados$box_id * 11)


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


nycounties <- geojsonio::geojson_read("myParq.json",
                                      what = "sp")
nycounties

library(leaflet)
#pal <- colorNumeric("Blues", NULL)
pal <- colorNumeric(c("red", "green", "blue", "pink", "brown"), c(clusters_encontrados))
leaflet(nycounties) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.4, fillColor = ~pal(clusters_encontrados), popup=nycounties)


setwd("~/OneDrive/r-files/AffinityPropagationClustering/")
library(mapview)
library(geojsonio)
dat <- geojson_read("myParq.json", what = "sp")
mapview(dat)

leaflet() %>% 
  addTiles() %>%
  addPolygons(data = dat, popup = popupTable(dat))

pal <- colorFactor(
  palette = 'Dark2',
  domain = dados$cluster
)

pal2 <- colorFactor(
  palette = 'Blues',
  domain = dat$box_id
)
leaflet(dados) %>%
  addTiles(group="OSM") %>% 
  addCircles(group="Alvaras", ~V1, ~V2, weight = 0.1, radius=30, color=~pal(cluster),
             stroke = TRUE, fillOpacity = 0.8) %>% 
  addPolygons(group="AAE", data = dat, fillOpacity = 0.8) %>% 
  addLegend(group="Legenda", "topright", colors= "", labels=paste(summary(apres)[1], "Clusters"), title="Alvaras em Porto Alegre") %>% 
  addLayersControl(overlayGroups = c("Alvaras", "AAE", "Legenda"),
                   options = layersControlOptions(collapsed = FALSE))



require(MASS)
topo
topogeo <- as.geodata(topo)
names(topogeo)
topogeo

