set.seed(212)
Data = matrix(rnorm(60), 30, 2)
Data <- cbind(geocoded$lat, geocoded$long)
Data
View(Data)
Data <- as.data.frame(Data)
Data$V1 <- as.numeric(as.character(Data$V1))
Data$V2 <- as.numeric(as.character(Data$V2))
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# apply it
center_scale(Data)
