#http://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/

set.seed(212)
Data = matrix(rnorm(60), 30, 2)
Data <- cbind(geocoded$lat, geocoded$long)
Data
View(Data)
Data <- as.data.frame(Data)
Data$V1 <- as.numeric(as.character(Data$V1))
Data$V2 <- as.numeric(as.character(Data$V2))

#############################
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# apply it
center_scale(Data)


#############################
center_apply <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}

# apply it
center_apply(Data)



############################
# center with 'sweep()'
center_sweep <- function(x, row.w = rep(1, nrow(x))/nrow(x)) {
  get_average <- function(v) sum(v * row.w)/sum(row.w)
  average <- apply(x, 2, get_average)
  sweep(x, 2, average)
}

# apply it
center_sweep(Data)



############################################
# RECOMENDADO
############################################
## center with 'colMeans()'
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}

# apply it
center_colmeans(Data)




####################################
# center matrix operator
center_operator <- function(x) {
  n = nrow(x)
  ones = rep(1, n)
  H = diag(n) - (1/n) * (ones %*% t(ones))
  H %*% x
}

# apply it
center_operator(Data)



# mean subtraction
center_mean <- function(x) {
  ones = rep(1, nrow(x))
  x_mean = ones %*% t(colMeans(x))
  x - x_mean
}

# apply it
center_mean(Data)

