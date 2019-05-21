#1 generation de 300 donnees au total

#a) generation de x et de y uniforme sur [0,1]
x1 <- runif(100,0,1)
y1 <- runif(100,0,1)
plot(x1,y1)

#b) x et y gaussienne variance = 1, x_moyenne = 4 et y centree
x2 <- rnorm(100,4,1)
y2 <- rnorm(100,0,1)
plot(x2,y2)

#c) x et y gaussienne variance = 2 , x_mean = 0.5, y_mean = 6
x3 <- rnorm(100,0.5,sqrt(2))
y3 <- rnorm(100,6,sqrt(2))
plot(x3,y3)

#fusion des données
x <- c(x1,x2,x3)
y <- c(y1,y2,y3)

x_mini <-min( c(x1,x2,x3))
y_mini <- min(c(y1,y2,y3))
x_max <- max( c(x1,x2,x3))
y_max <- max(c(y1,y2,y3))
plot(c(x_mini,x_max),c(y_mini,y_max))

points(x1,y1,pch=21, bg="red")
points(x2,y2,pch=21, bg="green")
points(x3,y3,pch=21, bg="blue")


#Exo 2 : Classification non supervisée

dist_euclid <- function(x,y){
  return ( sqrt( (x[1]-y[1] )^2 + (x[2] - y[2])^2 ))
}

matrice_moyenne <- function(X,Y){
  m <- length(X)
  M <- matrix(rep(Inf,m),m, m)
  for(i in 1:m){
    for(j in 1:i){
      x1 <- cbind(X[i], Y[i])
      x2 <- cbind(X[j], Y[j])
      M[i,j] <- dist_euclid( x1, x2 )
    }
  }
  return(M)
}

M<-matrice_moyenne(x,y)
which.min(M[2:300,1])
min(M[60,1])
B <- diag(3)

class_ascendate <- function(K,m){
  C <- diag(m)
  #D <- dist(cbind(x,y), method="euclidean", diag=TRUE)
  M<-matrice_moyenne(x,y)
  min(M[,1])
  m <- 300
  #  M <- matrice_moyenne(x,y)
  for(i in 1:m){
      i_proche <- i + which.min(M[(i+1):m,i]) # on cherche le voisin le plus proche
      
  }
}

#calculer la distance Euclidean
D <- dist(cbind(x,y),method="euclidean")

#la fonction de classification ascendante hiérarchique
AscHierarchique <- hclust(D, method = "ward")

plot(AscHierarchique, cex = 0.6, hang = -1)

cluster = cutree(AscHierarchique,3)
