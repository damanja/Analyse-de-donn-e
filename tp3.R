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
  M <- matrix(rep(NA,m),m, m)
  for(i in 1:m){
    j=1
    while(j<i){
      x1 <- cbind(X[i], Y[i])
      x2 <- cbind(X[j], Y[j])
      M[i,j] <- dist_euclid( x1, x2 )
      j <- j+1
    }
  }
  return(M)
}

#nos classe de départ composé des singletons 
classe <<- cbind(x,y)

class_ascendate <- function(K){
  M<-matrice_moyenne(x,y) 
  m<- sqrt(length(M))
  n<- sqrt(length(M))
  C <- diag(m)
  #  M <- matrice_moyenne(x,y)
  #  for(i in 1:m){
  #   i_proche <- i + which.min(M[(i+1):m,i]) # on cherche le voisin le plus proche
  #  }
  
  while(m>K){
    
    #cherche indice du min
    ind <- which.min(M) -1
    i <- ind %% m +1 #ligne
    j <- floor(ind/ m)+1 #colonne
    
    #mise a jour de la matrice C
    C[i, ] <- C[i,] + C[j,]
    
    
    #calcul des nouvelle classe
    # classe[i,] <<- ( classe[i,] + classe[j,])/2
    x_temp <- 0
    y_temp <- 0
    
    for(k in 1:n){
      if(C[i,k]>0) {
        x_temp <- x_temp + x[k]
        y_temp <- y_temp + y[k]
      }
    }
    temp <- cbind(x_temp,y_temp)
    
    classe [i,] <<- temp / sum(C[i,])
    
    C <- C[-j,] 
    classe <<- classe[-j,] # on enlève les points qui ne nous intéressent plus
    
    #mettre a jour les distances
    M <- matrice_moyenne(classe[,1],classe[,2])
    
    m <- sqrt(length(M))
    
  }
  return (t(C))
}
res <- class_ascendate(3)
print(classe)
points(classe[,1],classe[,2],pch=25, bg="grey")
