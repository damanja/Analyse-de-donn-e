#Recuperation des donnees
A <- read.delim("https://raw.githubusercontent.com/agusbudi/DataAnalysis/master/data1TP2.txt", header=TRUE)

#1. Affichage des donnees en 3 dimensions
library(car)
scatter3d(x=A$Stature, y=A$Poids, z=A$Taille, point.col="blue" , surface=FALSE)

#2. Calcule de la matrice centre B
B <-A
for(i in 1:length(A)){
  moyenne <- mean(A[[i]])
  B[[i]]<-B[[i]] - moyenne
}
#Calcul de la matrice de covariance V
V <- cov(A)

#3. Calcul des representation spectrales
valeur_propre<- eigen(V)$values
vecteur_propre<- eigen(V)$vectors

#4. les axes principaux
# les valeurs propres obtenu sont : 97.084062 22.183157  6.343892
# le premier axe principal est donc celui qui correspond à la valeur propre la plus grande, soit 97.084062.
# puis, le deuxième axe celui avec 22.183157

#5. Calcul du tableau C
C <-  as.matrix(B) %*% vecteur_propre
princomp(A)$scores

#On peut remarquer que par rapport à la fonction princomp(A)$scores,
#les 2 premieres colonnes ont les memes valeurs mais de signes different
#et la troisieme colonne, les deux requetes ont le meme resultat

#6. nuage de points avec trace du premier axe principal
library(plot3D)
library(plot3Drgl)
library(psych)
library(rgl)
a <- -150
b <- -300
scatter3D( x = c(a*vecteur_propre[1,1],b*vecteur_propre[1,1]),
           y =c(a*vecteur_propre[2,1],b*vecteur_propre[2,1]),
           z =c(a*vecteur_propre[3,1],b*vecteur_propre[3,1]), add=TRUE,type="l")
plotrgl()


#7. Representation des donnees  en dimension 2 sur le plan forme des deux premiers axes

# Comme les deux premiers colonnes de la matrice des vecteurs propres representent les deux premiers 
# axes principaux, les données seront donc présents dans les deux premières colonnes du tableau C
plot(C[,1],C[,2])


#8. On a une bonne représentation des données car ils sont bien espacé les unes par rapport aux autres.