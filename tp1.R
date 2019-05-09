#On va récupérer les données dans le fichier data1TP1.txt 
mydata <- read.table("./data1TP1.txt", header=TRUE)

# Question 1

# Tracer les différents nuage de point en dimension 2 pour chaque variable

plot(mydata$A,mydata$Y)
plot(mydata$B,mydata$Y)
plot(mydata$C,mydata$Y)
plot(mydata$D,mydata$Y)
plot(mydata$E,mydata$Y)


 
# Question 2

# Calcul du coefficient r de Pearson:

pearsonR <- function(X,Y){ #covariance(X,Y)/(sigmaX* sigmaY)
  return (cov(X,Y)/(sd(X)*sd(Y)))
}

pearsonR(mydata$A,mydata$Y) # = -0.9722452
pearsonR(mydata$B,mydata$Y) # =  0.9815886
pearsonR(mydata$C,mydata$Y) # =  0.4119462
pearsonR(mydata$D,mydata$Y) # =  0.7513686
pearsonR(mydata$E,mydata$Y) # =  0.210302


# Question 3

#Calcul du coefficient de Spearman:

spearman <- function(X,Y){
  N <- length(X)
  rangX <- rank(X)
  rangY <- rank(Y)
  res <- 0
  for(i in 1:N){
    res <- (res + (rangX[i]-rangY[i])^2)

    }
  res <- res*6/(N^3-N)
  return(1-res)
}

spearman(mydata[[1]],mydata$Y) # -0.9973214
spearman(mydata[[2]],mydata$Y) # 0.9982143
spearman(mydata[[3]],mydata$Y) # 0.4169643
spearman(mydata[[4]],mydata$Y) # 1
spearman(mydata[[5]],mydata$Y) # 0.3419643

#une autre alternative de cette fonction est la fonction suivante qui est déjà introduite dans R
cor(mydata$D,mydata$Y, method = "spearman")



# Question 4

# pour calculer la relation non-linéaire et non-monotone entre les variables E et Y, 
# on pourrait essayer de changer nos données en utilisant un fonction de plongement par exemple



# Question 5

# on va récupérer dans un premier temps, nos nouvelles données qui sont dans data2TP1.txt
data2 <- read.table("./data2TP1.txt", header=TRUE)


#on va créer la fonction qui calcul le score t
score <- function(X,u){ #X correspond aux données que l'on va traiter
  m<- mean(X)     # correspond à la moyenne
  n <- length(X)  # la taille de nos données
  sigma <- sd(X)  #ecart type de nos données
  res <- abs(m-u)
  res <- res/(sigma/sqrt(n))
  return( res)
}

# Soit l'hypothèse H0: l'inflation 2010-2019 n'a pas affecté le coût de la vie à Marseille
score(data2$Marseille,19) # = 2.177369
# Après calcul du score , on obtient t = 2.177369
# Or, pour alpha = 5% et avec un degré de liberté k = 14, cela correspond dans notre table à 
# la valeur 2.145 < t.
# On rejette donc H0, c'est à dire que l'inflation a impacté le coût de la vie à Marseille



# Question 6 

# nouvelle fonction score

score2 <- function(X, Y){
  m1 <- mean(X)
  m2 <- mean(Y)
  t=abs(m1-m2)
  sigma1 <- sd(X)
  sigma2 <- sd(Y)
  n1 <- length(X)
  n2 <- length(Y)
  t <- t/sqrt(sigma1^2/n1 + sigma2^2/n2)
  return (t)
}


deg_libert <- length(data2$Marseille) + length(data2$Aix)-2 
#on obtient un degré de liberté k = 28

# H0 : Il n'existe pas de dépendance significative entre Marseille et Aix en Provence
score2(data2$Marseille,data2$Aix) # 2.321494

# pour alpha = 5%,cela correspond dans notre table à 2.048 < t 
# on accepte donc H0

# Mais pour alpha =2%, cela correspondrait plutôt à 2.468 > t
# on rejette donc H0 dans ce cas





# Test Non Parametrique
# Question 7

observe <- c(1528,106,117,381) #Nos données observé
n <- sum(observe) #total de plante observe

#a) calcul des valeurs théorique par rapport au ratio
valeur_theorique <- function(ratio){
  return(ratio*n/(16)) #16 = ratio total
}

vl <- valeur_theorique(9) #violet,long
vr <- valeur_theorique(3) #violet, rond
rl <- valeur_theorique(3) #rouge, long
rr <- valeur_theorique(1) #rouge, rond



#b) fonction khi deux
khi_deux <- function(O,E){
  res <- 0
  for(k in 1:length(O)){
    res <- res + ((O[k] - E[k])^2)/(E[k])
  }
  return(res)
}

E <- c(vl,vr,rl,rr) #nos valeurs théorique calculé précedemment

# c) Application de notre fonction de Khi deux
# H0 = le vrai ration est 9:3:3:1
khi_deux(observe,E)

#Cela nous donne comme valeur = 966.61
#Pour alpha = 5% avac un degré de liberté k = 3, cela correspond comme valeur: 7.81
# on a 7.81 << 966.61
# On rejette donc H0, ce qui signifie que le ratio 9:3:3:1 n'est pas respecté




#Question 8

data_diag_form <- c(29,5,46,40,32,8,18,22,0)
diagnosis <- c("common nevus", "atypical nevus", "melanoma")
form <- c("absent","atypical", "typical")

#la matrice qui fait correspondre le diagnostic et la forme
diag_form <- matrix(data_diag_form,nrow=3,byrow=TRUE, dimnames = list(diagnosis,form)) 


#une fonction qui calcul la valeur théorique 
calcul_val_theo <- function(O){ #O est la valeur obsérvé
  N <- sum (O)
  ligne <- dim(O)[1]
  col <- dim(O)[2]
  E <- matrix((1:length(O))*0, nrow=dim(O)[1])
  for(i in 1:ligne){
    for(j in 1:col){
      E[i,j] = sum(O[i,])*sum(O[,j])/N
    }
  }
  return(E)
}

#valeur théorique qui correspond aux données diagnostic-forme
diag_form_theo <- calcul_val_theo(diag_form)

#on a essayé d'adapter une nouvelle fonction de khi deux qui prends cette fois ci en paramètre une matrice

khi_deux_v2 <- function(O,E){
  res <- 0
  ligne <- dim(O)[1]
  col <- dim(O)[2]
  for(i in 1:ligne){
    for(j in 1:col){
      res <- res + ((O[i,j] - E[i,j])^2)/(E[i,j])  
    }
  }
  return(res)
}

color<- c("absent","present")
data_diag_color <- c(20,60,29,51,12,28)

#la matrice qui fait correspondre le diagnostic et la couleur
diag_color <- matrix(data_diag_color,nrow=3,byrow=TRUE, dimnames = list(diagnosis,color))

#valeur théorique correspondant à cela
diag_color_theo <- calcul_val_theo(diag_color)


#Appliquons maintenant la fonction Khi deux à nos différentes données
khi_deux_v2(diag_form,diag_form_theo)#75.1564
khi_deux_v2(diag_color,diag_color_theo) #2.39415

#Dans le cas où l'on considère les deux variables diagnostic et forme, le score obtenu est de t = 75.1564
# avec un alpha = 5% et un degré de liberté k = 2 * 2 = 4, cela correspond a la valeur 9.49< t, on refusera donc H0

#Sinon, Si on considère plutôt les variables diagnostic-couleur, on a un score de 2.39415,
# avec un alpha = 5% et k = 2, cela correspond a la valeur 5.99 > t, on acceptera donc dans ce cas H0

# Pour détecter un melanome, on devrait donc considérer plutôt la forme du mélanome car les variables diagnostic et forme sont 
# indépendantes après notre test de Khi Deux



#Conclusion

#Question 9

#Le test de Student/t est classé comme paramétrique car ce test se base sur l'ensemble de tous nos données tandis
# que le test du Khi Deux est classé comme non paramétrique car on se base juste sur l'intégralité des données mais
# on ne prendra pas en compte tout l'ensemble.
# De ce fait, on ne peut pas appliquer le test e Student/t aux données qualitatives car on n'a pas accès à tout.


#Question 10

#On ne peut pas appliquer le coefficient de Pearson et de Spearman aux données qualitatives car on n'a pas vraiment
# la notion de moyenne dans les données qualitatives