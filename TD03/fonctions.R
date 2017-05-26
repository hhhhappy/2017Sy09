library(stats)
ceuc.app <- function(Xapp, zapp)
{
  zapp <- as.factor(zapp)
  donn <- cbind(Xapp,zapp)
  donn1 <- subset(donn,zapp == levels(zapp)[1])
  donn2 <- subset(donn,zapp == levels(zapp)[2])
  
  n1 <- length(donn1[,3])
  n2 <- length(donn2[,3])
  donn1 <- donn1[-c(ncol(donn1))]
  donn2 <- donn2[-c(ncol(donn2))]
  g1 <- colSums(donn1) / n1
  g2 <- colSums(donn2) / n2
  # g1 <- c(sum(donn1[,1])/n1, sum(donn2[,1])/n2)
  # g2 <- c(sum(donn1[,2])/n1, sum(donn2[,2])/n2)
  
  mu <- cbind(g1,g2)
}


ceuc.val <- function(mu, Xtst)
{
  dist <- distXY(t(mu), Xtst)
  len <- length(Xtst[,1])
  res <- rep(0,len)
  for(i in 1 : len){
    res[i] <- which.min(dist[,i])
  }
  returnValue(res)
}

kppv.val <- function(Xapp, zapp, K, Xtst)
{
  len <- length(Xtst[,1])
  res <- rep(0,len)
  dist <- distXY(as.matrix(Xapp), as.matrix(Xtst))
  for(i in 1:len){
    espece <- rep(0,K)
    for(j in 1:K){
      indice <- which.min(dist[, i])
      espece[j] <- zapp[indice]
      dist[indice, i] <- max(dist[, i])
    }
    
    n1 <- length(subset(espece, espece == 1 ))
    n2 <- length(subset(espece, espece == 2 ))
    if(n1< n2){
      res[i] <- 2
    }
    else{
      res[i] <- 1
    }
  }
  res
}

kppv.tune <- function(Xapp, zapp, Xval, zval, nppv)
{
  max_cpt <- 0
  res <- nppv[1]
  len <- length(nppv)
  for(i in 1:len){
    ztst <- kppv.val(Xapp, zapp, nppv[i], Xval)
    
    temp <- ztst == zval
    nTrue <- length(subset(temp, temp == TRUE))
    if (nTrue>max_cpt) {
      res <- nppv[i]
      max_cpt<-nTrue
    }
  }
  res
}

###Test example for the function kppv.val###
# zapp <- c(1,1,1,2,2,2)Xa
# Xapp <- as.matrix(cbind(c(1,3,3,7,8,6), c(1,2,1,5,5,6)))
# Xtst <- as.matrix(cbind(c(2.5, 3, 1, 5, 6, 7, 3), c(2.2, 2, 3, 6, 5.5, 5, 3)))
# res <- kppv.val(Xapp, zapp, 3, Xtst)
# 
# x11()
# plot(Xapp)
# text(Xapp, labels=zapp, cex= 0.7, pos=3)
# 
# x11()
# plot(Xtst)
# text(Xtst, labels=res, cex= 0.7, pos=3)

###Test example for the function kppv.tune###
donn <- read.csv("donnees/Synth1-40.csv")
Xapp <- donn[,1:2]
zapp <- donn[,3]

# 1.1.3 Test des fonctions
# mu <- ceuc.app(Xapp, zapp)
# front.ceuc(Xapp, zapp, mu, 1000)

# front.kppv(Xapp, zapp, 3, 1000)

donn <- read.csv("donnees/Synth1-100.csv")
Xtst <- donn[,1:2]
ztst <- donn[,3]
Kopt <- kppv.tune(Xapp, zapp, Xtst, ztst,2*(1:6)-1)
# front.kppv(Xapp, zapp, Kopt, 1000)

# Test d'erreur pour le classificateur euclidien
taux.erreur.euclid <- function(X, z, N)
{
  res <- rep(0, N)
  for(i in 1 : N){
    donn.sep <- separ1(X, z)
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    mu <- ceuc.app(Xapp, zapp)
    
    
    nTst <- ceuc.val(mu, Xtst)
    temp <- nTst == ztst
    nTrue <- length(subset(temp, temp == TRUE ))
    nFalse <- length(subset(temp, temp == FALSE ))
    res[i] <- nFalse / (nTrue + nFalse)
  }
  res
}


# 1.2 évaluation des performances
## Taux erreur kppv
taux.erreur.kppv <- function(X, z, N)
{
  res <- rep(0, N)
  kppv <- rep(0,N)
  for(i in 1 : N){
    donn.sep <- separ2(X, z)
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xval <- donn.sep$Xval
    zval <- donn.sep$zval
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    kppv[i] <- kppv.tune(Xapp, zapp, Xval, zval,(1:30))
    zres <- kppv.val(Xapp, zapp, kppv[i], Xtst)
    temp <- zres == ztst
    nTrue <- length(subset(temp, temp == TRUE ))
    nFalse <- length(subset(temp, temp == FALSE ))
    res[i] <- nFalse / (nTrue + nFalse)
  }
  res
}

pi.classes <- function(z){
  n1 <- length(subset(z, z == 1 ))
  n2 <- length(subset(z, z == 2 ))
  res <- rep(0, 2)
  res[1] <- n1/length(z)
  res[2] <- n2/length(z)
  res
}
mu.classes <- function(X, z){
  vect1 <- rep(0, 2)
  vect2 <- rep(0, 2)
  n1 <- length(subset(z, z == 1 ))
  n2 <- length(subset(z, z == 2 ))
  z <- as.factor(z)
  X <- cbind(X,z)
  donn1 <- subset(X,z == levels(z)[1])
  donn2 <- subset(X,z == levels(z)[2])
  vect1[1] <- sum(donn1[1]) / n1
  vect1[2] <- sum(donn1[2]) / n1
  vect2[1] <- sum(donn2[1]) / n2
  vect2[2] <- sum(donn2[2]) / n2
  res <- cbind(vect1, vect2)
}

var.classes <- function(X, z){
  mu <- mu.classes(X,z)
  z <- as.factor(z)
  X <- cbind(X,z)
  donn1 <- subset(X,z == levels(z)[1])
  donn2 <- subset(X,z == levels(z)[2])
  donn1 <- donn1[, 1:2]
  donn2 <- donn2[, 1:2]
  temp <- rep(0,2);
  temp <- t(temp) 
  print(temp)
  for(i in 1 : length(donn1)){
    temp <- temp + (donn1[i,] - mu[,1])*t((donn1[i,] - mu[,1]))
  }
  res <- temp / length(donn1)
  temp = 0;
  for(i in 1 : length(donn2)){
    temp <- temp+ (donn2[i,] - mu[,2])*t((donn2[i,] - mu[,2]))
  }
  res <- rbind(res, temp / length(donn2))
}

# test example for the function taux.erreur
donn <- read.csv("donnees/Synth1-40.csv")
Xapp <- donn[,1:2]
zapp <- donn[,3]
# tauxErreur <- taux.erreur(Xapp, zapp, 40, 3)
# tauxErreurEucl <- taux.erreur.euclid(Xapp, zapp, 40)
# kppv <- taux.erreur.kppv(Xapp, zapp, 40)
# the mean of error rate
# teMoyen <- sum(tauxErreur)/length(tauxErreur)*100
# teMoyenEucl <- sum(tauxErreurEucl)/length(tauxErreurEucl)*100
# intervalle de confiance

# donn <- read.csv("donnees/Synth1-40.csv")
# Xapp <- donn[,1:2]
# zapp <- donn[,3]
# pi <- pi.classes(zapp)
# mu <- mu.classes(Xapp, zapp)
# var <- var.classes(Xapp, zapp)
# 
# donn <- read.csv("donnees/Synth1-40.csv")
# Xapp <- donn[,1:2]
# zapp <- donn[,3]
# TEK40 <- taux.erreur.kppv(Xapp, zapp, 20)
# TEEC40 <- taux.erreur.euclid(Xapp, zapp, 20)
# teMoyen40 <- sum(TEK40)/length(TEK40)*100
# teMoyenEucl40 <- sum(TEEC40)/length(TEEC40)*100
# 
# 
# inter <- t.test(TEK40, conf.level = 0.90)
# 
# donn <- read.csv("donnees/Synth1-100.csv")
# Xapp <- donn[,1:2]
# zapp <- donn[,3]
# TEK100 <- taux.erreur.kppv(Xapp, zapp, 20)
# TEEC100 <- taux.erreur.euclid(Xapp, zapp, 20)
# teMoyen100 <- sum(TEK100)/length(TEK100)*100
# teMoyenEucl100 <- sum(TEEC100)/length(TEEC100)*100

# donn <- read.csv("donnees/Synth1-1000.csv")
# Xapp <- donn[,1:2]
# zapp <- donn[,3]
# pi <- pi.classes(zapp)
# mu <- mu.classes(Xapp, zapp)
# var <- var.classes(Xapp, zapp)
# TEK1000 <- taux.erreur.kppv(Xapp, zapp, 20)
# TEEC1000 <- taux.erreur.euclid(Xapp, zapp, 20)
# teMoyen1000 <- sum(TEK1000)/length(TEK1000)*100
# teMoyenEucl1000 <- sum(TEEC1000)/length(TEEC1000)*100

# donn <- read.csv("donnees/Synth2-1000.csv")
# Xapp <- donn[,1:2]
# zapp <- donn[,3]
# pi <- pi.classes(zapp)
# mu <- mu.classes(Xapp, zapp)
# var <- var.classes(Xapp, zapp)
# TEK1000 <- taux.erreur.kppv(Xapp, zapp, 20)
# TEEC1000 <- taux.erreur.euclid(Xapp, zapp, 20)
# teMoyen1000 <- sum(TEK1000)/length(TEK1000)*100
# teMoyenEucl1000 <- sum(TEEC1000)/length(TEEC1000)*100

donnPima <- read.csv("donnees/Pima.csv")
Xapp <- donnPima[,1 : (ncol(donnPima) - 1)]
zapp <- donnPima[,ncol(donnPima)]

pi <- pi.classes(zapp)
mu <- mu.classes(Xapp, zapp)
var <- var.classes(Xapp, zapp)
TEKPima <- taux.erreur.kppv(Xapp, zapp, 20)
TEECPima <- taux.erreur.euclid(Xapp, zapp, 20)
teMoyenPima <- sum(TEKPima)/length(TEKPima)*100
teMoyenEuclPima <- sum(TEECPima)/length(TEECPima)*100

donnBreastcancer <- read.csv("donnees/Breastcancer.csv")

