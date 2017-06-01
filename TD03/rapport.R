library(stats)

## 1.1.1 Classifieur euclidien

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

## 1.1.2 K plus proches voisins

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

## les paramètres muk et Sigmak des distributions conditionnelles,
## ainsi que les proportions pik des classes.

parameters <- function(Xapp, zapp){
  res <- NULL
  res$pi1 <- nrow(Xapp[zapp==1,])/nrow(Xapp)
  res$pi2 <- nrow(Xapp[zapp==2,])/nrow(Xapp)
  res$mu1 <- apply(Xapp[zapp==1,], 2, mean)
  res$mu2 <- apply(Xapp[zapp==2,], 2, mean)
  res$sigma1 <- cov(Xapp[zapp==1,])
  res$sigma2 <- cov(Xapp[zapp==2,]) 
  res
}

## 1.2.1 taux d'erreur avec le classifieur euclidien
taux.erreur.euclid <- function(X, z, N)
{
  erreurTest <- rep(0, N)
  erreurApp <- rep(0, N)
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
    erreurTest[i] <- nFalse / (nTrue + nFalse)
    
    nApp <- ceuc.val(mu, Xapp)
    temp <- nApp == zapp
    nTrue <- length(subset(temp, temp == TRUE ))
    nFalse <- length(subset(temp, temp == FALSE ))
    erreurApp[i] <- nFalse / (nTrue + nFalse)
  }
  res$erreurTest <- erreurTest
  res$erreurApp <- erreurApp
  res$meanTest <- sum(erreurTest)/length(erreurTest)*100
  res$meanApp <- sum(erreurApp)/length(erreurApp)*100
  res$interTest <- t.test(erreurTest, conf.level = 0.90)
  res$interApp <- t.test(erreurApp, conf.level = 0.90)
  res
}

## 1.2.1 Nombre optimal de voisins lorsque Xapp = Xval
voisin.optimal <- function(X,z) 
{
  donn.sep <- separ2(X, z)
  Xapp <- donn.sep$Xapp
  zapp <- donn.sep$zapp
  Xval <- donn.sep$Xval
  zval <- donn.sep$zval
  Xtst <- donn.sep$Xtst
  ztst <- donn.sep$ztst
  kppv <- kppv.tune(Xapp, zapp, Xapp, zapp,(1:10))
}


## 1.2.1 Taux d'erreur avec les kppv
taux.erreur.kppv <- function(X, z, N)
{
  resApp <- rep(0, N)
  resTest <- rep(0,N)
  kppv <- rep(0,N)
  for(i in 1 : N){
    donn.sep <- separ2(X, z)
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xval <- donn.sep$Xval
    zval <- donn.sep$zval
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    kppv[i] <- kppv.tune(Xapp, zapp, Xval, zval,(1:10))

    zres <- kppv.val(Xapp, zapp, kppv[i], Xtst)
    temp <- zres == ztst
    nTrue <- length(subset(temp, temp == TRUE ))
    nFalse <- length(subset(temp, temp == FALSE ))
    resTest[i] <- nFalse / (nTrue + nFalse)
  
    napp <- kppv.val(Xapp,zapp,kppv[i], Xapp)
    temp <- napp == zapp
    nTrue <- length(subset(temp, temp == TRUE ))
    nFalse <- length(subset(temp, temp == FALSE ))
    resApp[i] <- nFalse / (nTrue + nFalse)
  }
  res$resApp <- resApp
  res$resTest <- resTest
  res$meanApp <- sum(resApp)/length(resApp)*100
  res$meanTest <- sum(resTest)/length(resTest)*100
  res$interApp <- t.test(resApp, conf.level = 0.90)
  res$interTest <- t.test(resTest, conf.level = 0.90)
  res
}

# donn1 <- read.csv("donnees/Synth1-40.csv")
# X1 <- donn1[,1:2]
# z1 <- donn1[,3]
# param1 <- parameters(X1,z1)
# euclid1 <- taux.erreur.euclid(X1,z1,20)
# kppv1 <- taux.erreur.kppv(X1,z1,20)
# voisinOpt1 <- voisin.optimal(X1,z1)

# donn2 <- read.csv("donnees/Synth1-100.csv")
# X2 <- donn2[,1:2]
# z2 <- donn2[,3]
# param2 <- parameters(X2,z2)
# euclid2 <- taux.erreur.euclid(X2,z2,20)
# voisinOpt2 <- voisin.optimal(X2,z2)
# kppv2 <- taux.erreur.kppv(X2,z2,20)

# donn3 <- read.csv("donnees/Synth1-500.csv")
# X3 <- donn3[,1:2]
# z3 <- donn3[,3]
# param3 <- parameters(X3,z3)
# euclid3 <- taux.erreur.euclid(X3,z3,20)
# voisinOpt3 <- voisin.optimal(X3,z3)
# kppv3 <- taux.erreur.kppv(X3,z3,20)

# donn4 <- read.csv("donnees/Synth1-1000.csv")
# X4 <- donn4[,1:2]
# z4 <- donn4[,3]
# param4 <- parameters(X4,z4)
# euclid4 <- taux.erreur.euclid(X4,z4,20)
# voisinOpt4 <- voisin.optimal(X4,z4)
# kppv4 <- taux.erreur.kppv(X4,z4,20)

donn5 <- read.csv("donnees/Synth2-1000.csv")
X5 <- donn5[,1:2]
z5 <- donn5[,3]
param5 <- parameters(X5,z5)
euclid5 <- taux.erreur.euclid(X5,z5,20)
kppv5 <- taux.erreur.kppv(X5,z5,20)