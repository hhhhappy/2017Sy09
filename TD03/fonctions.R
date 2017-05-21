
ceuc.app <- function(Xapp, zapp)
{
  zapp <- as.factor(zapp)
  donn <- cbind(Xapp,zapp)
  donn1 <- subset(donn,zapp == levels(zapp)[1])
  donn2 <- subset(donn,zapp == levels(zapp)[2])
  
  n1 <- length(donn1[,3])
  n2 <- length(donn2[,3])
  
  g1 <- c(sum(donn1[,1])/n1, sum(donn2[,1])/n2)
  g2 <- c(sum(donn1[,2])/n1, sum(donn2[,2])/n2)
  
  mu <- cbind(g1,g2)
}


ceuc.val <- function(mu, Xtst)
{
  dist <- distXY(Xtst, mu)
  len <- length(Xtst[,1])
  res <- rep(0,len)
  for(i in 1 : len){
    res[i] <- which.min(dist[i,])
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
    Ztst <- kppv.val(Xapp, zapp, nppv[i], Xval)
    cpt <- 0;
    for(j in length(Ztst)){
      if(Ztst[j] == zval[j]){
        cpt <- cpt + 1;
      }
    }
    if (cpt>max_cpt) {
      res <- nppv[i]
      max_cpt<-cpt
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

donn <- read.csv("donnees/Synth1-100.csv")
Xtst <- donn[,1:2]
ztst <- donn[,3]

# mu <- ceuc.app(Xapp, zapp)
# front.ceuc(Xapp, zapp, mu, 1000)


Kopt <- kppv.tune(Xapp, zapp, Xtst, ztst,2*(1:6)-1)

front.kppv(Xapp, zapp, Kopt, 1000)

