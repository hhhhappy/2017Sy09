Pima <- read.csv("Pima.csv", header=T)
Pima$z <- factor(Pima$z)

###Femme Diabétique 355-532
diabetique = Pima[Pima$z =="2",]
###Femme non diabétique 1-354
nDiabetique = Pima[Pima$z =="1",]
###1 est diabétique


dataPima <- Pima[,-8]
#centrage et mise à l'echelle
#centragePima <- scale(dataPima)

#covariance
######covariance entre les variables est très élevé
######Il faut le traitement préalable
covPima <- cov(centragePima)
print(covPima)

#diagonalisation
diagPima <- eigen(covPima)
valeurPima <- diagPima$values
vecteurPima <- diagPima$vectors
#print(diagPima)

#pourcentage d’inertie
somme <- sum(valeurPima)
pct <- valeurPima/somme
print(pct)
#la somme des 5 premières est 0.33092463+0.21433725+0.14386052+0.11429711+0.10242156 = 0.904
#donc on choisit AXE1, AXE2, AXE3, AXE4, AXE5

#Composant
C <- centragePima %*% vecteurPima
print(C)

# Test <- cbind(C, Pima[,8])
# x11()
# pairs(Test, col = ifelse(Test[,8] == 1, 'red','green'), main = "composants")

