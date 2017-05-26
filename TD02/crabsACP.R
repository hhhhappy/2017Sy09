library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]


#########################################################################
#2.3.1	tous les crabs sans traitement préalable 

#centrage et mise à l'echelle
centrageCra <- scale(crabsquant)

#covariance
######covariance entre les variables est très élevé
######Il faut le traitement préalable, 
covCrab <- cov(centrageCra)
print(covCrab)

#diagonalisation
diagCrab <- eigen(covCrab)
valeurCrab <- diagCrab$values
vecteurCrab <- diagCrab$vectors
# print(diagCrab)

#pourcentage d’inertie
somme <- sum(valeurCrab)
pct <- valeurCrab/somme
#print(pct)
#la somme des deux première est 0.9577669569+0.0303370413 = 0.988
#donc on choisit AXE1 et AXE2 (ou seul AXE1)

#Composant
C <- centrageCra %*% vecteurCrab
#print(C)

#plot Axe c1 et c2
png(filename="F:/SY09/TD02/png/plotCrabsSansT_ACP_I.png")
xrange = range(c(C[,1]))
yrange = range(c(C[,2]))
# first plot
plot(C[1:50,1], C[1:50,2], col="red", xlab="Axe1", ylab="Axe2",main = "ACP sans traitement sur Crabs: les individus",
		xlim=xrange, ylim = yrange)

# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(C[51:100,1], C[51:100,2], col="black",
		xlim=xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(C[101:150,1], C[101:150,2], col="green",
		xlim=xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(C[151:200,1], C[151:200,2], col="blue",
		xlim=xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "")
abline(h = 0, v = 0, col = "gray60")

legend("topleft", legend = c("Blue male","Blue female","Orange male","Orange female"), col=c('red','black','green', 'blue'), pch=15)
dev.off()


#corrélation Cor(a,j)
cercle <- cor(crabsquant,C)
png(filename="F:/SY09/TD02/png/plotCrabsSansT_ACP_V.png")
plot(cercle[,1], cercle[,2], xlab="Axi1", ylab="Axe2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main="ACP sans traitement sur Crabs: les variables")
abline(h = 0, v = 0, col = "gray60")
symbols(0,0, circle = 1.0, inches = F, add = T)
text(cercle[,1], cercle[,2],labels=c("FL", "RW", "CL", "CW","BD"), cex= 0.7, pos=3)
dev.off()

#########################################################################
#2.3.2	tous les crabs avec traitement préalable.
# 		les covariances entre CW(CL) et les autres sont le plus grand, ici on choisit CW (Largeur de la carapace),on divise tous les autres données par Largeur de la carapace. 
#		on étudie les proportions entre tous les autres parties et Largeur de la carapace 

crabsquant2 <- crabsquant[,-4]
crabsquant2 <- crabsquant2 / crabsquant$CW

#centrage et mise à l'echelle
centrageCra2 <- scale(crabsquant2)
print(centrageCra2)

#covariance
covCrab2 <- cov(centrageCra2)
print(covCrab2)

#diagonalisation
diagCrab2 <- eigen(covCrab2)
valeurCrab2 <- diagCrab2$values
vecteurCrab2 <- diagCrab2$vectors
print(diagCrab2)

#pourcentage d’inertie
somme2 <- sum(valeurCrab2)
pct2 <- valeurCrab2/somme2
print(pct2)
#la somme des deux première est  0.63480762 + 0.27704202 = 0.911
#donc on choisit AXE1 et AXE2

#Composant
C2 <- centrageCra2 %*% vecteurCrab2
#print(C2)

#plot Axe c1 et c2
png(filename="F:/SY09/TD02/png/plotCrabsAvecT_ACP_I.png")
# plot(C2[,1], C2[,2], xlab="Axe1", ylab="Axe2", main = "ACP avec traitement sur Crabs: les individus")

xrange = range(c(C2[,1]))
yrange = range(c(C2[,2]))
# first plot
plot(C2[1:50,1], C2[1:50,2], col="red", xlab="Axe1", ylab="Axe2",main = "ACP sans traitement sur Crabs: les individus",
		xlim=xrange, ylim = yrange)

# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(C2[51:100,1], C2[51:100,2], col="black",
		xlim=xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(C2[101:150,1], C2[101:150,2], col="green",
		xlim=xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(C2[151:200,1], C2[151:200,2], col="blue",
		xlim=xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "")

abline(h = 0, v = 0, col = "gray60")
legend("topleft", legend = c("Blue male","Blue female","Orange male","Orange female"), col=c('red','black','green', 'blue'), pch=15)
dev.off()



#corrélation Cor(a,j)
cercle2 <- cor(crabsquant2,C2)
png(filename="F:/SY09/TD02/png/plotCrabsAvecT_ACP_V.png")
plot(cercle2[,1], cercle2[,2], xlab="Axi1", ylab="Axe2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main="ACP avec traitement sur Crabs: les variables")
abline(h = 0, v = 0, col = "gray60")
symbols(0,0, circle = 1.0, inches = F, add = T)
text(cercle2[,1], cercle2[,2],labels=c("FL", "RW", "CL","BD"), cex= 0.7, pos=3)
dev.off()

x11()
biplot(princomp(crabsquant), main="Représentation des individus dans le premier plan factoriel", ylab="Axe 2", xlab="Axe 1")


