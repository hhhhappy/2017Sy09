Pima <- read.csv("Pima.csv", header=T)
Pima$z <- factor(Pima$z)

###Femme Diabétique 355-532
diabetique = Pima[Pima$z =="2",]
###Femme non diabétique 1-354
nDiabetique = Pima[Pima$z =="1",]
###1 est diabétique


dataPima <- Pima[,-8]
#centrage et mise à l'echelle
centragePima <- scale(dataPima)

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
#print(C)

# Test <- cbind(C, Pima[,8])
# x11()
# pairs(Test, col = ifelse(Test[,8] == 1, 'red','green'), main = "composants")
# x11()
# res <- prcomp(dataPima, scale = TRUE)
# print(res)
# biplot(res, choices = 2:3)

#plot Axe c1 et c2
png(filename="F:/SY09/TD02/png/plotPima_ACP_I_AXE12.png")
xrange = range(c(C[,1]))
yrange = range(c(C[,2]))

plot(C[1:354,1], C[1:354,2], col="red", xlab="Axe1", ylab="Axe2",main = "ACP sur Pima: les individus",
		xlim=xrange, ylim = yrange)

par(new = TRUE)
plot(C[355:532,1], C[355:532,2], col="black",
		xlim=xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "")
abline(h = 0, v = 0, col = "gray60")

legend("topleft", legend = c("non diabetique","diabetique"), col=c('red','black'), pch=15)
dev.off()

#plot Axe 1 et 3
png(filename="F:/SY09/TD02/png/plotPima_ACP_I_AXE13.png")
xrange = range(c(C[,1]))
yrange = range(c(C[,3]))

plot(C[1:354,1], C[1:354,3], col="red", xlab="Axe1", ylab="Axe3",main = "ACP sur Pima: les individus",
		xlim=xrange, ylim = yrange)

par(new = TRUE)
plot(C[355:532,1], C[355:532,3], col="black",
		xlim=xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "")
abline(h = 0, v = 0, col = "gray60")

legend("topleft", legend = c("non diabetique","diabetique"), col=c('red','black'), pch=15)
dev.off()

#plot Axe 1 et 4
png(filename="F:/SY09/TD02/png/plotPima_ACP_I_AXE14.png")
xrange = range(c(C[,1]))
yrange = range(c(C[,4]))

plot(C[1:354,1], C[1:354,4], col="red", xlab="Axe1", ylab="Axe4",main = "ACP sur Pima: les individus",
		xlim=xrange, ylim = yrange)

par(new = TRUE)
plot(C[355:532,1], C[355:532,4], col="black",
		xlim=xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "")
abline(h = 0, v = 0, col = "gray60")

legend("topleft", legend = c("non diabetique","diabetique"), col=c('red','black'), pch=15)
dev.off()

#plot Axe 1 et 5
png(filename="F:/SY09/TD02/png/plotPima_ACP_I_AXE15.png")
xrange = range(c(C[,1]))
yrange = range(c(C[,5]))

plot(C[1:354,1], C[1:354,5], col="red", xlab="Axe1", ylab="Axe5",main = "ACP sur Pima: les individus",
		xlim=xrange, ylim = yrange)

par(new = TRUE)
plot(C[355:532,1], C[355:532,5], col="black",
		xlim=xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "")
abline(h = 0, v = 0, col = "gray60")

legend("topleft", legend = c("non diabetique","diabetique"), col=c('red','black'), pch=15)
dev.off()


#corrélation Cor(a,j)
cercle <- cor(dataPima,C)
png(filename="F:/SY09/TD02/png/plotPima_ACP_V_AXE12.png")
plot(cercle[,1], cercle[,2], xlab="Axi1", ylab="Axe2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main="ACP sur Pima: les variables")
abline(h = 0, v = 0, col = "gray60")
symbols(0,0, circle = 0.75, inches = F, add = T)
text(cercle[,1], cercle[,2],labels=c("npreg", "glu", "bp", "skin", "bmi", "ped", "age"), cex= 0.7, pos=3)
dev.off()

png(filename="F:/SY09/TD02/png/plotPima_ACP_V_AXE13.png")
plot(cercle[,1], cercle[,3], xlab="Axi1", ylab="Axe3", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main="ACP sur Pima: les variables")
abline(h = 0, v = 0, col = "gray60")
symbols(0,0, circle = 0.75, inches = F, add = T)
text(cercle[,1], cercle[,3],labels=c("npreg", "glu", "bp", "skin", "bmi", "ped", "age"), cex= 0.7, pos=3)
dev.off()

png(filename="F:/SY09/TD02/png/plotPima_ACP_V_AXE14.png")
plot(cercle[,1], cercle[,4], xlab="Axi1", ylab="Axe4", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main="ACP sur Pima: les variables")
abline(h = 0, v = 0, col = "gray60")
symbols(0,0, circle = 0.75, inches = F, add = T)
text(cercle[,1], cercle[,4],labels=c("npreg", "glu", "bp", "skin", "bmi", "ped", "age"), cex= 0.7, pos=3)
dev.off()

png(filename="F:/SY09/TD02/png/plotPima_ACP_V_AXE15.png")
plot(cercle[,1], cercle[,5], xlab="Axi1", ylab="Axe5", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main="ACP sur Pima: les variables")
abline(h = 0, v = 0, col = "gray60")
symbols(0,0, circle = 0.75, inches = F, add = T)
text(cercle[,1], cercle[,5],labels=c("npreg", "glu", "bp", "skin", "bmi", "ped", "age"), cex= 0.7, pos=3)
dev.off()

