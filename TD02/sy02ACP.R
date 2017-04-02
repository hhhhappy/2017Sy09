
notes <- read.csv("sy02-p2016.csv", na.strings="", header=T)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),
ordered=T)

attach(notes)

moy.median <- aggregate(note.median~correcteur.median, data=notes, FUN=mean)
names(moy.median) <- c("correcteur","moy.median")
std.median <- aggregate(note.median~correcteur.median, data=notes, FUN=sd)
names(std.median) <- c("correcteur","std.median")
median <- merge(moy.median, std.median)
moy.final <- aggregate(note.final~correcteur.final, data=notes, FUN=mean)
names(moy.final) <- c("correcteur","moy.final")
std.final <- aggregate(note.final~correcteur.final, data=notes, FUN=sd)
names(std.final) <- c("correcteur","std.final")
final <- merge(moy.final, std.final)
correcteurs <- merge(median, final, all=T)

corr.acp <- correcteurs[-c(2,8),]

M <- corr.acp[, 2:5]
print(M)
print(corr.acp)

#centrage et mise à l'echelle X
nameCorr <- corr.acp[, 1]
corr.acp <- scale(corr.acp[, 2:5])
#print(corr.acp)

#covariance
covCorr <- cov(corr.acp)
#print(covCorr)

#diagonalisation
diagcorr <- eigen(covCorr)
valeurCorr <- diagcorr$values
#U
vecteurCorr <- diagcorr$vectors

print(valeurCorr)
print(vecteurCorr)


somme <- sum(valeurCorr)

#pourcentage d’inertie
pct <- valeurCorr/somme
print(pct)

#Composants principaux
C = corr.acp%*%vecteurCorr
print(C)

#plot Axe c1 et c2
png(filename="F:/SY09/TD02/png/plotSy02_ACP_I_AXE12.png")
plot(C[,1], C[,2], xlab="Axe1", ylab="Axe2", main = "ACP sur Corr: les individus")
abline(h = 0, v = 0, col = "gray60")
text(C[,1], C[,2],labels=nameCorr, cex= 0.7, pos=3)
dev.off()

#plot Axe c1 et c3
png(filename="F:/SY09/TD02/png/plotSy02_ACP_I_AXE13.png")
plot(C[,1], C[,3], xlab="Axe1", ylab="Axe3", main = "ACP sur Corr: les individus")
abline(h = 0, v = 0, col = "gray60")
text(C[,1], C[,3],labels=nameCorr, cex= 0.7, pos=3)
dev.off()

#Corrélations
cercle <- cor(M,C)
print(cercle)
png(filename="F:/SY09/TD02/png/plotSy02_ACP_V_AXE12.png")
plot(cercle[,1], cercle[,2], xlab="Axi1", ylab="Axe2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main = "ACP sur Corr: les variables")
abline(h = 0, v = 0, col = "gray60")
symbols(0,0, circle = 0.8, inches = F, add = T)
text(cercle[,1], cercle[,2],labels=c("moy.median", "std.median", "moy.final", "std.final"), cex= 0.7, pos=3)
dev.off()

png(filename="F:/SY09/TD02/png/plotSy02_ACP_V_AXE13.png")
plot(cercle[,1], cercle[,3], xlab="Axi1", ylab="Axe3", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main = "ACP sur Corr: les variables")
abline(h = 0, v = 0, col = "gray60")
symbols(0,0, circle = 0.8, inches = F, add = T)
text(cercle[,1], cercle[,3],labels=c("moy.median", "std.median", "moy.final", "std.final"), cex= 0.7, pos=3)
dev.off()
######
#Approximation de dimension k = 1
Res1 <- C[,1]%*% t(vecteurCorr[,1])
#Approximation de dimension k = 2
Res2 <- C[,1:2]%*% t(vecteurCorr[,1:2])
#Approximation de dimension k = 3
Res3 <- C[,1:3]%*% t(vecteurCorr[,1:3])
#Approximation de dimension k = 4
Res4 <- C%*%t(vecteurCorr)

print(Res1)
print(Res2)
print(Res3)
print(Res4)
print(corr.acp)


########## replace NA 
corr.acp <- correcteurs[-c(2,8),]
Mean.moy.median <- mean(corr.acp[,2])
Mean.std.median <- mean(corr.acp[,3])
Mean.moy.final <- mean(corr.acp[,4])
Mean.std.final <- mean(corr.acp[,5])

print(Mean.moy.median)
print(Mean.std.final)
correcteurs[2,4] <- Mean.moy.final
correcteurs[2,5] <- Mean.std.final
correcteurs[8,2] <- Mean.moy.median
correcteurs[8,3] <- Mean.std.median

corr.acp <- correcteurs
M <- corr.acp[, 2:5]
print(M)
print(corr.acp)

#centrage et mise à l'echelle X
nameCorr <- corr.acp[, 1]
corr.acp <- scale(corr.acp[, 2:5])
print(corr.acp)

#covariance
covCorr <- cov(corr.acp)
print(covCorr)

#diagonalisation
diagcorr <- eigen(covCorr)
valeurCorr <- diagcorr$values
#U
vecteurCorr <- diagcorr$vectors

print(valeurCorr)
print(vecteurCorr)


somme <- sum(valeurCorr)

#pourcentage d’inertie
pct <- valeurCorr/somme
print(pct)

#Composants principaux
C <- corr.acp%*%vecteurCorr
print(C)

#plot Axe c1 et c2
png(filename="F:/SY09/TD02/png/plotSy02_ACP_I_AXE12_RNA.png")
plot(C[,1], C[,2], xlab="Axe1", ylab="Axe2", main = "ACP sur Corr: les individus")
abline(h = 0, v = 0, col = "gray60")
text(C[,1], C[,2],labels=nameCorr, cex= 0.7, pos=3)
dev.off()

#plot Axe c1 et c3
png(filename="F:/SY09/TD02/png/plotSy02_ACP_I_AXE13_RNA.png")
plot(C[,1], C[,3], xlab="Axe1", ylab="Axe3", main = "ACP sur Corr: les individus")
abline(h = 0, v = 0, col = "gray60")
text(C[,1], C[,3],labels=nameCorr, cex= 0.7, pos=3)
dev.off()

#Corrélations
cercle <- cor(M,C)
print(cercle)
png(filename="F:/SY09/TD02/png/plotSy02_ACP_V_AXE12_RNA.png")
plot(cercle[,1], cercle[,2], xlab="Axi1", ylab="Axe2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main = "ACP sur Corr: les variables")
abline(h = 0, v = 0, col = "gray60")
symbols(0,0, circle = 0.8, inches = F, add = T)
text(cercle[,1], cercle[,2],labels=c("moy.median", "std.median", "moy.final", "std.final"), cex= 0.7, pos=3)
dev.off()

png(filename="F:/SY09/TD02/png/plotSy02_ACP_V_AXE13_RNA.png")
plot(cercle[,1], cercle[,3], xlab="Axi1", ylab="Axe3", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main = "ACP sur Corr: les variables")
abline(h = 0, v = 0, col = "gray60")
symbols(0,0, circle = 0.8, inches = F, add = T)
text(cercle[,1], cercle[,3],labels=c("moy.median", "std.median", "moy.final", "std.final"), cex= 0.7, pos=3)
dev.off()



