library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]

crabBM = crabsquant[1:50,]
crabBF = crabsquant[50:100,]
crabOM = crabsquant[100:150,]
crabOF = crabsquant[150:200,]

crabM = rbind(crabBM, crabOM)
crabF = rbind(crabBF, crabOF)

crabO = rbind(crabOF, crabOM)
crabB = rbind(crabBF, crabBM)
png(filename = "caracteristiquesMF.png")
par(mar=c(5, 4, 4, 6), xpd=TRUE)
boxplot(list(crabM$FL, crabF$FL,crabM$RW, crabF$RW,crabM$CL, crabF$CL,crabM$CW, crabF$CW,crabM$BD, crabF$BD), 
        col = c('forestgreen','forestgreen','darkmagenta','darkmagenta','gold','gold','deepskyblue4','deepskyblue4','darkred','darkred'),
        method="jitter",
        vertical=T,
        ylab="size(mm)",
        las=2,
        main="Caractéristiques des crabes - Différence Male/Female",
        names = c('male','female','male','female','male','female','male','female','male','female'))
legend(11,50,xpd=NA,inset=.05, title="Legend",
       c("FL","RW","CL","CW","BD"), 
       fill=c("forestgreen","darkmagenta","gold","deepskyblue4","darkred"))
dev.off()
png(filename = "caracteristiquesOB.png")
par(mar=c(5, 4, 4, 6), xpd=TRUE)
boxplot(list(crabO$FL, crabB$FL,crabO$RW, crabB$RW,crabO$CL, crabB$CL,crabO$CW, crabB$CW,crabO$BD, crabB$BD), 
        col = c('forestgreen','forestgreen','darkmagenta','darkmagenta','gold','gold','deepskyblue4','deepskyblue4','darkred','darkred'),
        method="jitter",
        vertical=T,
        ylab="size(mm)",
        main="Caractéristiques des crabes 
        Différence des espèces Orange/Blue",
        las=2,
        names = c('Orange','Blue','Orange','Blue','Orange','Blue','Orange','Blue','Orange','Blue'))
legend(11,50,xpd=NA,inset=.05, title="Legend",
       c("FL","RW","CL","CW","BD"), 
       fill=c("forestgreen","darkmagenta","gold","deepskyblue4","darkred"))
dev.off()
png(filename="sexe-crabs.png")
par
plot(crabsquant, main="Graphique bidimensionnel des caractéristiques 
     des crabes male/female", bg=c("gold","darkred")[crabs[,2]], pch=c(21,22)[crabs[,2]])
dev.off()
png(filename="spicies-crabs.png")
plot(crabsquant,main="Graphique bidimensionnel des caractéristiques 
     des crabes orange/blue", bg=c("blue","darkorange")[crabs[,1]], pch=c(21,22)[crabs[,1]])
dev.off()
#####Male and Female
#attach(crabM)
#inter<-seq(min(CL),max(CL),by=(max(CL)-min(CL))/5)
#hist(CL,breaks=inter, main = "Male longueur carapace")
#detach(crabM)

#x11()
#attach(crabF)
#inter<-seq(min(CL),max(CL),by=(max(CL)-min(CL))/5)
#hist(CL,breaks=inter, main = "Female longueur carapace")
#detach(crabF)

X11()
boxplot(cbind(crabM$BD,crabF$BD ), main = "longueur carapace" , names = c("Male", "Female") , row.names = c(NA, -5L), class = "data.frame")

#X11()
#boxplot(cbind(crabM$RW,crabF$RW ), main = "Taille du train arrière", names = c("Male", "Female"), row.names = c(NA, -5L), class = "data.frame")

X11()
boxplot(cbind(crabM$FL,crabF$FL ), main = "Taille du lobe frontal" , names = c("Male", "Female") , row.names = c(NA, -5L), class = "data.frame")

X11()
boxplot(cbind(crabM$BD,crabF$BD ), main = "Profondeur du corps", names = c("Male", "Female"))

#X11()
#boxplot(cbind(crabM$RW,crabF$RW ), main = "Taille du train arrière")

#####Bleu and Orange#######

#attach(crabO)
#inter<-seq(min(BD),max(BD), by=(max(BD)-min(BD))/5)
#hist(BD,breaks=inter, main = "Orange longueur carapace")
#detach(crabO)

#x11()
#attach(crabB)
#inter<-seq(min(BD),max(BD),by=(max(BD)-min(BD))/5)
#hist(BD,breaks=inter, main = "Bleu longueur carapace")
#detach(crabB)


boxplot(cbind(crabO$BD,crabB$BD ), main = "longueur carapace" , names = c("Crabs Orange", "Crabs Bleu") , row.names = c(NA, -5L), class = "data.frame")

#X11()
#boxplot(cbind(crabO$RW,crabB$RW ), main = "Taille du train arrière", names = c("Crabs Orange", "Crabs Bleu"), row.names = c(NA, -5L), class = "data.frame")

X11()
boxplot(cbind(crabO$FL,crabB$FL ), main = "Taille du lobe frontal" , names = c("Crabs Orange", "Crabs Bleu") , row.names = c(NA, -5L), class = "data.frame")

X11()
boxplot(cbind(crabO$BD,crabB$BD ), main = "Profondeur du corps", names = c("Crabs Orange", "Crabs Bleu"))



covM = cov(crabM)
covF = cov(crabF)
covO = cov(crabO)
covB = cov(crabB)

print(covM)
print(covF)
print(covO)
print(covB)





