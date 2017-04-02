library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]

crabBM = crabsquant[1:50,]
crabBF = crabsquant[50:100,]
crabOM = crabsquant[100:150,]
crabOF = crabsquant[150:200,]


#####Male and Female
crabM = rbind(crabBM, crabOM)
crabF = rbind(crabBF, crabOF)

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

crabO = rbind(crabOF, crabOM)
crabB = rbind(crabBF, crabBM)

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





