dPima <- read.csv("Pima.csv", header=T)
Pima$z <- factor(Pima$z)

print(summary(Pima))


###Femme Diabétique
diabetique = Pima[Pima$z =="2",]
###Femme non diabétique
nDiabetique = Pima[Pima$z =="1",]
###1 est diabétique?



###taux plasmatique de glucose
boxplot(diabetique$glu, nDiabetique$glu, main = "taux plasmatique de glucose", names = c("Diabétique", "Non diabétique") , row.names = c(NA, -5L), class = "data.frame")


###nombre de grossesses

x11()
boxplot(diabetique$npreg, nDiabetique$npreg, main = "nombre de grossesses", names = c("Diabétique", "Non diabétique") , row.names = c(NA, -5L), class = "data.frame")

###pression artérielle diastolique

x11()
boxplot(diabetique$bp, nDiabetique$bp, main = "pression artérielle diastolique", names = c("Diabétique", "Non diabétique") , row.names = c(NA, -5L), class = "data.frame")

### épaisseur du pli cutané au niveau du triceps 

x11()
boxplot(diabetique$skin, nDiabetique$skin, main = " épaisseur du pli cutané au niveau du triceps ", names = c("Diabétique", "Non diabétique") , row.names = c(NA, -5L), class = "data.frame")

### indice de masse corporelle  

x11()
boxplot(diabetique$ped , nDiabetique$ped , main = " fonction de pedigree du diabète   ", names = c("Diabétique", "Non diabétique") , row.names = c(NA, -5L), class = "data.frame")

 

attach(Pima)

### indice de masse corporelle 
x11()
inter<-seq(min(bmi),max(bmi),by=(max(bmi)-min(bmi))/6)
old.par <- par(mfrow=c(1, 2))
hist(bmi[z=="2"],breaks=inter, main = "Diabétique", xlab = "indice de masse corporelle")
hist(bmi[z=="1"],breaks=inter, main = "Non diabétique", xlab = "indice de masse corporelle")
par(old.par)

### age
inter<-seq(min(age),max(age),by=(max(age)-min(age))/6)
x11()
hist(age[z=="2"],main = "Distribution d'Age (femmes diabetique)",breaks=inter, xlab = "age")

detach(Pima)




