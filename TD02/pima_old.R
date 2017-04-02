Pima <- read.csv("Pima.csv", header=T)
Pima$z <- factor(Pima$z)

print(summary(Pima))


###Femme Diabétique
diabetique = Pima[Pima$z =="1",]
###Femme non diabétique
nDiabetique = Pima[Pima$z =="2",]
###1 est diabétique?



###taux plasmatique de glucose
boxplot(diabetique$glu, nDiabetique$glu, main = "taux plasmatique de glucose", names = c("Diabétique", "Non diabétique") , row.names = c(NA, -5L), class = "data.frame")

x11()
###taux plasmatique de glucose
boxplot(diabetique$npreg, nDiabetique$npreg, main = "taux plasmatique de glucose", names = c("Diabétique", "Non diabétique") , row.names = c(NA, -5L), class = "data.frame")

