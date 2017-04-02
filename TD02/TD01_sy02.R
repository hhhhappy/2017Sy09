
#import datas
notes <- read.csv("sy02-p2016.csv", na.strings="", header=T)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),
ordered=T)

#summary
#print (summary(notes))

#transfert a factor into numeric
#resultat <- as.numeric(notes$resultat)
#print(resultat)

#delete NA rows
notes = notes[complete.cases(notes),]

###print students whose final is NA
#compareMF<- cbind(notes$note.median, notes$note.final)
#res <- compareMF[rowSums(is.na(compareMF))==1, ]
#print(res)

#notes[rowSums(is.na(notes))>1, ]

###boxplot with names Median Final Total
x11()
mfnote = structure(list(median = notes$note.median, final = notes$note.final, total = notes$note.total),
	Names = c("median", "final", "total"), row.names = c(NA, -5L), class = "data.frame")
boxplot(mfnote)

###function pour boîte
boiteNoteFunction <- function(notes, xElement, xLabel, yLabel, mainLabel, sizeXLabel){
	levelArray <- levels(xElement);
	nbLevel <- length(levelArray);
	print(nbLevel);
	i <- 1;
	for(level in levelArray){
		temp <- notes[xElement == level,];
		if(i == 1){
			listNote <- list(temp$note.total);
		}
		else{
			listNote <- append(listNote, list(temp$note.total));
		}
		i<- i+1;
	}
	print(c(levelArray))
	x11()
	mfnote = structure(listNote,
			Names =levelArray, row.names = c(NA, -5L), class = "data.frame");
	boxplot(mfnote, xaxt = "n", xlab = xLabel, ylab = yLabel, main = mainLabel);
	axis(1, at=1:nbLevel, labels=levelArray, cex.axis=sizeXLabel);
}
###boite pour specialite
boiteNoteFunction(notes, notes$specialite, "Spécialité", "Notes", "Boîte de note total selon Spécialité", 1)

###boite pour derniere formation
boiteNoteFunction(notes, notes$dernier.diplome.obtenu, "Derniere diplome", "Notes", "Boîte de note total selon derniere formation", 0.37)

###boites pour niveau (branche)
resBranche <- notes[notes$specialite!="TC",]
resBranche<- resBranche[resBranche$specialite != "HuTech", ]
resBranche<- resBranche[resBranche$specialite != "ISS", ]
boiteNoteFunction(resBranche, resBranche$niveau, "niveau", "Notes", "Boîte de note total selon niveau (branche)", 1)

###boites pour niveau (TC)
resTC <- notes[notes$specialite=="TC",]
boiteNoteFunction(resTC, resTC$niveau, "niveau", "Notes", "Boîte de note total selon niveau (TC)", 1)

###moyenne de chaque branche
#res<-c(mean(GI$note.total),mean( GB$note.total), mean(GSU$note.total), mean(GSM$note.total), mean(TC$note.total), 
#	mean(GP$note.total), mean(GM$note.total), mean(HuTech$note.total), mean(ISS$note.total)) 
	
#x11()
	
#plot(res, type = "o", xaxt = "n", xlab="Branche", ylab="Note", main = "Moyenne de note totale")
#axis(1, at=1:9, labels=c("GI","GB", "GSU", "GSM", "GP", "TC", "GM", "Hutech", "ISS"))
 
###Point selon le niveau de branche
#x11()
#resBranche <- notes[notes$specialite!="TC",]
#resBranche<- resBranche[resBranche$specialite != "HuTech", ]
#resBranche<- resBranche[resBranche$specialite != "ISS", ]
#m <- cbind(resBranche$niveau, resBranche$note.total)
#plot(m, main = "Note total/Branche Niveau",xlab = "Branche Niveau", ylab = "Notes")

###Correcteur median
#x11()
attach(notes)
inter<-seq(min(note.median),max(note.median),by=(max(note.median)-min(note.median))/6)
h1<-hist(plot=F,note.median[correcteur.median=='Cor1'],breaks=inter)
h2<-hist(plot=F,note.median[correcteur.median=='Cor2'],breaks=inter)
h3<-hist(plot=F,note.median[correcteur.median=='Cor4'],breaks=inter)

h4<-hist(plot=F,note.median[correcteur.median=='Cor5'],breaks=inter)

h5<-hist(plot=F,note.median[correcteur.median=='Cor6'],breaks=inter)

h6<-hist(plot=F,note.median[correcteur.median=='Cor7'],breaks=inter)

h7<-hist(plot=F,note.median[correcteur.median=='Cor8'],breaks=inter)
#barplot(rbind(h1$counts,h2$counts,h3$counts,h4$counts,h5$counts,h6$counts,h7$counts),space=0,
#legend=levels(correcteur.median),main="CorrcteurMedian",col=c('red','green3','blue', 'black', 'yellow', 'orange', 'pink'))
print(max(note.median))
x11()
dat <- cbind(h1$counts,h2$counts,h3$counts,h4$counts,h5$counts,h6$counts,h7$counts) # make data
matplot(dat, type = c("l"),pch=2,col = 1:7, main = "Distribution des notes en distinguant selon les correcteurs", 
	xaxt = "n", xlab='Notes moyenne de chaque intervalle', ylab="Nombre des etudiants") #plot
axis(1, at=1:7, labels=seq(min(note.median),max(note.median),by=round((max(note.median)-min(note.median))/6,2)))
legend("topleft", legend = levels(correcteur.median), col=1:7, pch=15) # optional legend

detach(notes)







