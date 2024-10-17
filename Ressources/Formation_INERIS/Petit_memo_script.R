#Pour accéder à la variable "pqnvh" du tableau "Ammoniac"
Ammoniac$pqnvh
#Pour en calculer sa moyenne en enlevant les valeurs manquantes
mean(Ammoniac$pqvnh,na.rm=T)


#Pour ouvrir une nouvelle fenêtre graphique (sans écraser la précédente)
boxplot(Ammoniac$pqvnh)
windows()
boxplot(Ammoniac$pqvnh,col=2)

#Pour représenter plusieurs plot dans la même fenêtre
#mfrow : (ntotal lignes,ntotal colonnes)
par(mfrow=c(2,3))

boxplot(Ammoniac$pqvnh)
boxplot(Ammoniac$pqvnh,col=2)
boxplot(Ammoniac$pqvnh)
abline(h=mean(Ammoniac$pqvnh,na.rm=T),col=2)

hist(Ammoniac$pqvnh)
hist(Ammoniac$pqvnh,col=2)
hist(Ammoniac$pqvnh)
abline(v=mean(Ammoniac$pqvnh,na.rm=T),col=2)


#idem avec les graphiques d'openair (plus généralement les graphes utilisant "lattice")
#on commence par enregistrer chaque plot
plt1 = windRose(Ammoniac,ws="ws",wd="wd",paddle=F)
plt2 = windRose(Ammoniac,ws="ws",wd="wd",angle=15,paddle=F)
plt3 = windRose(Ammoniac,ws="ws",wd="wd",angle=45,paddle=F)
plt4 = windRose(Ammoniac,ws="ws",wd="wd",angle=20,paddle=F)
#on les affiche avec print et l'option split
#split (colonne,ligne,ntotal colonnes,ntotal lignes)
print(plt1, split = c(1, 1, 2, 2), more = TRUE)
print(plt2, split = c(1, 2, 2, 2), more = TRUE)
print(plt3, split = c(2, 1, 2, 2), more = TRUE)
print(plt4, split = c(2, 2, 2, 2))




#A l'issue d'une classification sous RCommander après une ACP (avec FactoMineR),
#tous les résultats sont dans l'objet res.hcpc
#Ce qu'il contient :
res.hcpc
#La table contenant la labellisation des individus dans la classification
tab=res.hcpc$data.clust 


#Créer une variable date au bon format:
# du 1 janvier 2001 au 31 décembre 2001 par jour
dates=seq(as.POSIXlt("2001/1/1"),as.POSIXlt("2001/12/31"),"days")
# du 1 janvier 2005 au 31 décembre 2005 par heure
dates=seq(as.POSIXlt("2005/1/1 00:00:00"),as.POSIXlt("2005/12/31 23:00:00"),"hours")

#Pour l'ajouter au tableau "tab" en la nommant "dates"
tab$dates=dates

#Pour représenter la série x en mettant en abscisse la variable dates
# exemple avec une série journalière où seul le mois est indiqué en abscisses
plot(dates, x, type="l", xaxt="n",xlab='Mois')
r <- as.POSIXct(round(range(dates), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="month"), format="%b")



# Pour obtenir le meilleur modèle de régression possible de façon automatique
# On suppose que tab contient en première colonne la variable à expliquer, 
# tandis que toutes les autres colonnes sont les variables explicatives potentielles
# Ce petit script ne fonctionne que si aucune variable n'est de type facteur (dans ce cas on pourrait adapter)
library(leaps)
modsub<-regsubsets(as.formula(tab),data=tab,nbest=2, nvmax=8)
res=summary(modsub)
index=res$which[which.min(res$bic),]
tabnew=tab[,index]
mod<-lm(tabnew)
summary(mod)
