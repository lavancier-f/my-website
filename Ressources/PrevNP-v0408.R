
#############################################################
# Prevision non paramétrique V8
# version ---- 			01 12 99
# derniere modification		20 12 07 GO 06 04 08 (AP)
# Auteur :	 		Anne    Philippe  (version R)
# modifie:			Georges Oppenheim 
# version R des programmes de la boite matlab tbnp
# les fonctions sont 
#	np			principale.
#	demo.np			un exemple d'utilisation
#	erreur	      		calcule une erreur
#	qualprev		indices de qualité de la pravision		
#	validation		validation croisée pour choisir la taille du 
#				bloc témoin et la constante
#	calcprnp		fonction interne prepare les sorties
#	ecblocs			fonction interne crée les blocs
#	fenetre			famille de fenetres
#	prevnp			organise les appels aux fonctions
#	simila			calcule les similarités

#############################################################
library(Hmisc)   #calcul des stats pondérées  
	
#################################################################
	# fonction principale
	# entrees :	 serie, horizon, taille du bloc
	# sorties :	 similarites, prevision, regions de confiance
##################################################################

"np"	<- function(serie, kprev = 15, r = 10,cte=1,typtub=3,passim=1,debbloc1=1, methprev=1)
{
	n 		<- length(serie)
	lgtem 	<- r
	lgprev 	<- kprev
	exit 		<- prevnp(serie, lgtem, lgprev, cte, passim, debbloc1,typtub, methprev)	#	
	sim 		<- exit$simi[1:(length(exit$simi) - 1)]
	sim 		<- sim/sum(sim)
	
	par(mfcol = c(3, 2))  
	plot(sim[1:(length(sim) - 1)], ylab = "", main = paste("similarite, taille du bloc= ",r), 
		type = "h", xlim = c(0, n))
	plot(type="l",c(serie,exit$prevision))
	lines((1 + n):(n+kprev), exit$prevision, col = 2, lty=2)
	matplot(type="l",cbind(c(exit$prevision),c(exit$CS),c(exit$CI)),main=paste("bloc=",r),col=c(2,3,3),lty=c(2,3,3),ylab="prevision")
	legend("topleft",c("prev","inter conf"),lty=c(2:3) ,col=2:3 ) 

	c(exit)
	}
		

#--------------------------------------------------------------------------------------
#exemple  : demo.np(log(AirPassengers),  kprev = 12, r = 12, cte=1,methprev=3) 
# methprev=1 serie stationnaire 
# methprev=3 serie non stationnaire 
# r taille du bloc 
# kprev horizon de prevision 
# cte=1 constante fenetre 
# typtub=0.1 niveau de confiance 
#--------------------------------------------------------------------------------------
"demo.np"		<- function(serie, kprev = 15, r = 10,cte=1,typtub=0.1,passim=1,debbloc1=1, methprev=1,  dessin = 1)
{
	
	n 		<- length(serie)
	TT 		<- n - kprev
	observe 	<- serie[1:TT]
	test 		<- serie[ -c(1:TT)]
	lgtem 	<- r
	lgprev 	<- kprev

	exit 		<- prevnp(observe, lgtem, lgprev, cte, passim, debbloc1,typtub, methprev)	
	sim 		<- exit$simi[1:(length(exit$simi) - 1)]
	sim 		<- sim/sum(sim)

	par(mfrow = c(3, 1))
	if (dessin==1)
		{
		plot(sim[1:(length(sim) - 1)], ylab = "", main = paste('similarite; long prev',kprev, '; long serie', n, '; methprev', methprev), 
		type = "h", xlim = c(0, n))
		plot(type="l",c(serie),main = paste("similarite, taille du bloc= ",r, 'Constante de fenetre', signif(cte,3)))
		lines((1 + TT):n, exit$prevision, col = 2, lty=2)
		abline(v = c(TT-r+1,TT))
		}
	ERL2		<- qualprev(prevu=exit$prevision, realise=test , tech='rmse')
	erreurMAPE 	<- qualprev(prevu=exit$prevision, realise=test , tech='mape')
	if (dessin==1)
		{
		matplot(type="l",cbind(test,c(exit$prevision),c(exit$CS),c(exit$CI)),main=paste("long prev",kprev),col=c(1:2,3,3),lty=c(1:2,3,3),
		xlab=paste("sqrt(erreur L2)=",signif(ERL2,3), ', mape en %', signif(erreurMAPE,3)),ylab="prevision/series")
		legend("topleft",c("serie","prev",paste("inter conf niveau",typtub)),lty=c(1:3) ,col=1:3 ) 
		}
	stat 		<- summary(test - exit$prevision)

 w = c(exit, stat, RMSE=ERL2,MAPE=erreurMAPE)
	w
}

#--------------------------


erreur 	<- function(t,h,x,r,cte,methprev=1)
{
a= try(prevnp(x[1:t],r,h,cte,methprev=methprev),silent=TRUE)
ifelse(class(a)=='try-error', 'NA', mean((a$prevision-x[1:h+t])^2))
}

qualprev	<- function(prevu,realise,tech='rmse')
{ 	
	if (tech=='rmse') {w=sqrt(mean((prevu-realise)^2))}
	if (tech=='mepe') {w=mean(abs(prevu-realise))}
	if (tech=='mape') 
	{	
	j = which(abs(realise)> 1e-6*mean(abs(realise)))
	w=100*mean(abs((prevu[j]-realise[j])/realise[j]))
	}
	if (tech=='cor2') {w=  cor(x=prevu,y=realise, use="pairwise.complete.obs")^2} 
	w
}
	
#--------------------------------------------------------------------------------------
#exemple  : validation(log(AirPassengers),  hprev = 12, where.r = c(11:13), where.cte=c(1:3) ,methprev=3) 
# where.r contient les longueurs de bloc testées 
# where.cte contient les valeurs testées pour la constante dans l'exprssion de  h(n)
# hprev horizon de prevision 
#--------------------------------------------------------------------------------------

validation 	<- function(x,hprev,where.r,where.cte, methprev=1) 
	# x contient la serie 
	# hprev l'horizon de prevision
	# where.r,where.h sont des vecteurs les valeurs 
	# possibles du bloc where.r et de la fenetre where.h 
	#  METHPREV  A BRANCHER
	{
		n		= length(x)

		un		<-function(r,win) 
		{ 
		ind		= floor(2*n/3):(n-hprev) 
		e		= sapply(ind,erreur,h=hprev,x=x,r=r,cte=win,methprev=methprev) 
	 	mean(e,rm.action=na.omit)
		}
	RR		= outer(where.r,where.cte,'+')
	for  (i in 1:length(where.r))
	{
		for (j in 1:length(where.cte)) 
		{RR[i,j]	= un(where.r[i],where.cte[j])}
	}
	print(RR)
	i0			= which.min(apply(RR,1,min))
	j0			= which.min(RR[i0,]) 
	list(bloc = where.r[i0] , fenetre = where.cte[j0]) 
}
	 
###########################################################
	# fonctions 
############################################################
"calcprnp" <- function(serie, blocs, indbtem, sim, lgprev, typtub, methprev=1)
{
	lgtem 		<- indbtem[length(indbtem)] - indbtem[1] + 1 	# longueur du bloc temoin
	bloct 		<- serie[indbtem]						# bloc temoin
	lgser 		<- length(serie)						# longueur serie

	if (methprev != 1)
	{
		centrblt 	<- mean(bloct)						# moyenne du bloc temoin
		ind0 		<- indbtem - indbtem[1]					# indice du bloc temoin [0,1,...]
		# inter 	<- 1:length(sim);	ssgrille 		<- inter[sim > 0]
		ssgrille 	<- which(sim > 0)						# indice des similarités >0
		for(i in 1:length(ssgrille)) 	blocs[, i] <- serie[ind0 + ssgrille[i]] 
		centrbl 	<- rep(0, lgser - lgtem)
		centrbl[ssgrille] <- apply(blocs, 2, mean)

		if (methprev == 3)
		{reducblt 	<- sqrt(var(bloct))
		reducbl 	<- rep(1, lgser - lgtem)
		reducbl[ssgrille] <- sqrt(apply(blocs^2, 2, mean) - centrbl^2)}
	}

	#--- Calcul des previsions ----
	prev 		<- c(1:lgprev)
	tubeinf 	<- matrix(0,lgprev,1)
	tubesup 	<- tubeinf
	seuildes	<- c(0.01,0.025,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.975,0.99) 
	quantdes 	<- matrix(0,length(seuildes),lgprev)
	for(hor in 1:lgprev)
	{
		nbterm 	<- lgser - hor - lgtem + 1
		pond 		<- sim[1:nbterm]
		indnz 	<- which(pond != 0)
		if (length(indnz)==0) {stop('Les similarités sont toutes nulles. On est dans la fonction calcprev')}
		ser 		<- serie[(lgtem + hor):lgser]

		if (methprev==3)  {ser 	= (ser - centrbl[1:nbterm])/reducbl[1:nbterm] * reducblt + centrblt}
		if (methprev==2) 	{ser 	= (ser - centrbl[1:nbterm]) + centrblt}

		prev[hor] 	<- wtd.mean(ser[indnz],pond[indnz], normwt=T)
		if(typtub >= 1) 	
		{	
			sigchap		<- sqrt(wtd.var(ser[indnz],pond[indnz], normwt=T))
			tubesup[hor] 	<- prev[hor] + (typtub * sigchap)
			tubeinf[hor] 	<- prev[hor] - (typtub * sigchap)
		}		
		if(typtub < 1) 	
		{	
			alpha			= typtub
			quantil		<-  wtd.quantile(ser,weights=pond,probs=c(alpha/2, 1-alpha/2) ,normwt=T)
			quantdes[,hor]<-  wtd.quantile(ser,weights=pond,probs=seuildes ,normwt=T)
			tubesup[hor] 	<- quantil[1]
			tubeinf[hor] 	<- quantil[2]
			print(tubeinf[hor]);print(tubesup[hor]);
		}		

	}


	list(prevision = prev, CS = tubesup, CI = tubeinf, simi = sim, quantdes=quantdes, seuildes=seuildes)
}



"ecblocs"<-
function(serie, indbtem, passim, debbloc1, methprev=1)
{
	bloct 	<- serie[indbtem]	# extraction du bloc temoin
	lgtem 	<- indbtem[length(indbtem)] - indbtem[1] + 1
	lgser 	<- length(serie)	# longueur de la serie
	nblocs 	<- ceiling((lgser - lgtem + 1 - debbloc1 + 1)/passim)
	indb0 	<- indbtem - indbtem[1] + debbloc1
	blocs 	<- matrix(0, nrow = length(indbtem), ncol = nblocs)
	reducblt 	<- sqrt(var(bloct))
	centrblt 	<- mean(bloct)
	for(j in 1:length(indbtem))
		blocs[j,  ] <- serie[j:(lgser - length(indbtem) + j)]
	centrbl 	<- apply(blocs, 2, mean)
	reducbl 	<- sqrt(apply(blocs^2, 2, mean) - centrbl^2)
	blocs 		<- t(blocs)
	if (methprev==1) blocs 	<- t(blocs) - bloct
	if (methprev==2) blocs 	<- t(blocs - centrbl) - (bloct - centrblt)
	if (methprev==3) blocs 	<- t((blocs - centrbl)/reducbl) - (bloct - centrblt)/	reducblt

	ind 		<- 1:nblocs
	ind 		<- (ind - 1) * passim + debbloc1
	ssgrille 	<- rep(0, (lgser - lgtem + 1))
	ssgrille[ind] 	<- rep(1, length(ind))
	list(ssgrille = ssgrille, blocs = blocs)
}

"fenetre"<- function(lgser , indbtem, reglage = 1)
{
	#lgser <- length(serie)
	lgtem <- indbtem[length(indbtem)] - indbtem[1] + 1
	h 	<- reglage/(lgser^(1/(lgtem + 4))) #h <- sqrt(var(serie)) * h
	h		
}


"prevnp" <- function(serie, lgtem, lgprev, cte=1, passim = 1, debbloc1= 1, typtub= 3, methprev=1)
{
	lgser 	<- length(serie)
	indbtem 	<- (lgser - lgtem + 1):lgser
   	bb 		<- ecblocs(serie, indbtem, passim, debbloc1, methprev)
      win		<- fenetre(lgser , indbtem, cte)
	sim 		<- simila(win, bb$ssgrille, bb$blocs)
	calcprnp(serie, bb$blocs, indbtem, sim, lgprev, typtub, methprev) # modification 2007
}




"simila"<- function(h, ssgrille, blocs)
{
	sim 					<- rep(0, length(ssgrille))
	lgtem 				<- length(blocs[, 1])
	inter 				<- c(1:length(ssgrille))
	sim[inter[ssgrille == 1]] 	<- dnorm(sqrt(apply(blocs^2, 2, sum))/h)
	sim
}




PrevisionProbabiliste = function(w)
{
# w est la sortie de np.demo
kprev = length(w$prevision)
tprev = 1:kprev; tprev
#---------
ncoul = 15; coul 	= rainbow(ncoul)
xlim = c((tprev[1]-0.3),tprev[kprev]+0.3); 
ylim = c(min(w$quantdes,w$prevision),max(w$quantdes,w$prevision))
#---------
par(mfrow=c(1,1));
x	= c(tprev, rev(tprev))
plot(tprev,w$prevision, ylim=ylim, xlim=xlim,main='Probabilistic Forecast', xlab='Horizon', ylab='Forecast')
for (kdes in 1:(nrow(w$quantdes)-1))
{ 
	y = c(w$quantdes[kdes,],w$quantdes[(kdes+1),rev(tprev)])
	polygon(x,y,col=coul[min(c(kdes,ncoul-kdes))], lty=2, lwd=2,border = NA, , ylim=ylim, xlim=xlim) 
	par(new=TRUE)
	text(x=0.8,y=w$quantdes[kdes,1],labels=paste(w$seuildes[kdes]), cex=0.7)
	text(x=tprev[kprev]+0.2,y=w$quantdes[kdes,kprev],labels=paste(w$seuildes[kdes]), cex=0.7)
	#par(new=T)
}
text(x=0.8,y=w$quantdes[kdes+1,1],labels=paste(w$seuildes[kdes+1]), cex=0.7)
text(x=tprev[kprev]+0.2,y=w$quantdes[kdes+1,kprev],labels=paste(w$seuildes[kdes+1]), cex=0.7)
points(tprev,w$prevision, ylim=ylim, cex = 1);lines(tprev,w$prevision, ylim=ylim, cex = 1)
box("figure", col = "pink", lwd=3);
}

