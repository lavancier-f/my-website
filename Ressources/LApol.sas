/*********************************************************/
/* Exercice sur la mortalité en fonction de la pollution */
/*              ARIMA avec régresseurs dynamiques        */
/*********************************************************/


/*On suppose que la table initiale s'appelle Lapol et qu'elle est dans la librarie "a"*/
/*On ajoute le numéro des observations (id)*/
data pol;
set a.Lapol;
id=_N_;
run;
/*On trace les trois séries : le lien est évident*/
proc gplot data=pol;
  plot mort*id temp*id part*id;
  symbol1 interpol=join;
run;
quit;
/*On décale temp et part car on souhaite expliquer mort par le passé de temp et mort (démarche prévisionnelle)*/
data pol;
set pol;
temp1=lag1(temp);
part1=lag1(part);
run;
/*On analyse les corrélations croisés*/
proc arima data=pol;
identify var=mort crosscor=(temp1 part1);
run;

/*On crée les nouvelles variables qui seront utilisées comme régresseurs*/
data pol;
set pol;
part7=lag7(part);
run;


/*Les modèles testés un à un (pour voir la démarche)*/
/*Pour chaque modèle testé (=une ligne estimate), ajouter les dernières lignes "forecast ... run quit"*/
/*Cela permet de récupérer les résidus du modèle testé et d'analyser les crosscorr (voir la démarche plus bas)*/
proc arima data=pol;
identify var=mort crosscor=(id temp1 part1 part7);

/*D'abord un modèle arima seul, avec tendance déterministe (cela semble meilleur de l'inclure)*/
estimate p=2 input=id; /* résidus blancs, reste à analyser les crosscor (cf data mod3 plus bas), SBC=3226*/

/*On inclut maintenant les régresseurs, d'abord sans dynamique :*/
estimate p=2 input=(id temp1 part1 part7); /*BB ok, SBC=3165 (part1 moins sign que les autres)*/
estimate p=2 input=(id temp1 part7);/*idem sans part1, tout est ok, SBC=3165 : donc part1 pas forcément indispensable*/
/*on analyse les crosscor des résidus et des régresseurs dans les deux cas précédents :*/
/* petite decroissance pour part1 et part7 (ma1?) et MA1 ou MA2 (à voir) pour temp1*/

/*On inclut une dynamique dans les régresseurs pour enlever les crosscor du bruit*/
estimate p=2 input=(id /(1)temp1 /(1)part1 /(1)part7);/*MA1 pour part7 non-sign, BB ok, SBC=3135, crosscor ok*/
estimate p=2 input=(id /(1)temp1 /(1)part1 part7);/*BB ok, SBC=3136, crosscor ok, part7 significatif?*/
estimate p=2 input=(id /(1)temp1 /(1)part1);/*BB ok, SBC=3163 (moins bon), crosscor ok*/

/*A titre de comparaison : modélisation proposée dans le livre dont l'exo est extrait*/
estimate p=2 input=(id temp1 part part7);/*moins bien que les précédents*/

forecast lead=0 out=mod2;/*permet de récupérer la sortie du modèle dont les résidus*/
run;
quit;

/*ANALYSE DES RESIDUS (à faire pour chaque modèle testé)*/
/*On crée (pour chaque modèle) le tableau des résidus et des autres variables pour analyser les crosscor*/
data mod3;
merge mod2 pol;
run;
/*On analyse les crosscor*/
proc arima data=mod3;
identify var=residual crosscor=(id temp1 part1 part7);
run;



/*Finalement, on peut faire un test du rapport des log-vraisemblances pour choisir entre différents modèles emboités*/
/*Rappel : En notant L la log-vraisemblance du modèle général et Lc la log-v du modèle contraint (k contraintes),*/
/*         2(L-Lc) suit une chi2 à k degré de liberté sous l'hypothèse nulle (=les modèles se valent)*/
proc arima data=pol;
identify var=mort crosscor=(id temp1 part1 part7);
estimate p=2 input=(id /(1)temp1 /(1)part1 /(1)part7) method=ML grid gridval=0;/*GRID permet de voir la log-vrais pour ce modèle (astuce), SBC=3135*/
estimate p=2 input=(id /(1)temp1 /(1)part1 part7) method=ML grid gridval=0;/*SBC=3136*/
estimate p=2 input=(id /(1)temp1 /(1)part1) method=ML grid gridval=0;/*SBC=3162*/
run;
quit;
/*On trouve : L=-1536.32 (pour le premier modèle) Lc=-1540.08 (pour le second, avec moins de paramètre)*/
/*Les deux modèles sont=ils équivalent? 2(L-Lc)=3.76 et le quantile à 5% d'une chi2 à 1 df vaut 3.8,*/
/*  donc les deux modèles sont équivalents : on garde le plus simple, le second ce qui confirme le SBC*/

/*On peut tester le second modèle par rapport au troisième (pour lequel Lcc=-1556.51)
/* on obtient 2(Lc-Lcc)=20.19 et donc les deux modèles ne sont pas équivalents, on garde le second.

/* Finalement, on peut retenir le second modèle : */
/*  -un AR2 pour mort expliqué avec une tendance linéaire, temp1 avec une dynamique MA1, part1 avec une dynamique MA1 et part7 sans dynamique*/
