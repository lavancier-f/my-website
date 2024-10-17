/*********************************************************/
/* Exercice sur la mortalit� en fonction de la pollution */
/*              ARIMA avec r�gresseurs dynamiques        */
/*********************************************************/


/*On suppose que la table initiale s'appelle Lapol et qu'elle est dans la librarie "a"*/
/*On ajoute le num�ro des observations (id)*/
data pol;
set a.Lapol;
id=_N_;
run;
/*On trace les trois s�ries : le lien est �vident*/
proc gplot data=pol;
  plot mort*id temp*id part*id;
  symbol1 interpol=join;
run;
quit;
/*On d�cale temp et part car on souhaite expliquer mort par le pass� de temp et mort (d�marche pr�visionnelle)*/
data pol;
set pol;
temp1=lag1(temp);
part1=lag1(part);
run;
/*On analyse les corr�lations crois�s*/
proc arima data=pol;
identify var=mort crosscor=(temp1 part1);
run;

/*On cr�e les nouvelles variables qui seront utilis�es comme r�gresseurs*/
data pol;
set pol;
part7=lag7(part);
run;


/*Les mod�les test�s un � un (pour voir la d�marche)*/
/*Pour chaque mod�le test� (=une ligne estimate), ajouter les derni�res lignes "forecast ... run quit"*/
/*Cela permet de r�cup�rer les r�sidus du mod�le test� et d'analyser les crosscorr (voir la d�marche plus bas)*/
proc arima data=pol;
identify var=mort crosscor=(id temp1 part1 part7);

/*D'abord un mod�le arima seul, avec tendance d�terministe (cela semble meilleur de l'inclure)*/
estimate p=2 input=id; /* r�sidus blancs, reste � analyser les crosscor (cf data mod3 plus bas), SBC=3226*/

/*On inclut maintenant les r�gresseurs, d'abord sans dynamique :*/
estimate p=2 input=(id temp1 part1 part7); /*BB ok, SBC=3165 (part1 moins sign que les autres)*/
estimate p=2 input=(id temp1 part7);/*idem sans part1, tout est ok, SBC=3165 : donc part1 pas forc�ment indispensable*/
/*on analyse les crosscor des r�sidus et des r�gresseurs dans les deux cas pr�c�dents :*/
/* petite decroissance pour part1 et part7 (ma1?) et MA1 ou MA2 (� voir) pour temp1*/

/*On inclut une dynamique dans les r�gresseurs pour enlever les crosscor du bruit*/
estimate p=2 input=(id /(1)temp1 /(1)part1 /(1)part7);/*MA1 pour part7 non-sign, BB ok, SBC=3135, crosscor ok*/
estimate p=2 input=(id /(1)temp1 /(1)part1 part7);/*BB ok, SBC=3136, crosscor ok, part7 significatif?*/
estimate p=2 input=(id /(1)temp1 /(1)part1);/*BB ok, SBC=3163 (moins bon), crosscor ok*/

/*A titre de comparaison : mod�lisation propos�e dans le livre dont l'exo est extrait*/
estimate p=2 input=(id temp1 part part7);/*moins bien que les pr�c�dents*/

forecast lead=0 out=mod2;/*permet de r�cup�rer la sortie du mod�le dont les r�sidus*/
run;
quit;

/*ANALYSE DES RESIDUS (� faire pour chaque mod�le test�)*/
/*On cr�e (pour chaque mod�le) le tableau des r�sidus et des autres variables pour analyser les crosscor*/
data mod3;
merge mod2 pol;
run;
/*On analyse les crosscor*/
proc arima data=mod3;
identify var=residual crosscor=(id temp1 part1 part7);
run;



/*Finalement, on peut faire un test du rapport des log-vraisemblances pour choisir entre diff�rents mod�les emboit�s*/
/*Rappel : En notant L la log-vraisemblance du mod�le g�n�ral et Lc la log-v du mod�le contraint (k contraintes),*/
/*         2(L-Lc) suit une chi2 � k degr� de libert� sous l'hypoth�se nulle (=les mod�les se valent)*/
proc arima data=pol;
identify var=mort crosscor=(id temp1 part1 part7);
estimate p=2 input=(id /(1)temp1 /(1)part1 /(1)part7) method=ML grid gridval=0;/*GRID permet de voir la log-vrais pour ce mod�le (astuce), SBC=3135*/
estimate p=2 input=(id /(1)temp1 /(1)part1 part7) method=ML grid gridval=0;/*SBC=3136*/
estimate p=2 input=(id /(1)temp1 /(1)part1) method=ML grid gridval=0;/*SBC=3162*/
run;
quit;
/*On trouve : L=-1536.32 (pour le premier mod�le) Lc=-1540.08 (pour le second, avec moins de param�tre)*/
/*Les deux mod�les sont=ils �quivalent? 2(L-Lc)=3.76 et le quantile � 5% d'une chi2 � 1 df vaut 3.8,*/
/*  donc les deux mod�les sont �quivalents : on garde le plus simple, le second ce qui confirme le SBC*/

/*On peut tester le second mod�le par rapport au troisi�me (pour lequel Lcc=-1556.51)
/* on obtient 2(Lc-Lcc)=20.19 et donc les deux mod�les ne sont pas �quivalents, on garde le second.

/* Finalement, on peut retenir le second mod�le : */
/*  -un AR2 pour mort expliqu� avec une tendance lin�aire, temp1 avec une dynamique MA1, part1 avec une dynamique MA1 et part7 sans dynamique*/
