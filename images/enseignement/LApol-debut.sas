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
estimate p=2 input=(temp1 part1);
estimate p=2 input=((1)/(1 2)temp1 (1)part1);
forecast lead=0 out=mod2;/*permet de r�cup�rer la sortie du mod�le dont les r�sidus*/
run;
quit;
data mod3;
merge mod2 pol;
run;
/*On analyse les crosscor*/
proc arima data=mod3;
identify var=residual crosscor=(temp1 part1);
run;
