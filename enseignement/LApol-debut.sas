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
estimate p=2 input=(temp1 part1);
estimate p=2 input=((1)/(1 2)temp1 (1)part1);
forecast lead=0 out=mod2;/*permet de récupérer la sortie du modèle dont les résidus*/
run;
quit;
data mod3;
merge mod2 pol;
run;
/*On analyse les crosscor*/
proc arima data=mod3;
identify var=residual crosscor=(temp1 part1);
run;
