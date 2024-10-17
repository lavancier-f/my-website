library(lars)
library(leaps)
library(glmnet)


Simudata=function(n,p){
  if(p<4){stop("p>3 requis")}
  X=matrix(rnorm(n*p),n,p)
  bet=matrix(0,p)
  bet[1:3]=1
  y=X%*%bet+rnorm(n)
  return(list(X=X,y=y,bet=bet))
}



fun=function(n,p,M=100){
  selec_method1=NULL;selec_method2=NULL;selec_method3=NULL;
  taille_method1=NULL;taille_method2=NULL;taille_method3=NULL;
  prev_method1=NULL; prev_method2=NULL; prev_method3=NULL; prev_method4=NULL;
  temps1=NULL;temps2=NULL;temps3=NULL;temps4=NULL;
  for(i in 1:M){
    cat(i)
    
    datatrain=Simudata(n,p)
    Xtrain=datatrain$X
    y=datatrain$y
    bet=datatrain$bet
    
    datatest=Simudata(n,p)
    Xtest=datatest$X
    ytest=datatest$y
    
    #Méthode 1
    tic=proc.time()
    tab=data.frame(y=y,X=Xtrain)
    fit0=lm(y~1,tab)
    fit=lm(y~.,tab)
    tmp=step(fit0,scope=formula(fit),k=log(n),direction="both",trace=0)
    noms=sort(names(tmp$model))
    selec_method1[i]=identical(noms[-length(noms)],sort(paste("X.",which(bet!=0),sep="")))
    taille_method1[i]=length(noms)-1
    prev_method1[i]=mean((predict(tmp,data.frame(X=Xtest))-ytest)^2)
    tac=proc.time()-tic
    temps1[i]=tac[3]
    
    #Méthode 2
    tic=proc.time()
    cvglm=cv.glmnet(Xtrain,y)
    lambda=cvglm$lambda.min
    coef2=coef(cvglm,s=lambda)[-1]
    index=which(coef2!=0) 
    selec_method2[i]=identical(sort(index),which(bet!=0))
    taille_method2[i]=length(index)
    prev_method2[i]=mean((predict(cvglm,Xtest,s=lambda)-ytest)^2)
    tac=proc.time()-tic
    temps2[i]=tac[3]
    
    #Méthodes 3 et 4
    if(length(index)==0){
      selec_method3[i]=selec_method2[i];taille_method3[i]=taille_method2[i]; prev_method3[i]=prev_method2[i]; prev_method4[i]=prev_method2[i]}
    else{
      cvglm=cv.glmnet(Xtrain,y,penalty.factor=1/abs(coef2))
      lambda=cvglm$lambda.min
      coef3=coef(cvglm,s=lambda)[-1]
      index=which(coef3!=0) 
      selec_method3[i]=identical(sort(index),which(bet!=0))
      taille_method3[i]=length(index)
      prev_method3[i]=mean((predict(cvglm,Xtest,s=lambda)-ytest)^2)
      tac=proc.time()-tic
      temps3[i]=tac[3]
      #Méthode 4   
      if(length(index)==0){prev_method4[i]=mean((mean(y)-ytest)^2)}
      else{
        tab=data.frame(y=y,X=Xtrain)
        reg=lm(y~.,data=tab[,c(1,index+1)])
        prev_method4[i]=mean((predict(reg,data.frame(X=Xtest))-ytest)^2)
        tac=proc.time()-tic
        temps4[i]=tac[3]
      }
    }
    
  }
  
  res=list(mean(selec_method1),mean(selec_method2),mean(selec_method3),taille_method1,taille_method2,taille_method3,prev_method1,prev_method2,prev_method3,prev_method4,mean(temps1),mean(temps2),mean(temps3),mean(temps4))
  names(res)=c("selec_method1","selec_method2","selec_method3","taille_method1","taille_method2","taille_method3","prev_method1","prev_method2","prev_method3","prev_method4","temps1","temps2","temps3","temps4")
  return(res)
  
}



###### Exemple
a=fun(50,5,100)

a$selec_method1
a$selec_method2
a$selec_method3

a$taille_method1
a$taille_method2
a$taille_method3

boxplot(a$prev_method1,a$prev_method2,a$prev_method3,a$prev_method4,names=c("Method1","Method2","Method3","Method4"),main="Title")

mean(a$prev_method1)
mean(a$prev_method2)
mean(a$prev_method3)
mean(a$prev_method4)

a$temps1
a$temps2
a$temps3
a$temps4






