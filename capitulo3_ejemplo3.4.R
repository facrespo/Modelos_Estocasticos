library(lattice);
library(xtable);

K=20 #Etapas que se van a calcular.

str_eval=function(x) {return(eval(parse(text=x)))};

P =t(matrix(c(0.33,0.67,0,0,0,0.5,0.5,0,0,0.25,0,0.75,0,0,0.5,0.5), nrow=4, ncol=4));

r=nrow(P);

for (k in 1:K){
   if(k==1){
     expr=parse(text=paste("F",as.character(k),"=P",sep=""));
     eval(expr);
     print(k);
     print((F1),include.rownames = FALSE, include.colnames = FALSE);
   }
   else{
     Q=matrix(0,r,r);
     F=matrix(0,r,r);
     for(i in 1:r){
       Q=P;
       Q[,i]=0;
       expr=parse(text=paste("F[,i]=Q%*%as.matrix(F",as.character(k-1),"[,i])",sep=""));
       eval(expr);
     }
     print(k);
     expr=parse(text=paste("F",as.character(k),"=F",sep=""));
     eval(expr);
     expr=parse(text=paste("print((F",as.character(k),"),include.rownames = FALSE, include.colnames = FALSE)",sep=""));
     eval(expr);
   }
  }

