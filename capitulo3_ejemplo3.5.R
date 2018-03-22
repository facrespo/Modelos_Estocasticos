library(lattice);
library(xtable);

K=24; #Etapas que se van a calcular.
r=11; #dimension de la matriz P a generar segun definicion ejemplo 3.5

P=matrix(0,r,r);
f=matrix(0,r,K+1);
f[1,1]=1;

for (i in 1:r){
  if (i==1){
    P[i,i+1]=1;
  }
  else if (i==r){
    P[i,i]=1;
  }
  else{
    for (j in 1:(i-1)){
       P[i,j]=(1/(i));
       }
    a=sum(P[i,1:(i-1)]);
    if (i<r){
       P[i,i+1]=1-(a);
       }
   }
}

for (k in 2:K){
   if(k==2){
     expr=parse(text=paste("F",as.character(k-1),"=P",sep=""));
     eval(expr);
     expr=parse(text=paste("P",as.character(k-1),"=P",sep=""));
     eval(expr);
     expr=parse(text=paste("f[,",as.character(k),"]=t(P)%*%f[,",as.character(k-1),"]",sep=""));
     eval(expr);
     print(k);
     #print((P1),include.rownames = FALSE, include.colnames = FALSE);
     #expr=parse(text=paste("print((f",as.character(k),"),include.rownames = FALSE, include.colnames = FALSE)",sep=""));
     #eval(expr);
     #print((F1),include.rownames = FALSE, include.colnames = FALSE);
   }
   else{
     Q=matrix(0,r,r);
     F=matrix(0,r,r);
     for(i in 1:r){
       Q=P;
       Q[,i]=0;
       expr=parse(text=paste("F[,i]=Q%*%as.matrix(F",as.character(k-2),"[,i])",sep=""));
       eval(expr);
     }
     print(k);
     expr=parse(text=paste("F",as.character(k-1),"=F",sep=""));
     eval(expr);
     expr=parse(text=paste("P",as.character(k-1),"=P%*%P",as.character(k-2),sep=""));
     eval(expr);
     expr=parse(text=paste("f[,",as.character(k),"]=t(P)%*%f[,",as.character(k-1),"]",sep=""));
     eval(expr);
     #expr=parse(text=paste("print((P",as.character(k),"),include.rownames = FALSE, include.colnames = FALSE)",sep=""));
     #eval(expr);
     #expr=parse(text=paste("print((f",as.character(k),"),include.rownames = FALSE, include.colnames = FALSE)",sep=""));
     #eval(expr);
     #expr=parse(text=paste("print((F",as.character(k),"),include.rownames = FALSE, include.colnames = FALSE)",sep=""));
     #eval(expr);
   }
  }
}

xtmp=(xtable(f[,17:24]));
digits(xtmp)=c(7);
print(xtmp, include.colnames = FALSE);

vtmp=matrix(0,K-1,1);

for (i in 1:(K-1)){
  expr=parse(text=paste("vtmp[",as.character(i),"]=F",as.character(i),"[1,11]",sep=""));
  eval(expr);
}

vtmp2=matrix(0,(K/2),4);

for (i in 1:(K/2)){
  vtmp2[i,1]=i;
  vtmp2[i,2]=vtmp[i];
  if((12+i)==24){
    vtmp2[i,3]=12+i;
    vtmp2[i,4]=0;  
  }
  else {
    vtmp2[i,3]=12+i;
    vtmp2[i,4]=vtmp[12+i];  
  }
}

xtmp=(xtable(vtmp2));
digits(xtmp) <- c(0,0,9,0,9);
print(xtmp, include.colnames = FALSE, include.rownames = FALSE);

plot(x=(10:23),vtmp[10:23],type = "l",xlab="etapa", ylab="Probabilidad",cex.lab=1.5,cex.main=2.0);

iterate.P <- function(x, P, n) {
  res <- matrix(NA, n+1, length(x))
  res[1,] <- x
  for (i in seq_len(n))
    res[i+1,] <- x <- x %*% P
  res
}


y1 <- iterate.P(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), P, K);
y2 <- iterate.P(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), P, K);



matplot(0:K, y1, type="l", lty=1, xlab="Step", ylab="y", las=1);
matlines(0:K, y2, lty=2);
