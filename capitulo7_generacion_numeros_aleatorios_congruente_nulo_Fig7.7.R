library(lattice);
library(xtable);

N=2000; #Cantidad de números aleatorios.
Z0=7182; #Semilla.
b=32;
m=(2^(b-1))-1;
a=16807;
c=0;

M=matrix(0,N,3);

M[1,1]=0;
M[1,2]=Z0;

for (i in 2:N){
  M[i,1]=i-1;
  Z0=(a*Z0+c)%%m;
  M[i,2]=Z0;
  M[i,3]=Z0/m;
}

colnames(M) <- c("i","Z_i","U_i");

hist(M[,3],main="", xlab="Ui", ylab="Frecuencia");

xtmp=(xtable(M));
digits(xtmp)=c(4);
print(xtmp, include.colnames = TRUE, include.rownames = FALSE);

