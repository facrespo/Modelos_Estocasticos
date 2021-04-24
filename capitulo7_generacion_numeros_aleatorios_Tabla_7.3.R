library(lattice);
library(xtable);

N=8; #Cantidad de números aleatorios.
Z0=7182; #Semilla.

M=matrix(0,N,4);

M[1,1]=0;
M[1,2]=Z0;
M[1,3]=Z0^2;
aux=M[1,3];
aux=aux/100;
aux=aux%%10000;
aux=floor(aux);
Z0=aux;


for (i in 2:N){
  M[i,1]=i-1;
  M[i,2]=Z0;
  M[i,3]=Z0^2;
  aux=M[i,3];
  aux=aux/100;
  aux=aux%%10000;
  aux=floor(aux);
  U0=Z0/10000;
  M[i,4]=U0;
  Z0=aux;
}

colnames(M) <- c("i","Z_i","Z^2_i","U_i");

xtmp=(xtable(M));
digits(xtmp)=c(4);
print(xtmp, include.colnames = TRUE, include.rownames = FALSE);

